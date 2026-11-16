(ns whats-on.app.embed
  "Local sentence embeddings with bge-small-en-v1.5 via ONNX Runtime.
  The BERT WordPiece tokenizer is implemented here, so the only native piece
  is ONNX Runtime itself. Vectors are 384 floats, L2-normalised (CLS pooling)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [ai.onnxruntime OnnxTensor OrtEnvironment OrtSession OrtSession$SessionOptions]
           [java.nio.file Files Paths]
           [java.text Normalizer Normalizer$Form]))

(def model-name "bge-small-en-v1.5")
(def dim 384)
(def max-tokens 256)

;; bge's recommended prefix when a short query is matched against passages
(def query-prefix "Represent this sentence for searching relevant passages: ")

(def model-dir
  (str (or (System/getenv "XDG_CACHE_HOME") (str (System/getProperty "user.home") "/.cache"))
       "/whats-on/models/" model-name))

(def ^:private hf-base "https://huggingface.co/Xenova/bge-small-en-v1.5/resolve/main/")

(defn ensure-model!
  "Download the model files on first use (~130MB)."
  []
  (doseq [[remote local] [["onnx/model.onnx" "model.onnx"] ["vocab.txt" "vocab.txt"]]
          :let [f (io/file model-dir local)]
          :when (not (.exists f))]
    (io/make-parents f)
    (println "Downloading" remote "to" (str f) "…")
    (let [tmp (io/file (str f ".part"))]
      (with-open [in (io/input-stream (str hf-base remote))]
        (io/copy in tmp))
      (.renameTo tmp f))))

;;; --- tokenizer (BERT uncased WordPiece) -----------------------------------

(defn- load-vocab []
  (with-open [r (io/reader (io/file model-dir "vocab.txt"))]
    (into {} (map-indexed (fn [i tok] [tok i])) (line-seq r))))

(defn- punctuation? [^long cp]
  (or (<= 33 cp 47) (<= 58 cp 64) (<= 91 cp 96) (<= 123 cp 126)
      (let [t (Character/getType (int cp))]
        (contains? #{Character/CONNECTOR_PUNCTUATION Character/DASH_PUNCTUATION
                     Character/START_PUNCTUATION Character/END_PUNCTUATION
                     Character/INITIAL_QUOTE_PUNCTUATION Character/FINAL_QUOTE_PUNCTUATION
                     Character/OTHER_PUNCTUATION}
                   (int t)))))

(defn- cjk? [^long cp]
  (or (<= 0x4E00 cp 0x9FFF) (<= 0x3400 cp 0x4DBF) (<= 0x20000 cp 0x2A6DF)
      (<= 0x2A700 cp 0x2B73F) (<= 0x2B740 cp 0x2B81F) (<= 0x2B820 cp 0x2CEAF)
      (<= 0xF900 cp 0xFAFF) (<= 0x2F800 cp 0x2FA1F)))

(defn- basic-tokens
  "Lowercase, strip accents, split on whitespace and around punctuation / CJK."
  [^String text]
  (let [s (-> (Normalizer/normalize (str/lower-case text) Normalizer$Form/NFD)
              (str/replace #"\p{Mn}" ""))
        sb (StringBuilder.)]
    (doseq [cp (iterator-seq (.iterator (.codePoints s)))]
      (let [cp (long cp)]
        (cond
          (or (= cp 0) (= cp 0xFFFD) (and (Character/isISOControl (int cp))
                                          (not (#{9 10 13} cp))))
          nil
          (or (Character/isWhitespace (int cp)) (Character/isSpaceChar (int cp)))
          (.append sb " ")
          (or (punctuation? cp) (cjk? cp))
          (doto sb (.append " ") (.appendCodePoint (int cp)) (.append " "))
          :else (.appendCodePoint sb (int cp)))))
    (remove str/blank? (str/split (str sb) #" +"))))

(defn- wordpiece [vocab ^String word]
  (if (> (count word) 100)
    ["[UNK]"]
    (loop [start 0 acc []]
      (if (>= start (count word))
        acc
        (let [piece (loop [end (count word)]
                      (when (> end start)
                        (let [sub (cond->> (subs word start end) (pos? start) (str "##"))]
                          (if (contains? vocab sub) [sub end] (recur (dec end))))))]
          (if piece
            (recur (second piece) (conj acc (first piece)))
            ["[UNK]"]))))))

(defn token-ids [vocab text]
  (let [pieces (mapcat #(wordpiece vocab %) (basic-tokens text))
        pieces (take (- max-tokens 2) pieces)]
    (long-array (concat [(vocab "[CLS]")]
                        (map #(get vocab % (vocab "[UNK]")) pieces)
                        [(vocab "[SEP]")]))))

;;; --- model --------------------------------------------------------------

(defonce ^:private state (delay
                           (ensure-model!)
                           (let [env (OrtEnvironment/getEnvironment)
                                 opts (doto (OrtSession$SessionOptions.)
                                        (.setIntraOpNumThreads (max 1 (quot (.availableProcessors (Runtime/getRuntime)) 2))))]
                             {:env env
                              :session (.createSession env (str model-dir "/model.onnx") opts)
                              :vocab (load-vocab)})))

(defn- normalise! ^floats [^floats v]
  (let [n (Math/sqrt (areduce v i acc 0.0 (+ acc (* (aget v i) (aget v i)))))]
    (when (pos? n)
      (dotimes [i (alength v)] (aset v i (float (/ (aget v i) n)))))
    v))

(defn embed
  "Embed one text. Returns a normalised float[384]."
  ^floats [text]
  (let [{:keys [^OrtEnvironment env ^OrtSession session vocab]} @state
        ids (token-ids vocab (or text ""))
        n (alength ids)
        shape (long-array [1 n])
        mk #(OnnxTensor/createTensor env (java.nio.LongBuffer/wrap %) shape)
        inputs {"input_ids" (mk ids)
                "attention_mask" (mk (long-array n 1))
                "token_type_ids" (mk (long-array n 0))}]
    (try
      (with-open [result (.run session ^java.util.Map (select-keys inputs (.getInputNames session)))]
        ;; last_hidden_state [1][n][384]; bge uses the [CLS] vector
        (let [hidden ^"[[[F" (.getValue ^OnnxTensor (.get result 0))]
          (normalise! (aclone ^floats (aget hidden 0 0)))))
      (finally
        (run! #(.close ^OnnxTensor %) (vals inputs))))))

(defn embed-query [text] (embed (str query-prefix text)))

(defn cosine
  "Vectors are normalised, so this is just the dot product."
  ^double [^floats a ^floats b]
  (loop [i 0 acc 0.0]
    (if (< i (alength a))
      (recur (inc i) (+ acc (* (aget a i) (aget b i))))
      acc)))

(defn mean-vec ^floats [vecs]
  (let [n (count vecs)
        acc (double-array dim)]
    (doseq [^floats v vecs] (dotimes [i dim] (aset acc i (+ (aget acc i) (aget v i)))))
    (float-array (map #(/ % n) acc))))

(defn center
  "v − mean, renormalised so cosine is still a dot product."
  ^floats [^floats mu ^floats v]
  (let [out (float-array dim)]
    (dotimes [i dim] (aset out i (- (aget v i) (aget mu i))))
    (normalise! out)))

(defn ->bytes ^bytes [^floats v]
  (let [bb (java.nio.ByteBuffer/allocate (* 4 (alength v)))]
    (.order bb java.nio.ByteOrder/LITTLE_ENDIAN)
    (.put (.asFloatBuffer bb) v)
    (.array bb)))

(defn <-bytes ^floats [^bytes b]
  (let [fb (.asFloatBuffer (.order (java.nio.ByteBuffer/wrap b) java.nio.ByteOrder/LITTLE_ENDIAN))
        v (float-array (.remaining fb))]
    (.get fb v)
    v))
