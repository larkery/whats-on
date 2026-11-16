# whats-on

A personal reader for Bristol event listings. It fetches venue and listings
sites into one SQLite database and prints a plain list, so you don't have to
wade through the websites themselves.

## Use

    bb scrape                 # fetch ~6 months ahead into whats-on.db (10-15 minutes)
    bb scrape --only beacon,cube
    bb upcoming               # plain-text list of the next 7 days
    bb upcoming --days 30 --q jazz
    bb upcoming --category comedy
    bb html --days 30         # writes whats-on.html: plain, large text, no images
    bb sources

Meant to run about once a week (cron / systemd timer).

## Sources

| source            | how                                                      |
|-------------------|----------------------------------------------------------|
| headfirst         | JSON API (covers lots of small venues)                    |
| watershed         | listing + each film's page for screening times            |
| tobacco-factory   | listing (date ranges, no times)                           |
| old-vic           | paged listing + JSON-LD on each production page           |
| hippodrome        | ATG listing (date ranges) + page description             |
| alma              | the theatre's TicketTailor box office page               |
| wardrobe          | WordPress REST API (`/wp-json/wp/v2/shows`)               |
| st-georges        | paged listing + JSON-LD on each event page                |
| trinity           | listing                                                   |
| beacon            | date-filtered paged listing + event pages for prices      |
| cube              | programme page                                            |
| redgrave          | listing + show pages (time, prices, performance list)     |
| arnolfini         | listing + event pages                                     |
| bristol-museums   | M Shed, Museum & Art Gallery etc: paged listing + pages   |
| arnos-vale        | events blog listing (dates in prose)                      |
| we-the-curious    | evening/special events only; dates from event pages       |
| bristol-cathedral | The Events Calendar JSON API, services filtered out       |
| hen-and-chicken   | The Events Calendar JSON API, comedy filtered out         |
| st-mary-redcliffe | Eventbrite collections (currently stale; see source)      |
| storysmith        | WooCommerce category page + Store API for descriptions    |
| lost-in-the-reads | TicketTailor                                               |
| megascreen        | Bristol Aquarium "now showing" page                       |

Shared parsers: `tribe.clj` (The Events Calendar, a common WordPress plugin),
`tickettailor.clj`, `eventbrite.clj`. Any other venue on one of those
platforms is a few lines (see `alma.clj` or `bristol_cathedral.clj`).

Each source lives in `src/whats_on/sources/<name>.clj`, returns plain maps, and is
registered in `whats-on.main/registry`. To add a venue, copy the closest one.
Anything spanning more than 180 days (permanent displays, all-year tours) is
dropped.

Bookhaus, The BookCellar, East Bristol Books, Gloucester Road Books and the
Old Market Assembly's gigs all list through Headfirst, so they need no scraper.
Clifton Cathedral has no machine-readable listings.

## Data

`events` holds one row per performance, or per run where a site only gives a
date range (`end_date` set). Times are UK local. `tags` is a JSON array.

The same event often appears on Headfirst and the venue's own site. After each
scrape, listings at the same venue, on a compatible date/time and with similar
titles are linked by `canonical_id`, which prefers the venue's own listing. The
`upcoming` view shows one row per real event, fills in a missing price or
description from its duplicates, and lists the other sources in `also_listed`.

Upcoming rows a source no longer lists are deleted on its next successful
scrape. Past rows are kept. The `runs` table records each source's success or failure.

## The app

    bb app                    # or: clojure -M:app [port]
    open http://localhost:8787

A small local web app (JVM Clojure: http-kit, htmx, SQLite) on the same
database. It only listens on 127.0.0.1.

- **Plan ahead**: the top two matches per month from three weeks out, limited
  to ticketed one-offs and short runs, which are the things worth booking
  early. Anything you've marked Booked is listed at the top.
- **Find something**: a three-month calendar, shaded by how much matches your
  other filters each day (click a day; shift-click another for a range), plus
  time of day, kinds, max price, venue and optional free text ("something
  about history"). The best N come back as a table; each row's description
  and match score fold out under "details".
- **Quick rate**: a random spread of upcoming events to 👍/👎, to get started.
- **My taste**: sentences about what you like or don't, past events you've
  been to (paste `Title | Venue | Date | notes` lines), and all your ratings.

Every card has 👍 Interested · 👎 Not for me · ✓ Booked · Hide · More like this.
A rating applies to every date of that event (e.g. all screenings of a film).

### How suggestions work

Each event's title, tags, venue and description are embedded on-device with
[bge-small-en-v1.5](https://huggingface.co/BAAI/bge-small-en-v1.5) through
ONNX Runtime (no Python, no network after the first ~130MB model download into
`~/.cache/whats-on/models`). Embeddings are cached in the `embeddings` table,
and new events are picked up every 10 minutes while the app runs.

Your taste is a set of examples: liked (+1), booked (+1.5), past events
(+1.5), "I like…" sentences (+1), disliked and "not for me" (−1). An event
scores by the average similarity of its 3 closest positive examples, with a
penalty if it is very close to something negative. Using the nearest
examples, not an average of everything, keeps different tastes separate.
Each card says which example it was matched on. Free-text searches blend the
query match (65%) with your taste (35%). All the knobs are in
`src/whats_on/app/rank.clj`.

Not done yet: non-semantic scoring (price, day of week, venue habits).

## Politeness

- Requests to each site go one at a time, at least 4s apart (±20% jitter),
  or the site's `Crawl-delay` if that is longer. Different sites are fetched
  in parallel.
- Responses are cached in `~/.cache/whats-on/http`. Listing pages are reused
  for 12h and detail pages for 6 days; after that a conditional request
  (ETag / If-Modified-Since) is made where the server supports it.
- robots.txt is honoured. The one exception is Headfirst, whose robots.txt
  disallows `/api/v2`. That endpoint is the only way to read their listings,
  and it's what their own page loads in your browser. This tool reads it on
  your behalf at roughly the volume of one person browsing the site (about 20
  requests a week). See the note in `sources/headfirst.clj`.
- An honest User-Agent: `whats-on/0.1 (personal events reader; babashka)`. Set
  `:contact` in `config.edn` to add a way for site owners to reach you.
- 429 / 5xx responses back off (respecting `Retry-After`) and retry at most twice.

## Config

Optional `config.edn`:

    {:db "whats-on.db"
     :days 183
     :delay-ms 4000
     :contact "mailto:you@example.org"
     :disabled #{:hippodrome}}
