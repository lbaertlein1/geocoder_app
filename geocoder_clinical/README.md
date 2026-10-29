# Clinical Address Resolver

This is a separate, stateless Shiny application for use alongside a RedCap
reporting workflow. It resolves one location at a time and does not load,
store, synchronize, or download patient data.

## Run locally

From this directory, run shiny::runApp().

## Current behavior

- Search is limited to Guatemala.
- When exact study bounds are not configured, candidate results remain eligible
  within a 100 km search envelope around Coatepeque and are ranked by local
  address match, feature type, importance, distance, and a Coatepeque locality
  prior.
- If a query does not contain a recognized locality, “Coatepeque, Guatemala”
  is automatically appended before external geocoding. Explicit localities
  such as Flores Costa Cuca are preserved.
- The Coatepeque prior is a ranking preference within 15 km, not a hard
  exclusion. A good match elsewhere can still be selected when it is clearly
  better than the local candidates.
- An explicitly named locality bypasses the 100 km Coatepeque envelope, so a
  valid address in a distant city can still be returned.
- A clinician can choose a search result or click the map manually.
- Manual map clicks are reverse-geocoded.
- The map uses Google Maps JavaScript API map types: Google street, Google
  satellite, and Google hybrid.
- Results remain in the current browser session only.
- The result can be copied to the clipboard for entry into RedCap.

The geocoding service and Guatemala-specific defaults should be evaluated for
latency and acceptable use before production deployment.

## Backend testing

The address fixture in `tests/test_addresses.csv` contains Coatepeque-area
reference addresses and expected approximate coordinates. Run the offline
normalization, proximity, ranking, and round-trip tests with:

```r
system2("C:/Users/wxf7/AppData/Local/Programs/R/R-4.4.0/bin/Rscript.exe",
        "geocoder_clinical/tests/test_backend.R")
```

Live geocoder tests are opt-in because the public Nominatim service is
rate-limited. Set `RUN_LIVE_GEOCODER_TESTS=1` before running the same script.
The live tests are intentionally tolerant of several kilometers because these
are place/address reference points, not surveyed rooftop coordinates.

When Google Places is configured, set `RUN_LIVE_GOOGLE_PLACES_TESTS=1` as
well to run the recorded Galves church landmark regression. This requires
`USE_GOOGLE_PLACES=1` and `GOOGLE_PLACES_API_KEY` in the process environment
when the test script is launched.

`tests/guatemala_address_corpus.csv` is the broader regression corpus. It
covers Guatemalan numbered streets, zones, abbreviations, colonias,
lotificaciones, aldeas, caserios, parcelamientos, kilometer references, and
landmark-style descriptions such as “por la iglesia” and “proximo a”. Rows
without coordinates are query-pattern tests; rows with coordinates are
recorded geographic anchors.

The Magnolia reference point in the fixture is based on the Coatepeque
location shown in the Guatemalan SEGEPLAN planning document used during test
development.

Google Maps requires a browser-restricted API key with the Maps JavaScript API
enabled. The local environment file is ignored by Git.

For Google-like place-name search, add `GOOGLE_PLACES_API_KEY` to `.env`. If it
is not present, the Maps JavaScript key is used as the Places key; set
`USE_GOOGLE_PLACES=0` to disable that behavior. Enable Places API (New) and
billing for the key, and make sure its restrictions allow server-side REST
requests. Google Places Text Search is tried first for informal landmark
queries; the Guatemala-bounded Nominatim path remains the secondary fallback.

Search has a non-empty-result contract: if both providers fail or return no
candidate, the app returns a clearly labeled approximate point at the study
area center so the clinician can immediately refine it on the map.

Confirmed study-area landmarks can also be added to the small
`LOCAL_LANDMARKS` block in `app.R`. These are geographic reference data, not
patient records, and provide deterministic results when third-party place
search ranking is ambiguous.

## Configure the study area

Before deployment, edit the `STUDY_AREA` block near the top of `app.R`. Fill in
the site name, map center, zoom, and bounding box in west/south/east/north
order. Once filled in, address search results and manually selected points are
restricted to that box. This configuration is application setup, not patient
data, and is intentionally hardcoded rather than managed through an in-app
admin page.
