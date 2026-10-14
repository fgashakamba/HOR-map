# HOR Interactive Map

An R Shiny app for mapping district and sector data in Rwanda.

## CARTO basemaps

Copy `.Renviron.example` to `.Renviron` in the application directory and set
`CARTO_BASEMAP_API_KEY` to your CARTO basemap key. The app reads this file when
it starts. You can also supply the variable through the R process environment.
Keep `.Renviron` out of Git and restrict production access to the app owner and
the `shiny` group.

Positron and Dark Matter use authenticated tile URLs for initial rendering and
basemap changes. Other basemaps use their existing Leaflet providers. CARTO and
OpenStreetMap attribution stays visible. The browser receives the basemap key
in tile requests, as required by CARTO's client integration.

Reference: [CARTO basemap instructions](https://github.com/CartoDB/basemap-styles).

## Production deployment

App URL: <https://geodashlabs.com/hormap/>

Deploy `app.R` and `R/basemaps.R` to `/srv/shiny-server/hormap/`, alongside the
existing `data_wgs84/` directory and the private `.Renviron`. Run
`touch /srv/shiny-server/hormap/restart.txt` after deployment so Shiny Server
loads the updated app for new sessions.

`deploy/nginx-hormap.conf` records the hormap routes in
`/etc/nginx/sites-available/geodashlabs.com`. The app uses the existing
`oauth2-proxy-potato.service` and Keycloak client. Access follows the shared
policy for verified `@oneacrefund.org` staff. The shared Shiny proxy snippet
provides WebSocket forwarding. Validate Nginx with `sudo nginx -t` before
applying a route change with `sudo systemctl reload nginx`.

Run the basemap checks from the repository root with:

```sh
Rscript tests/test-basemaps.R
Rscript tests/test-shiny-basemaps.R
```
