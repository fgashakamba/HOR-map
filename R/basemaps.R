add_basemap_tiles <- function(map, provider) {
  carto_styles <- c(
    "CartoDB.Positron" = "light_all",
    "CartoDB.DarkMatter" = "dark_all"
  )

  if (!provider %in% names(carto_styles)) {
    return(leaflet::addProviderTiles(map, provider))
  }

  api_key <- trimws(Sys.getenv("CARTO_BASEMAP_API_KEY"))
  if (!nzchar(api_key)) {
    stop("Set CARTO_BASEMAP_API_KEY in .Renviron to use CARTO basemaps.",
         call. = FALSE)
  }

  leaflet::addTiles(
    map,
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/", carto_styles[[provider]],
      "/{z}/{x}/{y}{r}.png?key=", utils::URLencode(api_key, reserved = TRUE)
    ),
    attribution = paste0(
      '&copy; <a href="https://www.openstreetmap.org/copyright">',
      'OpenStreetMap</a> contributors, &copy; ',
      '<a href="https://carto.com/attributions">CARTO</a>'
    ),
    options = leaflet::tileOptions(subdomains = "abcd", minZoom = 0, maxZoom = 20)
  )
}
