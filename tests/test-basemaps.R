source(file.path("R", "basemaps.R"))

local({
  original_key <- Sys.getenv("CARTO_BASEMAP_API_KEY", unset = NA_character_)
  on.exit({
    if (is.na(original_key)) Sys.unsetenv("CARTO_BASEMAP_API_KEY")
    else Sys.setenv(CARTO_BASEMAP_API_KEY = original_key)
  })

  Sys.setenv(CARTO_BASEMAP_API_KEY = "test+key/with?symbols")
  for (provider in c("CartoDB.Positron", "CartoDB.DarkMatter")) {
    map <- add_basemap_tiles(leaflet::leaflet(), provider)
    call <- tail(map$x$calls, 1)[[1]]
    stopifnot(
      identical(call$method, "addTiles"),
      grepl("?key=test%2Bkey%2Fwith%3Fsymbols", call$args[[1]], fixed = TRUE),
      grepl(if (provider == "CartoDB.Positron") "/light_all/" else "/dark_all/",
            call$args[[1]], fixed = TRUE),
      grepl("OpenStreetMap", call$args[[4]]$attribution, fixed = TRUE),
      grepl("CARTO", call$args[[4]]$attribution, fixed = TRUE),
      identical(call$args[[4]]$maxZoom, 20)
    )
  }

  Sys.unsetenv("CARTO_BASEMAP_API_KEY")
  missing_key <- tryCatch(
    add_basemap_tiles(leaflet::leaflet(), "CartoDB.Positron"),
    error = identity
  )
  stopifnot(inherits(missing_key, "error"),
            grepl("CARTO_BASEMAP_API_KEY", conditionMessage(missing_key), fixed = TRUE))

  for (provider in c("OpenStreetMap", "Esri.WorldImagery", "Esri.WorldTopoMap",
                     "Stadia.StamenTerrain")) {
    map <- add_basemap_tiles(leaflet::leaflet(), provider)
    call <- tail(map$x$calls, 1)[[1]]
    stopifnot(identical(call$method, "addProviderTiles"),
              identical(call$args[[1]], provider))
  }
})

cat("Basemap checks passed.\n")
