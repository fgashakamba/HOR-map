app <- source("app.R")$value
stopifnot(inherits(app, "shiny.appobj"), nrow(country) > 0, nrow(sectors) > 0)

shiny::testServer(server, {
  messages <- list()
  session$sendCustomMessage <- function(type, message) {
    if (identical(type, "leaflet-calls")) {
      messages[[length(messages) + 1L]] <<- message
    }
  }

  session$setInputs(basemap = "CartoDB.Positron", data_type = "nominal",
                    input_mode = "manual",
                    district_input = sort(unique(sectors$district))[1])
  initial_map <- jsonlite::fromJSON(output$map, simplifyVector = FALSE)
  initial_tiles <- Filter(function(x) identical(x$method, "addTiles"),
                          initial_map$x$calls)
  stopifnot(length(initial_tiles) == 1L,
            grepl("/light_all/", initial_tiles[[1]]$args[[1]], fixed = TRUE),
            grepl("?key=", initial_tiles[[1]]$args[[1]], fixed = TRUE))

  for (provider in c("CartoDB.DarkMatter", "OpenStreetMap", "CartoDB.Positron")) {
    messages <- list()
    session$setInputs(basemap = provider)
    session$flushReact()
    calls <- unlist(lapply(messages, function(message) message$calls), recursive = FALSE)
    stopifnot(any(vapply(calls, function(x) identical(x$method, "clearTiles"), logical(1))))
    if (startsWith(provider, "CartoDB.")) {
      tiles <- Filter(function(x) identical(x$method, "addTiles"), calls)
      style <- if (provider == "CartoDB.Positron") "/light_all/" else "/dark_all/"
      stopifnot(length(tiles) == 1L,
                grepl(style, tiles[[1]]$args[[1]], fixed = TRUE),
                grepl("?key=", tiles[[1]]$args[[1]], fixed = TRUE))
    } else {
      tiles <- Filter(function(x) identical(x$method, "addProviderTiles"), calls)
      stopifnot(length(tiles) == 1L, identical(tiles[[1]]$args[[1]], provider))
    }
  }
})

cat("Shiny initial rendering and basemap switching checks passed.\n")
