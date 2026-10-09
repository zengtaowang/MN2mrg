# R/run_app.R
# Package entry point for the MN2mrg Shiny application.

#' Launch the MN2mrg Shiny application
#'
#' Start the MN2mrg Shiny app from an installed package. The app source
#' lives at `inst/shiny/app.R` and is discovered via [system.file()], so the
#' function works identically for `devtools::load_all()` and for an installed
#' package (Posit Connect deployment reads the same directory).
#'
#' @param ... Passed through to [shiny::runApp()] (e.g. `port`, `host`,
#'   `launch.browser`).
#'
#' @return Invisibly, the value of [shiny::runApp()]. Called for its side
#'   effect of starting the Shiny process.
#'
#' @importFrom shiny runApp
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny", package = "MN2mrg")
  if (!nzchar(app_dir))
    stop("Shiny app directory not found in the installed MN2mrg package.")
  shiny::runApp(app_dir, ...)
}
