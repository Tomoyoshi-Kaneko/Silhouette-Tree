# ============================================================================
# shiny_app.R -- launcher for the bundled Shiny GUI (inst/shiny_app/app.R)
# ============================================================================

#' Launch the Silhouette Tree Shiny application
#'
#' Opens a browser-based interface implementing the whole workflow -- load a
#' Newick tree or a measurement table, choose the distance and normalisation,
#' pick the number of clusters by silhouette width, and download the figures
#' and results -- without writing any R code.
#'
#' @param ... Passed on to [shiny::runApp()] (e.g. `launch.browser = TRUE`, `port = 1234`).
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required to run the app. Install it with install.packages('shiny').", call. = FALSE)
  }
  app_dir <- system.file("shiny_app", package = "silhouetteTree")
  if (!nzchar(app_dir)) {
    # Development mode (package not installed): assume the working directory is
    # the package root, exactly as in the README.
    app_dir <- file.path(getwd(), "inst", "shiny_app")
    if (!dir.exists(app_dir)) {
      stop("Could not locate the Shiny app directory ('inst/shiny_app'). ",
           "Please run this with your working directory set to the silhouetteTree package root.", call. = FALSE)
    }
  }
  shiny::runApp(app_dir, ...)
}
