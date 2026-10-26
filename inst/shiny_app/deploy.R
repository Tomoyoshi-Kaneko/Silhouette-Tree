# ============================================================================
# deploy.R -- deployment notes for the Silhouette Tree Shiny app.
#
# WORKFLOW: GitHub -> Posit Connect Cloud ("Publish from GitHub").
# Connect Cloud rebuilds and redeploys the app on every push to the connected
# branch (usually within a minute or two). Normally an update is just:
#
#   1. Edit files locally (R/ and/or inst/shiny_app/).
#   2. If the set of R packages the app needs changed (a new library() call
#      anywhere in R/ or inst/shiny_app/app.R), regenerate manifest.json
#      (see below).
#   3. Push everything to GitHub (drag-and-drop upload is fine).
#   4. Wait for the automatic redeploy, or trigger it from the app's page on
#      connect.posit.cloud.
#
# FIRST-TIME SETUP on Connect Cloud (connect.posit.cloud):
#   Publish -> Shiny -> choose this GitHub repository and branch (main)
#   -> "Primary file": inst/shiny_app/app.R -> Publish.
#   (manifest.json must be in inst/shiny_app/ -- generate it once as below.)
# ============================================================================

## --- Regenerating manifest.json (needed once, and whenever dependencies change)
## Run from R with the working directory set to the package root, with every
## package the app uses installed locally:
##   install.packages(c("shiny", "DT", "ggplot2", "openxlsx", "readxl", "ape",
##                      "cluster", "svglite", "rsconnect"))
rsconnect::writeManifest(
  appDir   = "inst/shiny_app",
  appFiles = c(list.files("inst/shiny_app", full.names = FALSE),
               file.path("..", "..", "R", list.files("R")))
)
## This writes inst/shiny_app/manifest.json, listing every package (and its
## installed version) the app needs. Commit and push it together with the other
## changes. Connect Cloud reads it to know what to install. If it is missing
## or out of date the build fails with a "package not found" style error.
