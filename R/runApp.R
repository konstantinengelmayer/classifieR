#' Run Shiny App
#'
#' This function launches the Shiny application included in the ClassifieR package.
#'
#' @return None
#' @export
runApp <- function() {
  appDir <- system.file("shiny",package = "classifieR")
  shiny::runApp(appDir, display.mode = "normal")
}
runApp()
