#' Run the PBPK Application
#'
#' This function runs the PBPK application. The user can alter certain properties
#' and then run the exposures with amounts reported on the graph.
#' @export
#' @examples
#' # Run the PBPK app.
#' if (interactive()) {
#'   PBPKapp()
#' }
#' @return No return value.
PBPKapp <- function() {
  shinyApp(ui = PBPKappUI, server = PBPKappServer)
}
