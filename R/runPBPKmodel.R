#' Run PBPK model
#'
#' Run the ODE solver from deSolve for PBPK model.
#' @param input shiny input value
#' @noRd
runPBPKmodel <- function(input) {
  parameters <- PBPKparameters(input)
  states <- PBPKstates()
  times <- seq(0, input$simulationLength, 0.1)
  modelOutput <- deSolve::ode(states, times, PBPKmodel, parameters,
    method = "lsodes"
  )
  modelOutput <- as.data.frame(modelOutput)
  return(modelOutput)
}
