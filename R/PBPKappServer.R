#' Shiny Server Logic for PBPKapp
#'
#' This function should not be run by the user.
#' @param input,output,session Internal shiny parameters.
#' @noRd
PBPKappServer <- function(input, output, session) {
  ## Run Model and Create Plots
  observeEvent(input$runModel, {

    # Run PBPK model
    modelOutput <- runPBPKmodel(input)

    # Create Amount Plot
    output$amountPlot <- renderPlotly({
      createPlot(
        modelOutput, c(
          "A_GI" = "GI", "A_Liver" = "Liver",
          "A_RestOfBody" = "Rest of Body", "A_Blood" = "Blood"
        ),
        "Amount Plot", "\u03BCmol"
      )
    })

    # Create Concentration Plot
    output$concentrationPlot <- renderPlotly({
      createPlot(
        modelOutput, c(
          "C_GI" = "GI", "C_Kidney" = "Kidney",
          "C_RestOfBody" = "Rest of Body", "C_Blood" = "Blood"
        ),
        "Concentration Plot", "\u03BCM"
      )
    })

    # Create Mass Balance Plot
    output$massBalancePlot <- renderPlotly({
      createPlot(
        modelOutput, c("massBalance" = "Mass Balance"),
        "Mass Balance Plot", "\u03BCmol"
      )
    })
  })
}
