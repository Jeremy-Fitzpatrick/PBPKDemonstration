#' Create Plot
#'
#' This function should not be run by the user.
#' @param modelOutput modelOutput data.frame from PBPK model
#' @param selectedColumns A named character vector of selected columns.
#' Names will be columns selected to graph. Strings in the
#' vector will be used to replace the column names in the graph.
#' @param plotTitle title for plot
#' @param yAxisLabel label for y-axis
#' @noRd
createPlot <- function(modelOutput, selectedColumns,
                       plotTitle, yAxisLabel) {
  ## Select Columns to Plot
  modelOutput <- modelOutput[, names(modelOutput) %in% c(
    "time",
    names(selectedColumns)
  )]

  ## Organize and Rename for Plotting
  setDT(modelOutput)
  modelOutput <- melt(modelOutput, "time")
  modelOutput$variable <- str_replace_all(modelOutput$variable, selectedColumns)

  ## Create Plotly plot of PBPK model output.
  PBPKplot <- plot_ly(modelOutput,
    split = ~variable,
    x = ~time, y = ~value, type = "scatter", mode = "lines"
  ) %>%
    layout(
      xaxis = list(title = "Time in Hours"),
      yaxis = list(title = yAxisLabel),
      title = plotTitle
    )

  return(PBPKplot)
}
