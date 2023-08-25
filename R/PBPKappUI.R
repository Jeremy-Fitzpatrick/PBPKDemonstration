#' The User-Interface for the PBPKapp
#'
#' This function should not be run by the user.
#' @noRd
PBPKappUI <- function() {
  # Link to PBPK image folder
  addResourcePath(prefix = "PBPKimage", directoryPath = "inst/app/www/")
  fluidPage(
    theme = shinytheme("superhero"),
    tabsetPanel(
      tabPanel(
        "Model Overview",
        fluidRow(h1("Basic 4-compartment PBPK Model", style = "text-align: center;")),
        fluidRow(column(
          offset = 1, width = 10,
          tags$img(
            src = "PBPKimage/PBPKschematic.png",
            width = "100%", height = "100%"
          )
        ))
      ),
      tabPanel(
        "Physiological",
        fluidRow(h3("Physiological", style = "text-align: center;")),
        fluidRow(
          column(5,
            offset = 1,
            numericInputIcon("bodyWeight", "Body Weight",
              value = 0.25, icon = list("kg")
            )
          ),
          column(
            5,
            numericInputIcon("cardiacOutput", "Cardiac Output",
              value = 15, icon = list("L/h/kg BW")
            )
          )
        )
      ),
      tabPanel(
        "ADME",
        tabsetPanel(
          tabPanel(
            "Absorbtion",
            fluidRow(h3("Absorbtion", style = "text-align: center;")),
            fluidRow(column(5,
              offset = 1,
              numericInputIcon("absorbtionRate",
                "Rate of Absorption in Gut Lumen",
                value = 1, icon = list("/h")
              )
            ))
          ),
          tabPanel(
            "Distribution",
            fluidRow(h3("Distribution", style = "text-align: center;")),
            br(),
            fluidRow(column(10,
              offset = 1,
              h4("GI Parameters", style = "text-align: left;")
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("PC_GI", "Permeability Coefficient",
                  value = 1000, icon = list("L/h")
                )
              ),
              column(
                5,
                numericInput("P_GI", "Partition Coefficient",
                  value = 10
                )
              )
            ),
            br(),
            fluidRow(column(10,
              offset = 1,
              h4("Liver Parameters", style = "text-align: left;")
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("PC_Liver",
                  "Permeability Coefficient",
                  value = 1000, icon = list("L/h")
                )
              ),
              column(
                5,
                numericInput("P_Liver", "Partition Coefficient",
                  value = 10
                )
              )
            ),
            br(),
            fluidRow(column(10,
              offset = 1,
              h4("Kidney Parameters", style = "text-align: left;")
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("PC_Kidney",
                  "Permeability Coefficient",
                  value = 1000, icon = list("L/h")
                )
              ),
              column(
                5,
                numericInput("P_Kidney", "Partition Coefficient",
                  value = 10
                )
              )
            ),
            br(),
            fluidRow(column(10,
              offset = 1,
              h4("Rest of Body", style = "text-align: left;")
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("PC_RestOfBody",
                  "Permeability Coefficient",
                  value = 1000, icon = list("L/h")
                )
              ),
              column(
                5,
                numericInput("P_RestOfBody", "Partition Coefficient",
                  value = 10
                )
              )
            ),
          ),
          tabPanel(
            "Metabolism",
            fluidRow(h3("Metabolism", style = "text-align: center;")),
            fluidRow(column(10,
              offset = 1,
              h4("Liver Metabolism", style = "text-align: left;")
            )),
            fluidRow(column(5,
              offset = 1,
              numericInputIcon("clearance",
                "First Order Hepatic Clearance",
                value = 0, icon = list("L/h")
              )
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("Vmax",
                  "Maximum Metabolism Rate (Vmax)",
                  value = 0, icon = list("\u03BCmol/h")
                )
              ),
              column(
                5,
                numericInputIcon("Km",
                  "Michaelis-Menten Constant for Metabolism (Km)*",
                  value = 0.0001, icon = list("\u03BCM")
                )
              )
            )
          ),
          tabPanel(
            "Excretion",
            fluidRow(h3("Excretion", style = "text-align: center;")),
            fluidRow(column(10,
              offset = 1,
              h4("Urinary Excretion", style = "text-align: left;")
            )),
            fluidRow(column(5,
              offset = 1,
              numericInputIcon("urinaryClearance",
                "First Order Urinary Clearance",
                value = 0, icon = list("L/h")
              )
            )),
            fluidRow(
              column(5,
                offset = 1,
                numericInputIcon("urinaryVmax",
                  "Maximum Urinary Excretion Rate (Vmax)",
                  value = 0, icon = list("\u03BCmol/h")
                )
              ),
              column(
                5,
                numericInputIcon("urinaryKm",
                  "Michaelis-Menten Constant for Urinary Excretion (Km)*",
                  value = 0.0001, icon = list("\u03BCM")
                )
              )
            )
          )
        )
      ),
      tabPanel(
        "Run",
        fluidRow(h3("Run Simulation", style = "text-align: center;")),
        br(),
        fluidRow(h4("Oral Exposure", style = "text-align: center;")),
        fluidRow(
          column(5,
            offset = 1,
            numericInputIcon("oralDose", "Bolus Oral Dose",
              value = 100, icon = list("mg/kg BW")
            )
          ),
          column(
            5,
            numericInputIcon("molecularWeight", "Molecular Weight",
              value = 400, icon = list("g/mol")
            )
          )
        ),
        br(),
        fluidRow(h4("Length of Simulation", style = "text-align: center;")),
        fluidRow(
          column(5,
            offset = 1,
            numericInputIcon("simulationLength", "Simulation Length",
              value = 24, icon = list("h")
            )
          )
        ),
        fluidRow(column(4,
          offset = 4,
          actionButton("runModel", "Run Model", width = "100%")
        )),
        fluidRow(column(offset = 1, 10, plotlyOutput("unemploymentPlot")))
      ),
      tabPanel(
        "Results",
        tabsetPanel(
          tabPanel(
            "Amount Plot",
            fluidRow(column(offset = 1, 10, plotlyOutput("amountPlot")))
          ),
          tabPanel(
            "Concentration Plot",
            fluidRow(column(offset = 1, 10, plotlyOutput("concentrationPlot")))
          ),
          tabPanel(
            "Mass Balance Plot",
            fluidRow(column(offset = 1, 10, plotlyOutput("massBalancePlot")))
          )
        )
      )
    )
  )
}
