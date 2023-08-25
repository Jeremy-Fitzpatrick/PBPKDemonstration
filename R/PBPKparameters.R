#' PBPK model
#'
#' This function contains the PBPK model under pinning the application.
#' Variable Prefixes
#' C = Concentration
#' P = Partition Coefficient
#' PC = Permeability Coefficient
#' Q = Flow
#' R = Rate
#' V = Volume
#' Vt = Volume fraction that is tissue
#'
#' 1kg body weight is assumed to be 1L
#' @param input shiny input
#' @noRd
PBPKparameters <- function(input) {
  Q_cardiacOutput <- input$cardiacOutput / input$bodyWeight
  parameters <- c(
    # Oral Dose
    oralDose = input$oralDose * input$bodyWeight *
      1000 / input$molecularWeight, # [µmol]

    # GI Parameters
    Vt_GI = 0.95, # Fraction that is Tissue GI
    V_GI = 0.06 * input$bodyWeight, # Total Volume GI [L]
    Q_GI = .05 * Q_cardiacOutput, # Blood Flow GI [L/h]
    PC_GI = input$PC_GI, # Permeability Coefficient GI [L/h]
    P_GI = input$P_GI, # Partition Coefficient GI
    R_absorbed = input$absorbtionRate, # Absorption Through Gut Lumen GI [/L]

    # Liver Parameters
    Vt_Liver = 0.95, # Fraction that is Tissue Liver
    V_Liver = 0.03 * input$bodyWeight, # Total Volume Liver [L]
    Q_Liver = 0.18 * Q_cardiacOutput, # Blood Flow Liver [L/h]
    PC_Liver = input$PC_Liver, # Permeability Coefficient Liver [L/h]
    P_Liver = input$P_Liver, # Partition Coefficient Liver

    # Liver Metabolism
    clearance = input$clearance, # First Order Hepatic Clearance [L/h]
    Vmax = input$Vmax, # Maximum Metabolism Rate [µmol/h]
    Km = input$Km, # Michaelis-Menten Constant for Metabolism [µM]

    # Kidney Parameters
    Vt_Kidney = 0.95, # Fraction that is Tissue Kidney
    V_Kidney = 0.04 * input$bodyWeight, # Total Volume Kidney [L]
    Q_Kidney = 0.1 * Q_cardiacOutput, # Blood Flow Kidney [L/h]
    PC_Kidney = input$PC_Kidney, # Permeability Coefficient Kidney [L/h]
    P_Kidney = input$P_Kidney, # Partition Coefficient Kidney

    # Urinary Excretion
    urinaryClearance = input$urinaryClearance, # First Order Urinary Excretion [L/h]
    urinaryVmax = input$urinaryVmax, # Maximum Rate Urinary Excretion [µmol/h]
    urinaryKm = input$urinaryKm, # Michaelis-Menten Constant for Urinary Excretion [µM]

    # Rest Of Body Parameters
    Vt_RestOfBody = 0.95, # Fraction that is Tissue Rest of Body
    V_RestOfBody = 0.71 * input$bodyWeight, # Total Volume Rest of Body [L]
    Q_RestOfBody = 0.67 * Q_cardiacOutput, # Blood Flow Rest of Body [L/h]
    PC_RestOfBody = input$PC_RestOfBody, # Permeability Coefficient Rest of Body [L/h]
    P_RestOfBody = input$P_RestOfBody, # Partition Coefficient Rest of Body

    # Blood Parameters
    Vb_arterial = 0.03 * input$bodyWeight, # Arterial Blood Volume[L]
    Vb_venous = 0.03 * input$bodyWeight, # Venous Blood Volume [L]
    Q_cardiacOutput = Q_cardiacOutput # Cardiac Output [L/h]
  )
  return(parameters)
}
