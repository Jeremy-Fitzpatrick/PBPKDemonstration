#' PBPK model
#'
#' This function contains the PBPK model under pinning the application.
#' Variable Prefixes
#' A = Amount
#' At = Amount in Tissue
#' Ab = Amount in Blood
#' @noRd
PBPKstates <- function() {
  states <- c(
    # Oral Dose
    A_oralDoseAbsorbed = 0, # Amount of Oral Dose Absorbed by the Body [µmol]
    Ab_GI = 0, # Amount in GI Blood Fraction [µmol]
    At_GI = 0, # Amount in GI Tissue Fraction [µmol]
    A_metabolism1 = 0, # Amount Metabolized in Liver 1st Order Metabolism [µmol]
    A_metabolism2 = 0, # Amount Metabolized in Liver Saturable Metabolism [µmol]
    Ab_Liver = 0, # Amount in Liver Blood Fraction [µmol]
    At_Liver = 0, # Amount in Liver Tissue Fraction [µmol]
    A_urineExcretion1 = 0, # Amount Excreted Urinary First Order Excretion [µmol]
    A_urineExcretion2 = 0, # Amount Excreted Urinary Saturable Excretion  [µmol]
    Ab_Kidney = 0, # Amount in Kidney Blood Fraction [µmol]
    At_Kidney = 0, # Amount in Kidney Tissue Fraction [µmol]
    Ab_RestOfBody = 0, # Amount in Rest of Body Blood Fraction [µmol]
    At_RestOfBody = 0, # Amount in Rest of Body Tissue Fraction [µmol]
    Ab_arterial = 0, # Amount in Arterial Blood [µmol]
    Ab_venous = 0 # Amount in Venous Blood [µmol]
  )
  return(states)
}
