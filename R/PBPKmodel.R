#' PBPK model
#'
#' This function contains the PBPK model under pinning the application.
#' Variable Prefixes
#' A = Amount
#' At = Amount in Tissue
#' Ab = Amount in Blood
#' C = Concentration
#' Ct = Concentration in Tissue
#' Cb = Concentration in Blood
#' P = Partition Coefficient
#' PC = Permeability Coefficient
#' Q = Flow
#' R = Rate
#' V = Volume
#' @param time time step
#' @param state vector of states
#' @param parameters vector of parameters
#' @noRd
PBPKmodel <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    ## Concentrations ##

    # GI Concentration [µM]
    C_GI <- (At_GI + Ab_GI) / V_GI
    # GI Tissue Concentration [µM]
    Ct_GI <- At_GI / (Vt_GI * V_GI)
    # GI Blood Concentration [µM]
    Cb_GI <- Ab_GI / ((1 - Vt_GI) * V_GI)

    # Liver Concentration [µM]
    C_Liver <- (At_Liver + Ab_Liver) / V_Liver
    # Liver Tissue Concentration [µM]
    Ct_Liver <- At_Liver / (Vt_Liver * V_Liver)
    # Liver Blood Concentration [µM]
    Cb_Liver <- Ab_Liver / ((1 - Vt_Liver) * V_Liver)

    # Kidney Concentration [µM]
    C_Kidney <- (At_Kidney + Ab_Kidney) / V_Kidney
    # Kidney Tissue Concentration [µM]
    Ct_Kidney <- At_Kidney / (Vt_Kidney * V_Kidney)
    # Kidney Perfused Tissue Blood Concentration [µM]
    Cb_Kidney <- Ab_Kidney / ((1 - Vt_Kidney) * V_Kidney)

    # Rest of Body Concentration [µM]
    C_RestOfBody <- (At_RestOfBody + Ab_RestOfBody) / V_RestOfBody
    # Rest of Body Tissue Concentration [µM]
    Ct_RestOfBody <- At_RestOfBody / (Vt_RestOfBody * V_RestOfBody)
    # Rest of Body Blood Concentration [µM]
    Cb_RestOfBody <- Ab_RestOfBody / ((1 - Vt_RestOfBody) * V_RestOfBody)

    # Blood Concentration [µM]
    C_Blood <- Ab_arterial + Ab_venous / (Vb_arterial + Vb_venous)
    # Concentration in Arterial Blood [µM]
    Cb_arterial <- Ab_arterial / Vb_arterial
    # Concentration in Venous Blood [µM]
    Cb_venous <- Ab_venous / Vb_venous

    # Oral Dose
    # Amount in Gut That Has Not Been Absorbed [µmol]
    A_oralDoseInGut <- oralDose - A_oralDoseAbsorbed
    # Amount in That Has Been Absorbed [µmol]
    dA_oralDoseAbsorbed <- A_oralDoseInGut * R_absorbed

    # GI Amount [µmol]
    A_GI <- Ab_GI + At_GI
    # Amount in GI Blood [µmol]
    dAb_GI <- Q_GI * Cb_arterial - Q_GI * Cb_GI +
      PC_GI * Ct_GI / P_GI - PC_GI * Cb_GI
    # Amount in GI Tissue [µmol]
    dAt_GI <- PC_GI * Cb_GI - PC_GI * Ct_GI / P_GI + A_oralDoseInGut * R_absorbed

    # Liver Metabolism
    # Clearance Rate [µmol/h]
    R_metabolism1 <- clearance * (Ct_Liver / P_Liver)
    # Amount Metabolized First Order [µmol]
    dA_metabolism1 <- R_metabolism1
    # Vmax and Km
    # Saturable Metabolism Rate [µmol/h]
    R_metabolism2 <- Vmax * (Ct_Liver / P_Liver) /
      ((Ct_Liver / P_Liver) + Km)
    # Amount Metabolized Saturable [µmol]
    dA_metabolism2 <- R_metabolism2

    # Liver Amount [µmol]
    A_Liver <- Ab_Liver + At_Liver
    # Amount in Liver Blood [µmol]
    dAb_Liver <- Q_Liver * Cb_arterial + Q_GI * Cb_GI -
      (Q_Liver + Q_GI) * Cb_Liver +
      PC_Liver * Ct_Liver / P_Liver - PC_Liver * Cb_Liver
    # Amount in Liver Tissue [µmol]
    dAt_Liver <- PC_Liver * Cb_Liver - PC_Liver * Ct_Liver / P_Liver -
      R_metabolism1 - R_metabolism2

    # Urinary Excretion
    # Clearance
    # Urinary 1st Order Clearance Rate [µmol/h]
    R_urineExcretion1 <- urinaryClearance * (Ct_Kidney / P_Kidney)
    # Amount Urinary Excretion First Order [µmol]
    dA_urineExcretion1 <- R_urineExcretion1
    # Vmax and Km
    # Saturable Urinary Rate [µmol/h]
    R_urineExcretion2 <- urinaryVmax * (Ct_Kidney / P_Kidney) /
      ((Ct_Kidney / P_Kidney) + urinaryKm)
    # Amount Removed Through Urine Saturable [µmol]
    dA_urineExcretion2 <- R_urineExcretion2

    # Kidney Amount [µmol]
    A_Kidney <- Ab_Kidney + At_Kidney
    # Amount in Kidney Blood [µmol]
    dAb_Kidney <- Q_Kidney * Cb_arterial - Q_Kidney * Cb_Kidney +
      PC_Kidney * Ct_Kidney / P_Kidney - PC_Kidney * Cb_Kidney
    # Amount in Kidney Tissue [µmol]
    dAt_Kidney <- PC_Kidney * Cb_Kidney - PC_Kidney * Ct_Kidney / P_Kidney -
      R_urineExcretion1 - R_urineExcretion2

    # Rest of Body Amount [µmol]
    A_RestOfBody <- Ab_RestOfBody + At_RestOfBody
    # Amount in Rest of Body Blood [µmol]
    dAb_RestOfBody <- Q_RestOfBody * Cb_arterial - Q_RestOfBody * Cb_RestOfBody +
      PC_RestOfBody * Ct_RestOfBody / P_RestOfBody - PC_RestOfBody * Cb_RestOfBody
    # Amount in Rest of Body Tissue [µmol]
    dAt_RestOfBody <- PC_RestOfBody * Cb_RestOfBody -
      PC_RestOfBody * Ct_RestOfBody / P_RestOfBody

    # Blood Amount [µmol]
    A_Blood <- Ab_arterial + Ab_venous
    # Amount in Arterial Blood [µmol]
    dAb_arterial <- Q_cardiacOutput * Cb_venous -
      (Q_Kidney * Cb_arterial + Q_Liver * Cb_arterial + Q_GI * Cb_arterial +
        Q_RestOfBody * Cb_arterial)
    # Amount in Venous Blood [µmol]
    dAb_venous <- Q_Kidney * Cb_Kidney + (Q_GI + Q_Liver) * Cb_Liver +
      Q_RestOfBody * Cb_RestOfBody - Q_cardiacOutput * Cb_venous

    ## Mass Balance
    # Amount in Body [µmol]
    A_body <- Ab_Kidney + At_Kidney + Ab_Liver + At_Liver + Ab_GI +
      At_GI + Ab_RestOfBody + At_RestOfBody + Ab_arterial + Ab_venous +
      A_oralDoseInGut

    # Mass Balance [µmol]
    massBalance <- oralDose - A_body - A_metabolism1 - A_metabolism2 -
      A_urineExcretion1 - A_urineExcretion2

    return(list(
      c(
        dA_oralDoseAbsorbed, dAb_GI, dAt_GI,
        dA_metabolism1, dA_metabolism2,
        dAb_Liver, dAt_Liver, dA_urineExcretion1,
        dA_urineExcretion2, dAb_Kidney, dAt_Kidney,
        dAb_RestOfBody, dAt_RestOfBody, dAb_arterial, dAb_venous
      ),
      "A_GI" = A_GI, "C_GI" = C_GI,
      "Ct_GI" = Ct_GI, "Cb_GI" = Cb_GI,
      "C_Liver" = C_Liver, "A_Kidney" = A_Kidney,
      "Ct_Liver" = Ct_Liver,
      "A_Liver" = A_Liver, "C_Kidney" = C_Kidney,
      "Cb_Liver" = Cb_Liver, "Ct_Kidney" = Ct_Kidney,
      "Cb_Kidney" = Cb_Kidney,
      "A_RestOfBody" = A_RestOfBody, "C_RestOfBody" = C_RestOfBody,
      "Ct_RestOfBody" = Ct_RestOfBody, "Cb_RestOfBody" = Cb_RestOfBody,
      "Cb_arterial" = Cb_arterial,
      "A_Blood" = A_Blood, "C_Blood" = C_Blood,
      "Cb_venous" = Cb_venous, "A_oralDoseInGut" = A_oralDoseInGut,
      "A_body" = A_body, "massBalance" = massBalance
    ))
  })
}
