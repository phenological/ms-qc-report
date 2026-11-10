#' Bile acids reported for the BA assay
#'
#' ms.parser names bile acids "<abbreviation>-<full name>", e.g.
#' "CDCA-Chenodeoxycholic acid". The abbreviation is matched against this list.
#' @noRd
BA_names <- c(
  "CDCA", "GCDCA", "TCDCA", "UDCA", "GUDCA",
  "TUDCA", "DCA", "GDCA", "LCA", "GLCA",
  "TLCA", "CA", "GCA", "TCA", "TDCA"
)

#' QC acceptance criteria
#'
#' Used for PASS/CAUTION in summaryData(), the cell colours in summaryTables()
#' and the criteria shown in the report, so they always agree.
#' @noRd
QC_CRITERIA <- list(
  R2_MIN      = 0.99,       # calibration R2 must be >= this
  ACC_RANGE   = c(80, 120), # QC Accuracy/Recovery[%] must be inside this range
  QC_PASS_MIN = 66,         # % of QC injections inside ACC_RANGE must be >= this
  RSD_MAX     = 20          # LTR RSD (PQC RSD for SCFA) % must be <= this
)
