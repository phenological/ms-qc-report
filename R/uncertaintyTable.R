#' Working range / uncertainty table for an assay
#'
#' Returns the package's table for an assay (inst/uncertainty/<assay>.csv), so
#' you can see the current values and the exact analyte names before changing
#' some of them with `generateReport(uncertaintyEdits = )`.
#'
#' @param assay "SCFA", "AA", "BA", "TRY" or "ddhN".
#' @return Data frame with columns analyte, LLOQ, ULOQ, units, U_percent, or
#'   NULL if the package has no table for the assay.
#' @export
#' @examples
#' uncertaintyTable("BA")
uncertaintyTable <- function(assay) {
  f <- system.file("uncertainty", paste0(assay, ".csv"), package = "ms.qc.report")
  if (!nzchar(f)) {
    message("No uncertainty table in the package for assay '", assay, "'.")
    return(invisible(NULL))
  }
  utils::read.csv(f, check.names = FALSE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
}
