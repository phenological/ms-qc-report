# Helpers shared by summaryData(), summaryTables() and plotQC()

#' TRUE for internal standards (ms.parser marks them "[IS]")
#' @noRd
isInternalStandard <- function(analyteName) {
  grepl("\\[IS\\]", analyteName)
}

#' Report name of an analyte
#'
#' Names are kept as they are ("1-methylhistidine" stays "1-methylhistidine").
#' For BA, "CDCA-Chenodeoxycholic acid" becomes "CDCA" when "CDCA" is in
#' BA_names.
#' @noRd
cleanAnalyteName <- function(analyteName, method) {
  if (!identical(method, "BA")) return(analyteName)
  abbreviation <- sub("-.*$", "", analyteName)
  ifelse(abbreviation %in% BA_names, abbreviation, analyteName)
}

#' Plate order used everywhere in the report (tables and every plot)
#'
#' 1. plateOrder if given (plates not listed are added at the end);
#' 2. otherwise by each plate's earliest Acquisition Date, if every plate has one;
#' 3. otherwise by plate name (COVp2 before COVp10).
#'
#' @return list(order = plate IDs in order, note = text for the report)
#' @noRd
plateOrderInfo <- function(dae, plateOrder = NULL) {
  if (is.null(plateOrder) || length(plateOrder) == 0 || all(is.na(plateOrder) | plateOrder == "")) {
    plateOrder <- NULL
  }

  # plate IDs and Acquisition Dates across all analytes except internal standards
  rows <- lapply(dae@obsDescr, function(df) {
    if (any(isInternalStandard(df$AnalyteName))) return(NULL)
    acq <- if ("Acquisition Date" %in% names(df)) as.numeric(as.POSIXct(df$`Acquisition Date`)) else NA_real_
    data.frame(plateID = as.character(df$plateID), acq = acq, stringsAsFactors = FALSE)
  })
  info <- do.call(rbind, rows)
  plates <- unique(info$plateID)

  # plate names in natural order: COVp2 before COVp10
  byName <- function(p) {
    num <- suppressWarnings(as.numeric(sub("^.*?(\\d+)$", "\\1", p)))
    num[!grepl("\\d$", p)] <- NA
    p[order(sub("\\d+$", "", p), num, p)]
  }

  if (!is.null(plateOrder)) {
    unknown <- setdiff(plateOrder, plates)
    if (length(unknown)) {
      warning("plateOrder contains plate(s) not in the data: ", paste(unknown, collapse = ", "),
              call. = FALSE)
    }
    notListed <- byName(setdiff(plates, plateOrder))
    note <- "Plates in order manually provided"
    if (length(notListed)) {
      note <- paste0(note, "; plates not listed added at the end (",
                     paste(notListed, collapse = ", "), ")")
    }
    return(list(order = c(intersect(plateOrder, plates), notListed), note = note))
  }

  firstRun <- tapply(info$acq, info$plateID, function(x) {
    if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  })
  if (length(firstRun) && all(!is.na(firstRun))) {
    return(list(order = names(sort(firstRun)),
                note = "Plates automatically ordered by Acquisition Date"))
  }

  list(order = byName(plates),
       note = "Plates ordered by name (No Acquisition Date for at least one plate, may provide plateOrder)")
}

# Column names used inside ggplot2 aes() and gt row selection
utils::globalVariables(c("plateExperimentID", "plateID", "sampleType", "variable", "Metric"))
