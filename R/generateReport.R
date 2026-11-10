#' Generate an HTML Report
#'
#' This function generates an HTML QC report using the package's R Markdown
#' template (inst/rmarkdown/templates/reportTemplate.Rmd), with the details
#' needed for an ISO/IEC 17025 record. The report shows DRAFT until it has a
#' report ID, all three sign-off initials and a decision (in `decisions`) for
#' every CAUTION result; it then shows AUTHORISED.
#'
#' @param data The data: a folder of exported files, a series of files
#'   (character vector, or one comma-separated string), a wildcard such as
#'   "exports/COVp*.xml", a dataElement saved as .rds, or a dataElement.
#'   Relative paths are taken from the working directory.
#' @param fileType Character "xml", "tsv" or "txt". Optional: taken from the
#'   file extensions if not given.
#' @param assay Character of "ddhN", "AA", "TRY", "BA" or "SCFA".
#' @param plateOrder Character vector of order plates should appear in. For
#'   example c("COVp005", "COVp002", "COVp001"); plates not listed are added at
#'   the end. The default is NULL resulting in an order from Acquisition date
#'   (if present for all plates), or name order.
#' @param title Title of the report.
#' @param output_file Path of the HTML file. A bare file name is saved in the
#'   working directory; ".html" is added if missing.
#' @param reportID Unique ID, e.g. "covid19_biogune_COVr01_MS_AA_QC_20261109".
#'   If NULL, one is made from project, runID, assay and today's date.
#' @param revision 0 = original; 1, 2, ... for a reissued report.
#' @param supersedes ID of the report this one replaces (required if
#'   revision > 0).
#' @param client Client contact name.
#' @param contact Client contact details (e.g. email).
#' @param sampleReceivedDate Date the samples were received ("YYYY-MM-DD").
#' @param project Project, e.g. "IRMA".
#' @param runID Run ID, e.g. "COVr01".
#' @param methodID Method / SOP reference and version, e.g. "v2".
#' @param instrument Instrument name or serial number, e.g. "PAI03".
#' @param ms_specialist_1 Initials of the person who processed the data and
#'   prepared the report.
#' @param ms_specialist_2 Initials of the person who independently reviewed the
#'   QC results.
#' @param ms_lead Initials of the person who authorised the report for release.
#' @param notes Deviations from the method, reruns, excluded samples or other
#'   remarks. Shown as "None." if empty.
#' @param materials Lots / IDs of reference materials, as a named vector, e.g.
#'   c(Calibrants = "CAL-2411", "QC material" = "QC-2410"), or a data frame.
#' @param decisions Decisions on CAUTION results: a data frame with columns
#'   plate, analyte, disposition ("Accept", "Re-run" or "Reject") and
#'   justification (required for "Accept").
#' @param uncertainty Working range / measurement uncertainty table. NULL (the
#'   default) uses the package table for the assay
#'   (inst/uncertainty/<assay>.csv); or give the path to a CSV, or a data frame,
#'   with columns analyte, LLOQ, ULOQ, units, U_percent.
#' @param scale Boolean. TRUE (default) if the data should be log10(value + 1).
#' @return The path to the generated HTML report, invisibly.
#' @export
#' @importFrom htmltools tagList tagAppendChild h3
#' @importFrom knitr kable
#' @importFrom tools md5sum file_ext
#' @importFrom utils read.csv packageDescription
#' @importFrom methods is
#' @examples
#' \dontrun{
#' generateReport(
#'   data = "~/Downloads/BA_xml/", assay = "BA",
#'   title = "IRMA SRVr015 MS BA", output_file = "IRMA_SRVr015_MS_BA.html",
#'   client = "Client name", contact = "client@example.com",
#'   sampleReceivedDate = "2026-10-28", project = "IRMA", runID = "SRVr015",
#'   methodID = "v2", instrument = "PAI03",
#'   ms_specialist_1 = "YW", ms_specialist_2 = "MR", ms_lead = "RK",
#'   decisions = data.frame(plate = "SRVp001", analyte = "GCA",
#'                          disposition = "Re-run", justification = NA)
#' )
#' }
generateReport <- function(data = NULL, fileType = NULL, assay = NULL, plateOrder = NULL,
                           title = "QC Report", output_file = "qcreport.html",
                           reportID = NULL, revision = 0, supersedes = NULL,
                           client = NULL, contact = NULL, sampleReceivedDate = NULL,
                           project = NULL, runID = NULL, methodID = NULL, instrument = NULL,
                           ms_specialist_1 = NULL, ms_specialist_2 = NULL, ms_lead = NULL,
                           notes = NULL, materials = NULL, decisions = NULL,
                           uncertainty = NULL, scale = TRUE) {

  # ---- Check the arguments before rendering ------------------------------------
  assays <- c("SCFA", "AA", "BA", "TRY", "ddhN")
  if (is.null(data)) {
    stop("`data` must be a folder, files, an .rds file or a dataElement.", call. = FALSE)
  }
  if (is.null(assay) || length(assay) != 1 || !assay %in% assays) {
    stop("`assay` must be one of: ", paste(assays, collapse = ", "), call. = FALSE)
  }
  if (!is.logical(scale) || length(scale) != 1 || is.na(scale)) {
    stop("`scale` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(revision) || length(revision) != 1 || is.na(revision) || revision < 0) {
    stop("`revision` must be 0 (original) or 1, 2, ... (reissued).", call. = FALSE)
  }
  if (revision > 0 && (is.null(supersedes) || !nzchar(supersedes))) {
    stop("A reissued report (revision > 0) needs `supersedes`: the ID of the report it replaces.",
         call. = FALSE)
  }
  if (!is.null(fileType) && !tolower(fileType) %in% c("xml", "tsv", "txt")) {
    stop("`fileType` must be \"xml\", \"tsv\" or \"txt\".", call. = FALSE)
  }
  if (!is.null(sampleReceivedDate) && nzchar(sampleReceivedDate) &&
      is.na(as.Date(sampleReceivedDate, format = "%Y-%m-%d"))) {
    stop("`sampleReceivedDate` must be a date as \"YYYY-MM-DD\".", call. = FALSE)
  }

  # The report is rendered in a temporary folder, so make paths absolute
  absolute <- function(p) {
    p <- path.expand(p)
    relative <- !grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", p)
    p[relative] <- file.path(getwd(), p[relative])
    p
  }
  if (!isS4(data) && is.character(data)) {
    paths <- trimws(unlist(strsplit(data, ",", fixed = TRUE)))
    paths <- absolute(paths[nzchar(paths)])
    missing <- paths[!file.exists(paths) & !grepl("[*?]", paths)]
    if (length(missing)) stop("Data not found: ", paste(missing, collapse = ", "), call. = FALSE)
    data <- paste(paths, collapse = ", ")
  }
  if (is.character(uncertainty) && nzchar(uncertainty)) {
    uncertainty <- absolute(uncertainty)
    if (!file.exists(uncertainty)) stop("Uncertainty file not found: ", uncertainty, call. = FALSE)
  }

  # Initials: 2-4 letters, stored in upper case
  initials <- list(ms_specialist_1 = ms_specialist_1, ms_specialist_2 = ms_specialist_2,
                   ms_lead = ms_lead)
  for (role in names(initials)) {
    v <- initials[[role]]
    if (is.null(v) || !nzchar(trimws(v))) { initials[[role]] <- ""; next }
    v <- toupper(gsub("[[:space:].]", "", v))
    if (!grepl("^[A-Z]{2,4}$", v)) {
      stop("`", role, "` must be initials (2-4 letters), e.g. \"YW\".", call. = FALSE)
    }
    initials[[role]] <- v
  }

  if (!is.null(materials) && !is.data.frame(materials)) {
    if (is.null(names(materials)) || any(!nzchar(names(materials)))) {
      stop("`materials` must be named, e.g. c(Calibrants = \"CAL-2411\").", call. = FALSE)
    }
    materials <- data.frame(Material = names(materials),
                            `Lot / ID` = unname(as.character(materials)),
                            check.names = FALSE, stringsAsFactors = FALSE)
  }

  if (!is.null(decisions)) {
    if (!is.data.frame(decisions)) stop("`decisions` must be a data frame.", call. = FALSE)
    names(decisions) <- tolower(names(decisions))
    need <- setdiff(c("plate", "analyte", "disposition"), names(decisions))
    if (length(need)) {
      stop("`decisions` is missing column(s): ", paste(need, collapse = ", "),
           ". Expected: plate, analyte, disposition, justification.", call. = FALSE)
    }
    if (!"justification" %in% names(decisions)) decisions$justification <- NA_character_
  }

  if (is.null(reportID) || !nzchar(reportID)) {
    parts <- c(project, runID, "MS", assay, "QC", format(Sys.Date(), "%Y%m%d"))
    reportID <- paste(gsub("[^A-Za-z0-9_-]+", "", parts[nzchar(parts)]), collapse = "_")
  }

  text <- function(x) if (is.null(x)) "" else paste(x, collapse = ", ")

  # ---- Output location, relative to the working directory -----------------------
  output_file <- path.expand(output_file)
  if (!grepl("\\.html?$", output_file, ignore.case = TRUE)) {
    output_file <- paste0(output_file, ".html")
  }
  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) stop("Output folder does not exist: ", out_dir, call. = FALSE)
  out_dir <- normalizePath(out_dir, mustWork = TRUE)

  # ---- Render a temporary copy of the template ---------------------------------
  # (rendering in place would write into the installed package folder)
  rmd_file <- system.file("rmarkdown/templates/reportTemplate.Rmd", package = "ms.qc.report")
  if (rmd_file == "") {
    stop("Rmd template not found in the package!", call. = FALSE)
  }
  work <- tempfile("ms-qc-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  file.copy(rmd_file, file.path(work, "reportTemplate.Rmd"))

  out <- rmarkdown::render(
    file.path(work, "reportTemplate.Rmd"),
    output_file       = basename(output_file),
    output_dir        = out_dir,
    intermediates_dir = work,
    knit_root_dir     = work,
    params = list(
      title              = title,
      data               = data,
      fileType           = text(fileType),
      assay              = assay,
      scale              = scale,
      plateOrder         = text(plateOrder),
      reportID           = reportID,
      revision           = revision,
      supersedes         = text(supersedes),
      client             = text(client),
      contact            = text(contact),
      sampleReceivedDate = text(sampleReceivedDate),
      project            = text(project),
      runID              = text(runID),
      methodID           = text(methodID),
      instrument         = text(instrument),
      ms_specialist_1    = initials$ms_specialist_1,
      ms_specialist_2    = initials$ms_specialist_2,
      ms_lead            = initials$ms_lead,
      notes              = text(notes),
      uncertainty        = if (is.null(uncertainty)) "" else uncertainty,
      materials          = materials,
      decisions          = decisions
    ),
    envir = new.env(parent = globalenv()),
    quiet = TRUE
  )

  message("Report ", reportID, " saved to ", out)
  invisible(normalizePath(out))
}
