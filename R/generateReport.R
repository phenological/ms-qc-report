#' Generate an HTML Report
#'
#' This function generates an HTML report using an R Markdown template.
#' 
#' @param folder Either a character for the file path or a dataElement to be included in the report.
#' @param assay Character of "ddhN", "AA", "TRY", "BA" or "SCFA".
#' @param title The title of the report.
#' @param fileType Character "xml", "tsv" or "txt" if supplying file path to the folder argument. 
#' @param scale - Boolean. TRUE if the data should be log10(value + 1) 
#' @param plateOrder Character vector of order plates should appear in. For 
#' example c("COVp005", "COVp002", "COVp001"). The default is NULL resulting in 
#' an order from Acquisition date (if present for all plates), or name order.
#' @param output_file The name of the output HTML file.
#' @return The path to the generated HTML report.
#' @export

generateReport <- function(folder = NULL, fileType = NULL, assay = NULL, plateOrder = NULL, title = "QC Report", output_file = "report.html", scale = TRUE) {
  # Define the path to the Rmd template
  rmd_file <- system.file("rmarkdown/templates/reportTemplate.Rmd", package = "ms.qc.report")
  # Check if the Rmd template is available
  if (rmd_file == "") {
    stop("Rmd template not found in the package!")
  }
  
  # Render the Rmd file to HTML, passing data and title as parameters
  rmarkdown::render(rmd_file, 
                    output_file = output_file, 
                    params = list(
                      title = title,
                      folder = folder,
                      fileType = fileType,
                      assay = assay,
                      scale = scale,
                      plateOrder = plateOrder
                    ),
                    envir = new.env(parent = globalenv())  # Use a new environment for rendering
  )
}
