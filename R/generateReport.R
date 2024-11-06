#' Generate an HTML Report
#'
#' This function generates an HTML report using an R Markdown template.
#' 
#' @param data The dataset (dataElement) to be included in the report.
#' @param title The title of the report.
#' @param file Path name to folder containing files or path name to file. 
#' @param plateOrder Character vector of order plates should appear in. For 
#' example c("COVp005", "COVp002", "COVp001"). The default is NULL resulting in 
#' an order from Acquisition date (if present for all plates), or name order.
#' @param output_file The name of the output HTML file.
#' @examples
#' #generateReport(data = bio, output_file = "~/Desktop/report.html")
#' @return The path to the generated HTML report.
#' @export

generateReport <- function(data = NULL, file = NULL, plateOrder = NULL, title = "QC Report", output_file = "report.html", scale = TRUE) {
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
                      data = data,
                      file = file,
                      scale = scale
                    ),
                    envir = new.env(parent = globalenv())  # Use a new environment for rendering
  )
}
