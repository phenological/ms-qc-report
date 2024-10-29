#' Generate an HTML Report
#'
#' This function generates an HTML report using an R Markdown template.
#' 
#' @param data The dataset (dataElement) to be included in the report.
#' @param title The title of the report.
#' @param output_file The name of the output HTML file.
#' @examples
#' #generateReport(data = bio, output_file = "~/Desktop/report.html")
#' @return The path to the generated HTML report.
#' @export

generateReport <- function(data, title = "QC Report", output_file = "report.html") {
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
                      data = data
                    ),
                    envir = new.env(parent = globalenv())  # Use a new environment for rendering
  )
}