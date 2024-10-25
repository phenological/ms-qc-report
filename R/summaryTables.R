#'summaryTables
#'
#' Displays status (pass or caution) for each analyte in each plate in table format.
#' @param dae - dae of targeted Mass Spectrometry.
#' @return list of tables and matching data with a slot for each plate. Criteria 
#' for PASS, must meet all 3 criteria of R2 is ≥ 0.99, LTR RSD is ≤ 20 or "N/A" 
#' (uses ltr sampleType Quantity), totalQCInjectionPass fraction of qc samples 
#' with 80=< Accuracy/Recovery`[%]` =< 120 totalQCPass(%), percent of qc injections 
#' that have 80=< Accuracy/Recovery`[%]` =< 120. Pass needs to be ≥ 66% (uses qc sampleType)
#'
#' @export
#' @import gt
#' @importFrom reshape2 melt


#make NA for qc related stuff 0, RSD of Nan can be NA and flag a caution

summaryTables <- function(dae){

  QCLTRData <- summaryData(dae = dae)
  
  QCLTRTables <- list()
    #########table formatting#############
  for(plate in names(QCLTRData)){
    
    result_df <- QCLTRData[[plate]]
    # Get unique analyte names
    analyte_names <- unique(result_df$AnalyteName)
    
    # Create an empty data frame to hold the rotated data
    rotated_df <- data.frame(Metric = c("totalQCInjectionPass", "totalQCPassPerc", "LTR_RSD", "R2", "PassFail"))
    
    # Loop through each analyte and extract values for each metric
    for (analyte in analyte_names) {
      # Get the row corresponding to the current analyte
      analyte_data <- result_df[result_df$AnalyteName == analyte, ]
      
      # Extract the values for each metric
      metric_values <- c(analyte_data$totalQCInjectionPass,
                         analyte_data$totalQCPassPerc,
                         analyte_data$LTR_RSD,
                         analyte_data$R2,
                         analyte_data$PassFail)
      
      # Add the values to the rotated data frame
      rotated_df <- cbind(rotated_df, metric_values)
    }
    
    # Set column names for the rotated data frame
    colnames(rotated_df) <- c("Metric", analyte_names)
    
    table_output <- gt(data = rotated_df)
    
    # Add a header to the table
    table_output <- tab_header(
      table_output,
      title = paste(plate)
    )
    
    # Loop through each analyte column to apply color coding for PassFail
    for (analyte in colnames(rotated_df)[-1]) {  # Skip the Metric column
      # Get the PassFail value for the current analyte
      pass_fail_value <- rotated_df[rotated_df$Metric == "PassFail", analyte]
      
      # Set color based on the PassFail value
      if (pass_fail_value == "CAUTION") {
        color <- "red"
      } else if (pass_fail_value == "PASS") {
        color <- "green"
      } else {
        color <- "white"  # Default color if it's neither
      }
      
      # Apply the color to the PassFail row
      table_output <- tab_style(
        table_output,
        style = cell_fill(color = color),
        locations = cells_body(
          rows = Metric == "PassFail",
          columns = !!analyte
        )
      )
    }
    QCLTRTables[[plate]] <- table_output
    }
    
  QCLTRResults <- list(data = QCLTRData,
                       tables = QCLTRTables)
  
 return(QCLTRResults)

  }



