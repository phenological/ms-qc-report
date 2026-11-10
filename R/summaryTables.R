#'summaryTables
#'
#' Displays status (pass or caution) for each analyte in each plate in table format.
#' @param dae - dae of targeted Mass Spectrometry.
#' @param plateOrder Character vector of order plates should appear in. For
#' example c("COVp005", "COVp002", "COVp001"); plates not listed are added at
#' the end. The default is NULL resulting in an order from Acquisition date (if
#' present for all plates), or name order. plotQC() uses the same order.
#' @return list with `data` and `tables` (a slot for each plate, then
#' "allPlates") and `note` (how plates were ordered). PASS needs all 3 criteria
#' in QC_CRITERIA (R/Constants.R): R2 >= 0.99; >= 66% of qc injections with
#' 80 <= `Accuracy/Recovery[%]` <= 120; LTR RSD <= 20 (PQC RSD for SCFA).
#' Anything missing gives CAUTION. Cells are green when they meet their
#' criterion and red otherwise.
#'
#' @export
#' @import gt
summaryTables <- function(dae = NULL, plateOrder = NULL){
  QCLTRData <- summaryData(dae = dae)

  ######Plate Order#######
  # same order as plotQC(); "allPlates" always last
  ord <- plateOrderInfo(dae, plateOrder)
  ordered_plate_names <- c(intersect(ord$order, names(QCLTRData)), "allPlates")
  QCLTRData <- QCLTRData[ordered_plate_names]

  # name of the RSD row: PQC_RSD for SCFA, LTR_RSD otherwise
  rsd_row <- if (dae@method == "SCFA") "PQC_RSD" else "LTR_RSD"
  
  QCLTRTables <- list()
  #########table formatting#############
  for(plate in ordered_plate_names){
    
    result_df <- QCLTRData[[plate]]
    # Get unique analyte names
    analyte_names <- unique(result_df$AnalyteName)
    
    # Create an empty data frame to hold the rotated data
    rotated_df <- data.frame(Metric = c("totalQCInjectionPass", "totalQCPassPerc", "LTR_RSD",
                                        # "response_RSD", 
                                        "R2", "PassFail"))
    
    # Loop through each analyte and extract values for each metric
    for (analyte in analyte_names) {
      # Get the row corresponding to the current analyte
      analyte_data <- result_df[result_df$AnalyteName == analyte, ]
      
      # Extract the values for each metric
      metric_values <- c(analyte_data$totalQCInjectionPass,
                         analyte_data$totalQCPassPerc,
                         analyte_data$LTR_RSD,
                         # analyte_data$response_RSD,
                         analyte_data$R2,
                         analyte_data$PassFail)
      
      # Add the values to the rotated data frame
      rotated_df <- cbind(rotated_df, metric_values)
    }
    
    if(plate == "allPlates"){
      rotated_df <- rotated_df[which(rotated_df$Metric %in%c( "LTR_RSD")),]
    }
    
    # label the RSD row (PQC_RSD for SCFA); the colouring below uses rsd_row
    rotated_df$Metric[rotated_df$Metric == "LTR_RSD"] <- rsd_row
    
    # Set column names for the rotated data frame
    colnames(rotated_df) <- c("Metric", analyte_names)
    
    table_output <- gt(data = rotated_df)
    
    # Add a header to the table
    table_output <- tab_header(
      table_output,
      title = paste(plate)
    )
    
    if(plate != "allPlates"){
      # Loop through each analyte column to apply color coding for PassFail
      for (analyte in colnames(rotated_df)[-1]) {  # Skip the Metric column
        # Get the PassFail value for the current analyte
        pass_fail_value <- rotated_df[rotated_df$Metric =="PassFail", analyte]
        
        # Set color based on the PassFail value
        if (is.na(pass_fail_value) || pass_fail_value == "CAUTION") {
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
        ######### 
        pass_fail_value <- as.numeric(rotated_df[rotated_df$Metric == rsd_row, analyte])
        
        # Set color: same limit as PASS/CAUTION (RSD <= RSD_MAX passes)
        if (is.na(pass_fail_value) || pass_fail_value > QC_CRITERIA$RSD_MAX) {
          color <- "red"
        } else if (pass_fail_value <= QC_CRITERIA$RSD_MAX) {
          color <- "green"
        } else {
          color <- "white"  # Default color if it's neither
        }
        
        # Apply the color to the PassFail row
        table_output <- tab_style(
          table_output,
          style = cell_fill(color = color),
          locations = cells_body(
            rows = Metric == rsd_row,
            columns = !!analyte
          )
        )
        
        #########  TOTAL QC PASS PERC
        pass_fail_value <- as.numeric(rotated_df[rotated_df$Metric =="totalQCPassPerc", analyte])
        
        # Set color based on the PassFail value
        if (is.na(pass_fail_value) || pass_fail_value < QC_CRITERIA$QC_PASS_MIN) {
          color <- "red"
        } else if (pass_fail_value >= QC_CRITERIA$QC_PASS_MIN) {
          color <- "green"
        } else {
          color <- "white"  # Default color if it's neither
        }
        
        # Apply the color to the PassFail row
        table_output <- tab_style(
          table_output,
          style = cell_fill(color = color),
          locations = cells_body(
            rows = Metric == "totalQCPassPerc",
            columns = !!analyte
          )
        )
        
        #########  R2
        pass_fail_value <- as.numeric(rotated_df[rotated_df$Metric =="R2", analyte])
        
        # Set color based on the PassFail value
        if (is.na(pass_fail_value) || pass_fail_value < QC_CRITERIA$R2_MIN) {
          color <- "red"
        } else if (pass_fail_value >= QC_CRITERIA$R2_MIN) {
          color <- "green"
        } else {
          color <- "white"  # Default color if it's neither
        }
        
        # Apply the color to the PassFail row
        table_output <- tab_style(
          table_output,
          style = cell_fill(color = color),
          locations = cells_body(
            rows = Metric == "R2",
            columns = !!analyte
          )
        )
        
        
        
        
      }
    }
    
    QCLTRTables[[plate]] <- table_output
  }
  
  
  QCLTRResults <- list(data = QCLTRData,
                       tables = QCLTRTables,
                       note = ord$note)
  
  return(QCLTRResults)
  
}


