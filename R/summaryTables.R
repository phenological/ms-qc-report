#'summaryTables
#'
#' Displays status (pass or caution) for each analyte in each plate in table format.
#' @param dae - dae of targeted Mass Spectrometry.
#' @param plateOrder Character vector of order plates should appear in. For 
#' example c("COVp005", "COVp002", "COVp001"). The default is NULL resulting in 
#' an order from Acquisition date (if present for all plates), or name order.
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

summaryTables <- function(dae = NULL, plateOrder = NULL){

  QCLTRData <- summaryData(dae = dae)
  
  ######Plate Order#######
  df <- dae@obsDescr[[1]]
  if("Acquisition Date" %in% names(df) && all(!is.na(df$`Acquisition Date`)) & is.null(plateOrder)){
    #factor plates
    plate_min_times <- aggregate(`Acquisition Date` ~ plateID, data = df, min)
    
    # Order plateIDs by their minimum Acquisition Time
    ordered_plateIDs <- plate_min_times$plateID[order(plate_min_times$`Acquisition Date`)]
    plateOrder <- ordered_plateIDs
  }
  
  if((any(is.na(df$`Acquisition Date`)) | !("Acquisition Date" %in% names(df))) & is.null(plateOrder)){
    plateOrder <- names(QCLTRData)
  }
  
  if(!is.null(plateOrder)){
  }
  
  # Order the plate names in QCLTRData based on plateOrder
  ordered_plate_names <- intersect(plateOrder, names(QCLTRData))
  QCLTRData <- QCLTRData[ordered_plate_names]
  
  QCLTRTables <- list()
    #########table formatting#############
  for(plate in ordered_plate_names){
    
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
    
    if(plate == "allPlates"){
      rotated_df <- rotated_df[which(rotated_df$Metric == "LTR_RSD"),]
    }
    
    if(dae@method == "SCFA"){
      rotated_df$Metric[rotated_df$Metric == "LTR_RSD"] <- "PQC_RSD"
    }
    
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
        pass_fail_value <- rotated_df[rotated_df$Metric == "PassFail", analyte]
        
        # Set color based on the PassFail value
        if (pass_fail_value == "CAUTION" | is.na(pass_fail_value)) {
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
    }

    QCLTRTables[[plate]] <- table_output
    }
    
  
  QCLTRResults <- list(data = QCLTRData,
                       tables = QCLTRTables)
  
 return(QCLTRResults)

  }



