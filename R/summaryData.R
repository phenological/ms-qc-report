
#' summaryData
#' 
#' Calculates the data for summaryTables and PassFail used to propagate colouring 
#' in plotQC. 
#' @param dae DataElement 
#' @param file path to a folder of multiple files or path to a single file.
#' @return list of data per plate
#' @import stats
#' @import ms.parser
summaryData <- function(dae = NULL){
  QCLTR <- list()
  
  ####if TSV#######
  # if(is(dae)[1] != "dataElement" && !(is.null(file))){
  # 
  #   folder <- file.path(file)
  #   
  #   plates <- dir(folder, pattern = "\\.TSV$")
  #   
  #   #if there is only one TSV
  #   if(identical(plates, character(0))){
  #     plates <- file
  #     
  #     d <- list()
  #     for (i in plates) {
  #       d[[i]] <- readAA(file = plates, 
  #                        optns = list())
  #     }
  #   } else{ #if there are multiple TSVs
  #     d <- list()
  #     for (i in plates) {
  #       d[[i]] <- readAA(file = file.path(folder, i), 
  #                        optns = list())
  #     }
  #   }
  #   
  #   DT_to_append <- do.call(rbind, d)
  #   obsCols <- c("Quantity", "Accuracy/Recovery[%]",  "R2", "plateID")
  #   #remove internal standards
  #   idx <- which(!grepl("\\[IS\\]", DT_to_append$AnalyteName) & 
  #                  (DT_to_append$sampleType == "qc" | DT_to_append$sampleType == "ltr") & 
  #                  DT_to_append$paramName %in% obsCols)
  #   
  #   result <- DT_to_append[idx,]
  #   
  #   analytes <- unique(result$AnalyteName)
  #   
  #   QCLTR <- list()
  #   for(analyte in analytes){
  #     idx <- which(result$AnalyteName == analyte)
  #     newish <- result[idx, ]
  #     newish <- newish[which(newish$paramName %in% obsCols),]
  #     
  #     # Use reshape to convert long to wide
  #     reshaped_df <- reshape(newish, 
  #                            idvar = c("sampleID", "AnalysisName", "AnalyteName", "projectName", 
  #                                      "cohortName", "sampleType", "sampleMatrixType"), 
  #                            timevar = "paramName", 
  #                            direction = "wide")
  #     
  #     # Clean up column names by removing the "paramValue." prefix added during reshaping
  #     colnames(reshaped_df) <- gsub("paramValue\\.", "", colnames(reshaped_df))
  #     cols <- c("AnalyteName", 
  #               "sampleID", 
  #               "Quantity", 
  #               "Accuracy/Recovery[%]", 
  #               "R2", 
  #               "plateID", 
  #               "sampleType")
  #     
  #     QCLTR[[analyte]] <- reshaped_df[,cols]
  #   }
  #  
  # }
  # 
  ####if DAE#####
  # if(is(dae)[1] == "dataElement" && is.null(file)){
    for(i in  1:length(dae@obsDescr)){
      df <- dae@obsDescr[[i]]
      if(any(grepl("\\[IS\\]", df$AnalyteName))) next
      idx<- which(df$sampleType == "qc" | df$sampleType == "ltr")
      cols <- c("AnalyteName", 
                "sampleID", 
                "Quantity", 
                "Accuracy/Recovery[%]", 
                "R2", 
                "plateID", 
                "sampleType")
      QCLTR[[i]] <- df[idx, cols]
      
    }
  # }
  
  all <- do.call(rbind, QCLTR)
  #make sure cols are numeric
  numCols <- c("Quantity", "Accuracy/Recovery[%]", "R2")
  
  all[numCols] <- lapply(all[numCols], as.numeric)
  
  # Replace NA with 0 in the specified numeric columns
  all$`Accuracy/Recovery[%]`[is.na(all$`Accuracy/Recovery[%]`)] <- 0
  QCLTRData <- list()
  QCLTRTables <- list()
  
  for (plate in unique(all$plateID)) {
    
    # Subset data for the current PlateID
    plate_data <- all[all$plateID == plate, ]
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(AnalyteName = unique(plate_data$AnalyteName),
                            totalQCInjectionPass = character(length(unique(plate_data$AnalyteName))),
                            totalQCPassPerc = numeric(length(unique(plate_data$AnalyteName))),
                            LTR_RSD = numeric(length(unique(plate_data$AnalyteName))),
                            R2 = numeric(length(unique(plate_data$AnalyteName))),
                            PassFail = numeric(length(unique(plate_data$AnalyteName))),
                            stringsAsFactors = FALSE)
    
    # Loop through each AnalyteName within the current PlateID
    for (analyte in unique(plate_data$AnalyteName)) {
      
      # Subset data for the current AnalyteName
      analyte_data <- plate_data[plate_data$AnalyteName == analyte, ]
      
      ## Calculate totalQCInjectionPass (character)
      qc_data <- analyte_data[analyte_data$sampleType == "qc", ]
      pass_qc <- sum(qc_data$`Accuracy/Recovery[%]` >= 80 & qc_data$`Accuracy/Recovery[%]` <= 120)
      total_qc <- nrow(qc_data)
      result_df[result_df$AnalyteName == analyte, "totalQCInjectionPass"] <- paste(pass_qc, total_qc, sep = "/")
      
      ## Calculate totalQCPass(%) (numeric)
      result_df[result_df$AnalyteName == analyte, "totalQCPassPerc"] <- round((pass_qc / total_qc) * 100, 2)
      
      ##add R2
      # result_df[result_df$AnalyteName == analyte, "R2"] <- unique(na.omit(analyte_data$R2))
      R2_values <- unique(na.omit(analyte_data$R2))
      if (length(R2_values) > 0) {
        result_df[result_df$AnalyteName == analyte, "R2"] <- R2_values[1]  # Use the first unique R2 value
      } else {
        result_df[result_df$AnalyteName == analyte, "R2"] <- 0  # Assign NA if no valid R2 value
      }
      
      ## Calculate LTR RSD (numeric)
      ltr_data <- analyte_data[analyte_data$sampleType == "ltr", ]
      if (nrow(ltr_data) > 1) {
        rsd <- (sd(ltr_data$Quantity) / mean(ltr_data$Quantity)) * 100
        if(rsd == "NaN"){
          rsd <- NA
        }
        result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- round(rsd, 2)
      } else {
        result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- NA  # Not enough data for RSD calculation
      }
      
      result_df$PassFail <- ifelse(
        (result_df$R2 >= 0.99) +                                  # Check if R2 is ≥ 0.99
          (result_df$totalQCPassPerc >= 66) +                       # Check if totalQCPass(%) is ≥ 66%
          (result_df$LTR_RSD <= 20 & !is.na(result_df$LTR_RSD))    # Check if LTR RSD is ≤ 20 
        >= 3,                                                     # At least 3 of the conditions must be TRUE
        "PASS", "CAUTION"                                             # Return "PASS" if TRUE, otherwise "CAUTION"
      )
    }
    
    # Store the result data frame for the current PlateID in the list
    QCLTRData[[plate]] <- result_df
    
  }
  
  
  #LTR RSD across all plates
  result_df <- data.frame(AnalyteName = unique(all$AnalyteName),
                          LTR_RSD = numeric(length(unique(all$AnalyteName))),
                          stringsAsFactors = FALSE)
  
  for (analyte in unique(all$AnalyteName)) {
    ## Calculate LTR RSD (numeric)
    ltr_data <- all[all$AnalyteName == analyte & all$sampleType == "ltr", ]
  
    if (nrow(ltr_data) > 1) {
      rsd <- (sd(ltr_data$Quantity) / mean(ltr_data$Quantity)) * 100
      if(rsd == "NaN"){
        rsd <- NA
      }
      result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- round(rsd, 2)
    } else {
      result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- NA  # Not enough data for RSD calculation
    }
    
  }
  QCLTRData[["allPlates"]] <- result_df
  
  return(QCLTRData)
}

