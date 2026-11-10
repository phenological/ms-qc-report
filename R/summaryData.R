
#' summaryData
#' 
#' Calculates the data for summaryTables and PassFail used to propagate colouring
#' in plotQC. Internal standards are excluded for every assay; for BA only the
#' bile acids in BA_names are kept. Criteria are in QC_CRITERIA (R/Constants.R).
#' @param dae DataElement
#' @return list of data per plate, plus "allPlates" (LTR RSD across all plates)
#' @import stats
#' @import fusion
#' @import ms.parser
summaryData <- function(dae){
  QCLTR <- list()

  for(i in  1:length(dae@obsDescr)){
    df <- dae@obsDescr[[i]]

    # remove internal standards (all assays), before any renaming
    if(any(isInternalStandard(df$AnalyteName))) next

    # keep names whole, except BA: "CDCA-Chenodeoxycholic acid" -> "CDCA"
    df$AnalyteName <- cleanAnalyteName(df$AnalyteName, dae@method)

    if(dae@method=="BA"){
      if(!any(df$AnalyteName %in% BA_names)) next
    }
    
    if(dae@method == "SCFA"){
      idx<- which(df$sampleType == "qc" | df$sampleType == "pqc")
    } else{idx<- which(df$sampleType == "qc" | df$sampleType == "ltr")}
    
    cols <- c("AnalyteName", 
              "sampleID", 
              "Quantity", 
              # "relative Area",
              "Accuracy/Recovery[%]", 
              "R2", 
              "plateID", 
              "sampleType")
    QCLTR[[i]] <- df[idx, cols]
    
  }
  
  
  all <- do.call(rbind, QCLTR)
  if (is.null(all) || nrow(all) == 0) {
    stop("No QC data to report for assay '", dae@method, "': no analytes with qc/",
         if (dae@method == "SCFA") "pqc" else "ltr", " samples were found.", call. = FALSE)
  }
  #make sure cols are numeric
  numCols <- c("Quantity",
               # "relative Area",
               "Accuracy/Recovery[%]", "R2")
  
  all[numCols] <- lapply(all[numCols], as.numeric)
  
  # Replace NA with 0 in the specified numeric columns
  all$`Accuracy/Recovery[%]`[is.na(all$`Accuracy/Recovery[%]`)] <- 0
  # all$`relative Area`[is.na(all$`relative Area`)] <- 0
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
                            # response_RSD = numeric(length(unique(plate_data$AnalyteName))),
                            R2 = numeric(length(unique(plate_data$AnalyteName))),
                            PassFail = numeric(length(unique(plate_data$AnalyteName))),
                            stringsAsFactors = FALSE)
    
    # Loop through each AnalyteName within the current PlateID
    for (analyte in unique(plate_data$AnalyteName)) {
      
      # Subset data for the current AnalyteName
      analyte_data <- plate_data[plate_data$AnalyteName == analyte, ]
      
      ## Calculate totalQCInjectionPass (character)
      qc_data <- analyte_data[analyte_data$sampleType == "qc", ]
      acc <- QC_CRITERIA$ACC_RANGE
      pass_qc <- sum(qc_data$`Accuracy/Recovery[%]` >= acc[1] & qc_data$`Accuracy/Recovery[%]` <= acc[2])
      total_qc <- nrow(qc_data)
      result_df[result_df$AnalyteName == analyte, "totalQCInjectionPass"] <- paste(pass_qc, total_qc, sep = "/")

      ## Calculate totalQCPass(%) (numeric); NA if the plate has no QC injections
      result_df[result_df$AnalyteName == analyte, "totalQCPassPerc"] <-
        if (total_qc > 0) round((pass_qc / total_qc) * 100, 2) else NA

      ##add R2
      R2_values <- unique(na.omit(analyte_data$R2))
      if (length(R2_values) > 0) {
        result_df[result_df$AnalyteName == analyte, "R2"] <- R2_values[1]  # Use the first unique R2 value
      } else {
        result_df[result_df$AnalyteName == analyte, "R2"] <- NA  # no R2 reported (gives CAUTION)
      }
      
      ## Calculate LTR RSD (numeric)
      if(dae@method == "SCFA"){
        ltr_data <- analyte_data[analyte_data$sampleType == "pqc", ]
      } else{ltr_data <- analyte_data[analyte_data$sampleType == "ltr", ]}
      
      result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- calcRSD(ltr_data$Quantity)

    }

    # PASS only if all 3 criteria are met; anything missing (NA) gives CAUTION
    result_df$PassFail <- ifelse(
      !is.na(result_df$R2) & !is.na(result_df$totalQCPassPerc) & !is.na(result_df$LTR_RSD) &
        result_df$R2 >= QC_CRITERIA$R2_MIN &
        result_df$totalQCPassPerc >= QC_CRITERIA$QC_PASS_MIN &
        result_df$LTR_RSD <= QC_CRITERIA$RSD_MAX,
      "PASS", "CAUTION"
    )
    
    # Store the result data frame for the current PlateID in the list
    QCLTRData[[plate]] <- result_df
    
  }
  
  
  #LTR RSD across all plates
  result_df <- data.frame(AnalyteName = unique(all$AnalyteName),
                          LTR_RSD = numeric(length(unique(all$AnalyteName))),
                          # response_RSD = numeric(length(unique(all$AnalyteName))),
                          stringsAsFactors = FALSE)
  
  for (analyte in unique(all$AnalyteName)) {
    ## Calculate LTR RSD (numeric)
    if(dae@method == "SCFA"){
      ltr_data <- all[all$AnalyteName == analyte & all$sampleType == "pqc", ]
    }else{ ltr_data <- all[all$AnalyteName == analyte & all$sampleType == "ltr", ]}
    
    ## LTR RSD
    result_df[result_df$AnalyteName == analyte, "LTR_RSD"] <- calcRSD(ltr_data$Quantity)

  }
  QCLTRData[["allPlates"]] <- result_df

  return(QCLTRData)
}

#' RSD (%) of x, or NA if it can't be calculated (fewer than 2 values, mean 0)
#' @noRd
calcRSD <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(NA_real_)
  rsd <- (sd(x) / mean(x)) * 100
  if (!is.finite(rsd)) NA_real_ else round(rsd, 2)
}

