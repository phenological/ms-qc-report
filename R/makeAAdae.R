#' makeAAdae
#' @param result results from SQLite database
#' @param file Path name to folder containing files or path name to file.
#' @import ms.parser
#' @import fusion
#' @import reshape2
#' @import methods
#' @return data element
#' @export
makeAAdae <- function(result = NULL, file = NULL){
  
  ####if TSV#######
  if(is(result)[1] != "data.frame" && !(is.null(file))){
    
    folder <- file.path(file)
    
    plates <- dir(folder, pattern = "\\.TSV$")
    
    #if there is only one TSV
    if(identical(plates, character(0))){
      plates <- file
      
      d <- list()
      for (i in plates) {
        d[[i]] <- readAA(file = plates, 
                         optns = list())
      }
    } else{ #if there are multiple TSVs
      d <- list()
      for (i in plates) {
        d[[i]] <- readAA(file = file.path(folder, i), 
                         optns = list())
      }
    }
    result <- do.call(rbind, d) 
  }
  #########make obsDescr###########
  
  #columns other than sampleMatrixType, sampleID, sampleType, AnalysisName and AnalyteName 
  obCols <- c( 
    "m/z",
    "expected m/z",
    "Area",
    "Height",
    "mSigma",
    "Retention Time[min]",
    "Expected Retention Time[min]",
    "Quantity",
    "Internal Standard(ISTD)",
    "Expected Quantity",
    "Unit of Quantity",
    "R2",
    "Accuracy/Recovery[%]",
    "Residuals[%]",
    "Visited",
    "plateID", 
    "Internal standard used for calibration function calculation", 
    "ISTD Area",
    "relative Area",
    "Manually Integrated",
    "Acquisition Date"
  )
  
  #are the obsCols available
  obCols %in% unique(result$paramName)
  
  #should be obsDescr for each analyte in a list. 
  analytes <- unique(result$AnalyteName)
  
  obsDescr <- list()
  for(analyte in analytes){
    idx <- which(result$AnalyteName == analyte)
    newish <- result[idx, ]
    newish <- newish[which(newish$paramName %in% obCols),]
    
    # Use reshape to convert long to wide
    reshaped_df <- reshape(newish, 
                           idvar = c("sampleID", "AnalysisName", "AnalyteName", "projectName", 
                                     "cohortName", "sampleType", "sampleMatrixType"), 
                           timevar = "paramName", 
                           direction = "wide")
    
    # Clean up column names by removing the "paramValue." prefix added during reshaping
    colnames(reshaped_df) <- gsub("paramValue\\.", "", colnames(reshaped_df))
    
    #change Acquisition dates to poscixt
    if("Acquisition Date" %in% names(reshaped_df)){
      idx <- which(!is.na(reshaped_df$`Acquisition Date`))
      reshaped_df[, "Acquisition Date"] <- gsub(" [A-Z]{3,4} ", " ", reshaped_df[, "Acquisition Date"])
      reshaped_df[, "Acquisition Date"] <- as.POSIXct(reshaped_df[, "Acquisition Date"], format = "%a %b %d %H:%M:%S %Y", tz = "Australia/Perth")
    }
    
    obsDescr[[analyte]] <- reshaped_df
  }
  
  #original sampleIDs
  original_sampleIDs <- lapply(obsDescr, function(df) df$sampleID)
  
  # reference_sampleID <- obsDescr[[1]]$sampleID
  
  # # Check if the sampleID is the same order in all data frames
  # same_order <- sapply(obsDescr, function(df) identical(df$sampleID, reference_sampleID))
  # 
  # # Print result
  # if (all(same_order)) {
  #   print("The sampleID column is in the same order in all slots.")
  # } else {
  #   print("The sampleID column is NOT in the same order in all slots.")
  #   # Optionally, you can check which slots have mismatches
  #   print(which(!same_order))
  # }
  
  #######make .Data########
  idx <- which(result$paramName == "Quantity")
  newData <- dcast(result[ idx,],
                   AnalysisName + sampleID ~ AnalyteName,
                   value.var = "paramValue")
  
  
  reference_sampleID <- obsDescr[[1]]$sampleID
  
  #ensure in same order as obsDescr
  newData_ordered <- newData[match(reference_sampleID, newData$sampleID), ]
  rownames(newData_ordered) <- seq_len(nrow(newData_ordered))
  
  if (all(newData_ordered$sampleID == reference_sampleID)) {
    cat("The sampleID in newData_ordered is in the same order as reference_sampleID.\n")
  } else {
    cat("The sampleID in newData_ordered does NOT match the order of reference_sampleID.\n")
    
    # Optionally, show mismatches
    mismatch_indices <- which(newData_ordered$sampleID != reference_sampleID)
    cat("Mismatches at rows:", mismatch_indices, "\n")
  }
  
  
  
  #####fix duplicated sampleIDs for all non samples (ltrs, qc, blanks etc)######
  for(i in seq_along(obsDescr)){
    
    df <- obsDescr[[i]]
    
    # Identify duplicated sampleIDs where sampleType is not "sample"
    idx <- which(duplicated(df$sampleID) & df$sampleType != "sample")
    if(length(idx) > 0 ){
      # Loop over the indices of duplicated sampleIDs
      for (id in idx) {
        # Extract the plate ID number from the plateID column (e.g., COVp021 -> 21)
        plate_id_number <- suppressWarnings(as.numeric(sub(".*?(\\d+)$", "\\1", df[id, "plateID"])))
        if(is.na(plate_id_number)){
          plate_id_number <- df[id, "plateID"]
        }
        # Append the plate ID number to the sampleID (e.g., "sampleID#21")
        df[id, "sampleID"] <- paste0(df[id, "sampleID"], "#", plate_id_number)
      }
      
    }
   
    #are there duplicates left
    idx <- which(duplicated(df[["sampleID"]]) | 
                   duplicated(df[["sampleID"]], fromLast = TRUE)) 
    
    if(length(idx) > 0){
      
      # Loop through each duplicated sampleID and modify it
      for (id in idx) {
        # Split AnalysisName by underscore
        parts <- strsplit(df$AnalysisName[id], "_")[[1]]
        end <- length(parts)
        num_part <- suppressWarnings(as.integer(parts[end]))
        if (is.na(num_part)) {
          # Append the numeric part to sampleID and break out of the loop
          df$sampleID[id] <- paste0(df$sampleID[id], ".", parts[end])
        }
        
      }
    }
    
    #are there duplicates left, print them
    idx <- which(duplicated(df[["sampleID"]]) | 
                   duplicated(df[["sampleID"]], fromLast = TRUE)) 
    if(length(idx) > 0){
      stop(paste0(unique(df[idx, "sampleID"]), "sampleIDs is duplicated, please fix"))
    } else{
      obsDescr[[i]]$sampleID <- df$sampleID
    }
  }

#drop sampleID and AnalysisName
newData_ordered <- newData_ordered[ , -which(names(newData_ordered) %in% c("AnalysisName","sampleID"))]
  
  ######make dae########
  da <- new("dataElement",
            .Data = newData_ordered,
            obsDescr = obsDescr,
            varName = unlist(colnames(newData_ordered)),
            type = "T-MS",
            method = "aminoAcids")


for (i in seq_along(da@obsDescr)) {
  da@obsDescr[[i]]$sampleID <- original_sampleIDs[[i]]
}

  return(da)
}
