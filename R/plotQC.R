#'plotQC
#'
#'Plots sample types across plates, excluding blanks. If Acquisition Date is 
#'available and there are no NAs, plates appear in date order, otherwise in name
#'order or order provided (see plateOrder argument).
#' @param dae - dae of targeted Mass Spectrometry.
#' @param optns list of options
#' @param scale - Boolean. TRUE if the data should be log10(value + 1)
#' @param plateOrder NULL or character vector of order of plates desired. 
#' Default is NULL. Supply as c("COVp002", "COVp001", "COVp020") etc.
#' @return list of plots with a slot for each analyte.
#'
#' @export
#' @import patchwork
#' @import ggplot2
#' @import ggpubr
#' @import grid
#' @importFrom reshape2 melt
#'

plotQC <- function(dae = NULL, scale = TRUE, plateOrder = NULL, optns = list()){
  
  QCLTRData <- summaryData(dae = dae)
  
  #don't need allPlates slot
  QCLTRData[["allPlates"]] <- NULL
  
  plots <- list()
  for(i in 1:length(dae@obsDescr)){
    df <- dae@obsDescr[[i]]
    #remove IS
    if(any(grepl("\\[IS\\]", df$AnalyteName))) next
    
    # columns
    qCCols <- c(
      "plateID",
      "AnalysisName",
      "AnalyteName",
      "Area",
      "ISTD Area",
      "relative Area",
      "Quantity",
      "Expected Quantity",
      "Internal standard used for calibration function calculation")
    
    #are required columns present
    qCCols %in% names(df)
    
    # Rename specific columns in the df
    colnames(df)[colnames(df) == "ISTD Area"] <- "AreaSIL"
    colnames(df)[colnames(df) == "relative Area"] <- "Response"
    colnames(df)[colnames(df) == "Quantity"] <- "Conc"
    
    #make sure cols are numeric
    numCols <- c("Area",
                 "AreaSIL",
                 "Response",
                 "Conc",
                 "Expected Quantity")
    
    df[numCols] <- lapply(df[numCols], as.numeric)
    
    #change NA to 0
    cols <- c("Area", "AreaSIL", "Response", "Conc")
    
    df[cols] <- lapply(df[cols], function(x) {
      x[is.na(x)] <- 0
      return(x)
    })
    
    # Extract numeric part from plateID (removes "COVp")
    df$plateID_numeric <- as.numeric(sub(".*?(\\d+)$", "\\1", df$plateID))
    
    #add column (auto pick up the counter and deal with any rerun, rederiv etc att he end of analysisname)
    # df$plateExperimentID <- sapply(strsplit(df$AnalysisName, "_"), function(parts) {
    #   idx <- length(parts)
    #   
    #   # Suppress warnings only for the integer conversion attempt
    #   last_element <- suppressWarnings(as.integer(parts[idx]))
    #   
    #   if (is.na(last_element)) {
    #     # Try the second last element if the last one isn't numeric
    #     second_last_element <- suppressWarnings(as.integer(parts[idx - 1]))
    #     return(second_last_element)
    #   } else {
    #     return(last_element)
    #   }
    # })
    
    #if -rerun instead of _rerun
    if (any(grepl("-rerun", df$AnalysisName, ignore.case = TRUE))) {
      result$AnalysisName <- gsub(pattern = "-rerun", replacement = "_rerun", x = result$AnalysisName,
                                  ignore.case = FALSE)
    }
    
    df$plateExperimentID <- sapply(strsplit(df$AnalysisName, "_"), function(parts) {
      idx <- length(parts)
      
      # Loop backwards through the parts until we find a numeric value
      while (idx > 0) {
        # Suppress warnings during the integer conversion attempt
        num <- suppressWarnings(as.integer(parts[idx]))
        
        if (!is.na(num)) {
          return(num)
        }
        
        # Move to the previous part if current part is not numeric
        idx <- idx - 1
      }
      
      # Return NA if no numeric value is found
      return(NA)
    })
    
    # Create the runIndex by combining plateID numeric part and plateExperimentID with leading zeros
    df$runIndex <- as.numeric(sprintf("%d.%03d", df$plateID_numeric, df$plateExperimentID))
    
    #arrange in plate and count id
    df <- df[order(df$runIndex), ]
    
    #set colors
    plotFill = c(
      "sample" = "cornflowerblue",
      "ltr" = "darkorange",
      "sltr" = "purple3",
      "cal" = "black",
      "qc" = "black"
    )
    
    plotColor = c(
      "sample" = "cornflowerblue",
      "ltr" = "black",
      "sltr" = "black",
      "cal" = "black",
      "qc" = "black"
    )
    
    plotShape = c(
      "sample" = 4,
      "ltr" = 24,
      "sltr" = 25,
      "cal" = 1,
      "qc" = 4
    )
    
    #remove blanks
    idx <- which(df$sampleType == "blank")
    
    if(length(idx>0)){
      df <- df[-idx, ]
    }
    
    #pass/fail information

    df$PassFail <- NA
    for(plate in unique(df$plateID)){
      df[which(df$plateID == plate), "PassFail"] <- QCLTRData[[plate]][which(QCLTRData[[plate]]$AnalyteName == unique(df$AnalyteName)), "PassFail"]
    }
    ######Plate Order#######
    
    if("Acquisition Date" %in% names(df) && all(!is.na(df$`Acquisition Date`)) & is.null(plateOrder)){
      #factor plates
      plate_min_times <- aggregate(`Acquisition Date` ~ plateID, data = df, min)
      
      # Order plateIDs by their minimum Acquisition Time
      ordered_plateIDs <- plate_min_times$plateID[order(plate_min_times$`Acquisition Date`)]
      
      # Factorize plateID in df based on the sorted order
      df$plateID <- factor(df$plateID, levels = ordered_plateIDs)
      note <- "Plates automatically ordered by Acquisition Date"
    }
    
    if((any(is.na(df$`Acquisition Date`)) | !("Acquisition Date" %in% names(df))) && is.null(plateOrder)){
      # print(paste0("No Acquisition Date present or it contains NAs, please provide plateOrder, else plate name is used for ", names(dae@obsDescr)[[i]]))
      note <- "Plates ordered by name (No Acquisition Date or contains NAs, may provide plateOrder)"
    }
    
    if(!is.null(plateOrder)){
      df$plateID <- factor(df$plateID, levels = plateOrder)
      note <- "Plates in order manually provided"
    }
    #transform to long
    y_columns <- c("Area", "AreaSIL", "Response", "Conc")
    df_long <- melt(df, id.vars = c("plateExperimentID", "sampleType", "plateID", "PassFail"), measure.vars = y_columns)
    df_long$fill_color <- ifelse(df_long$PassFail == "CAUTION", "red", NA)
    
    #possible log10(value + 1) scaling
    if(scale == TRUE){
      df_long$value <- log10(df_long$value + 1)
      ylab <- "log10(value + 1)"
    } else {ylab <- ""}
    
    # Create the plot with log10 transformation
    plot <-
      ggplot(df_long, aes(x = plateExperimentID,
                          y = value,
                          color = sampleType,
                          shape = sampleType,
                          fill = sampleType)) +
      geom_rect(data = subset(df_long[!duplicated(df_long[c("plateID", "variable")]) & df_long$PassFail == "CAUTION", ]),
                fill = "red", 
                xmin = -Inf, 
                xmax = Inf, 
                ymin = -Inf, 
                ymax = Inf, 
                alpha = 0.3, 
                inherit.aes = FALSE) +
      geom_point(data = subset(df_long, sampleType != "ltr")) +  # Plot all other sampleTypes first
      geom_point(data = subset(df_long, sampleType == "ltr")) + 
      facet_grid(rows = vars(variable),
                 cols = vars(plateID),
                 scales = "free_y", switch = "both")  +  # Facet by column names
      scale_x_continuous(labels = NULL) +
      theme_minimal() +
      scale_fill_manual(values = plotFill, na.value = "transparent") +
      scale_shape_manual(values = plotShape) +
      scale_color_manual(values = plotColor) +
      xlab("Sample Index") +
      ylab(paste0(ylab)) +
      theme(plot.caption = element_text(hjust = 0),
            strip.text.y.left = element_text(angle = 90),  # Rotate y-axis facet labels
            strip.background = element_blank(),  # Optional: remove background from facet labels
            axis.title.y.left = element_text(),
            strip.placement = "outside",
            panel.spacing.x = unit(0, "lines"),
            panel.spacing.y = unit(0, "lines"), 
            legend.position = "bottom", 
            legend.justification = "left",
            panel.background = element_rect(fill = "white", color = NA)) +
      # Add vertical dashed lines manually
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "dashed", 
                                                        col = "black")), 
                        xmin = -Inf, 
                        xmax = -Inf) +
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "dashed", 
                                                        col = "black")), 
                        xmin = Inf, 
                        xmax = Inf) +
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "solid", 
                                                        col = "black")), 
                        xmin = -Inf, 
                        xmax = Inf, 
                        ymin = -Inf, 
                        ymax = -Inf) +  # Horizontal bottom line
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "solid", 
                                                        col = "black")), 
                        xmin = -Inf, 
                        xmax = Inf, 
                        ymin = Inf, 
                        ymax = Inf) +
      ggtitle(unique(df$AnalyteName))+
      labs(caption = paste0(note))
    
    #make boxPLot
    box <-
      ggplot(df_long, aes(x = sampleType,
                          y = value,
                          color = sampleType,
                          shape = sampleType)) +
      facet_wrap(~variable, 
                 ncol = 1, 
                 strip.position = "right") +
      geom_boxplot() +
      theme_bw() +
      theme(axis.line = element_line(color='black'),
            plot.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x=element_blank(), 
            panel.spacing.y = unit(0, "lines")) +
      xlab("") +
      ylab(paste0("")) +
      theme(legend.position = "none") +
      scale_color_manual(values = plotFill) +
      scale_shape_manual(values = plotShape) + 
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "solid", col = "black")), 
                        xmin = -Inf, 
                        xmax = Inf, 
                        ymin = -Inf, 
                        ymax = -Inf) +  # Horizontal bottom line
      annotation_custom(grid::linesGrob(y = c(0, 1), 
                                        gp = grid::gpar(lty = "solid", col = "black")), 
                        xmin = -Inf, 
                        xmax = Inf, 
                        ymin = Inf, 
                        ymax = Inf) 
    
    QCPlot <- plot + box + plot_layout(widths = c(1, 0.2))
    
    # Append the valid plot to the list
    plots[[length(plots) + 1]] <- QCPlot
    names(plots)[length(plots)] <- unique(df$AnalyteName)
  }
  
  return(plots)
}

