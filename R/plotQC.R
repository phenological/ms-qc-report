#'plotQC
#'
#'Plots sample types across plates, excluding blanks and internal standards.
#'Plates appear in the same order as in summaryTables(): the order provided
#'(see plateOrder argument), otherwise by Acquisition Date if every plate has
#'one, otherwise by name.
#' @param dae - dae of targeted Mass Spectrometry.
#' @param optns list of options
#' @param scale - Boolean. TRUE if the data should be log10(value + 1)
#' @param plateOrder NULL or character vector of order of plates desired. 
#' Default is NULL. Supply as c("COVp002", "COVp001", "COVp020") etc. Plates
#' not listed are added at the end.
#' @return list of plots with a slot for each analyte.
#'
#' @export
#' @import patchwork
#' @import ggplot2
#' @import grid
#' @importFrom reshape2 melt
#'

plotQC <- function(dae = NULL, scale = FALSE, plateOrder = NULL, optns = list()){
  
  QCLTRData <- summaryData(dae = dae)
  
  # one plate order for every plot, the same as summaryTables()
  ord <- plateOrderInfo(dae, plateOrder)
  note <- ord$note
  
  #don't need allPlates slot
  QCLTRData[["allPlates"]] <- NULL
  
  plots <- list()
  for(i in 1:length(dae@obsDescr)){
    df <- dae@obsDescr[[i]]
    
    # remove internal standards, before any renaming
    if(any(isInternalStandard(df$AnalyteName))) next
    
    # keep names whole, except BA: "CDCA-Chenodeoxycholic acid" -> "CDCA"
    df$AnalyteName <- cleanAnalyteName(df$AnalyteName, dae@method)
    if(dae@method=="BA"){
      if(!any(df$AnalyteName %in% BA_names)) next
    }
    
    # columns needed for the plot
    qCCols <- c(
      "plateID",
      "AnalysisName",
      "AnalyteName",
      "sampleType",
      "Area",
      "ISTD Area",
      "Quantity")
    
    #are required columns present
    missingCols <- setdiff(qCCols, names(df))
    if (length(missingCols)) {
      warning("No plot for ", df$AnalyteName[1], ": missing column(s) ",
              paste(missingCols, collapse = ", "), call. = FALSE)
      next
    }
    
    # Rename specific columns in the df
    colnames(df)[colnames(df) == "ISTD Area"] <- "AreaSIL"
    # colnames(df)[colnames(df) == "relative Area"] <- "Response"
    colnames(df)[colnames(df) == "Quantity"] <- "Conc"
    
    #make sure cols are numeric
    numCols <- c("Area",
                 "AreaSIL",
                 # "Response",
                 "Conc")
    
    df[numCols] <- lapply(df[numCols], function(x) suppressWarnings(as.numeric(x)))
    
    #change NA to 0
    cols <- c("Area", 
              "AreaSIL", 
              # "Response", 
              "Conc")
    
    df[cols] <- lapply(df[cols], function(x) {
      x[is.na(x)] <- 0
      return(x)
    })
    
    #if -rerun instead of _rerun
    if (any(grepl("-rerun", df$AnalysisName, ignore.case = TRUE))) {
      df$AnalysisName <- gsub(pattern = "-rerun", replacement = "_rerun", x = df$AnalysisName,
                              ignore.case = FALSE)
    }
    
    # #remove rerun
    # if (any(grepl("-rerun", df$AnalysisName, ignore.case = TRUE))) {
    #   result$AnalysisName <- gsub(pattern = "-rerun[1-6]", replacement = "", x = result$AnalysisName,
    #                               ignore.case = FALSE)
    # }
    # 
    
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
    
    #sometimes the AnalysisName doesn't have the injection counter included, in older cases where the runID is a number isntead of COVr40 etc, it'll use that instead which ruins the plot.
    # Put those injections after the real ones, each with its own number.
    idx <- which(is.na(df$plateExperimentID) | df$plateExperimentID > 200)
    
    if(length(idx) > 0){
      good <- df$plateExperimentID[!is.na(df$plateExperimentID) & df$plateExperimentID <= 200]
      df$plateExperimentID[idx] <- (if (length(good)) max(good) else 0) + seq_along(idx)
    }
    
    #set colors
    plotFill = c(
      "sample" = "cornflowerblue",
      "ltr" = "darkorange",
      "sltr" = "purple3",
      "vltr" = "gold",
      "cal" = "black",
      "qc" = "black",
      "pqc" ="lightgreen"
    )
    
    plotColor = c(
      "sample" = "cornflowerblue",
      "ltr" = "black",
      "sltr" = "black",
      "vltr" = "black",
      "cal" = "black",
      "qc" = "black",
      "pqc" = "black"
    )
    
    plotShape = c(
      "sample" = 4,
      "ltr" = 24,
      "sltr" = 25,
      "vltr" = 23,
      "cal" = 1,
      "qc" = 4,
      "pqc" = 24
    )
    
    # any other sample type is drawn in grey rather than dropped
    other <- setdiff(unique(df$sampleType), c(names(plotShape), "blank"))
    plotFill[other] <- "grey60"
    plotColor[other] <- "grey40"
    plotShape[other] <- 3
    
    #remove blanks
    idx <- which(df$sampleType == "blank")
    
    if(length(idx) > 0){
      df <- df[-idx, ]
    }
    
    #pass/fail information
    
    # (stays NA if this analyte has no QC result on a plate)
    df$PassFail <- NA
    for(plate in unique(df$plateID)){
      plate_result <- QCLTRData[[plate]]
      value <- plate_result$PassFail[plate_result$AnalyteName == df$AnalyteName[1]]
      if (length(value)) df[which(df$plateID == plate), "PassFail"] <- value[1]
    }
    ######Plate Order#######
    df$plateID <- factor(df$plateID, levels = ord$order)
    
    #arrange in plate and injection order
    df <- df[order(df$plateID, df$plateExperimentID), ]
    
    #transform to long
    y_columns <- c("Area", 
                   "AreaSIL", 
                   # "Response", 
                   "Conc")
    df_long <- melt(df, id.vars = c("plateExperimentID", "sampleType", "plateID", "PassFail"), measure.vars = y_columns)
    
    #possible log10(value + 1) scaling
    if(scale == TRUE){
      df_long$value <- log10(df_long$value + 1)
      ylab <- "log10(value + 1)"
    } else {ylab <- "value"}
    
    # Create the plot with log10 transformation
    plot <-
      ggplot(df_long, aes(x = plateExperimentID,
                          y = value,
                          color = sampleType,
                          shape = sampleType,
                          fill = sampleType)) +
      geom_rect(data = df_long[!duplicated(df_long[c("plateID", "variable")]) & df_long$PassFail %in% "CAUTION", ],
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
                 scales = "free_y", switch = "both", drop = FALSE)  +  # Facet by column names
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
                 strip.position = "right",
                 scales = "free_y") + # different y-axis scales for each variable
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
