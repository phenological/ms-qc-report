#'plotQC
#'
#'Plots sample types across plates, excluding blanks.
#' @param dae - dae of targeted Mass Spectrometry.
#' @param optns list of options
#' \itemize{
#'    \item scale - Boolean. TRUE if the data should be log10(value + 1)
#' }
#' @return list of plots with a slot for each analyte.
#'
#' @export
#' @import patchwork
#' @import ggplot2
#' @import ggpubr
#' @import grid
#' @importFrom reshape2 melt
#'

plotQC <- function(dae, scale = TRUE, optns = list()){
  
  plots <- list()
  for(i in 1:length(dae@obsDescr)){
    df <- bio@obsDescr[[i]]
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
                 "AreaSil",
                 "Response",
                 "Conc",
                 "Expected Quantity")
    
    df[numCols] <- lapply(df[numCols], as.numeric)
    
    #change NA to 0
    cols <- c("Area", "AreaSil", "Response", "Conc")
    
    df[cols] <- lapply(df[cols], function(x) {
      x[is.na(x)] <- 0
      return(x)
    })
    
    # Extract numeric part from plateID (removes "COVp")
    df$plateID_numeric <- as.numeric(sub(".*?(\\d+)$", "\\1", df$plateID))
    
    #add column
    df$plateExperimentID <- as.integer(gsub(".*_","", sub("_rerun", "", df$AnalysisName)))
    
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
    df <- df[-idx, ]
    
    #transform to long
    y_columns <- c("Area", "AreaSil", "Response", "Conc")
    df_long <- melt(df, id.vars = c("plateExperimentID", "sampleType", "plateID"), measure.vars = y_columns)
    
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
      geom_point() +
      facet_grid(rows = vars(variable),
                 cols = vars(plateID),
                 scales = "free_y", switch = "both")  +  # Facet by column names
      scale_x_continuous(labels = NULL) +
      theme_minimal() +
      scale_fill_manual(values = plotFill) +
      scale_shape_manual(values = plotShape) +
      scale_color_manual(values = plotColor) +
      xlab("Sample Index") +
      ylab(paste0(ylab)) +
      theme(strip.text.y.left = element_text(angle = 90),  # Rotate y-axis facet labels
            strip.background = element_blank(),  # Optional: remove background from facet labels
            axis.title.y.left = element_text(),
            strip.placement = "outside",
            panel.spacing.x = unit(0, "lines"),
            panel.spacing.y = unit(0, "lines"), 
            legend.position = "bottom") +
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
      ggtitle(unique(df$AnalyteName))
    
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
