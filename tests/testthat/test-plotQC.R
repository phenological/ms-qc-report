
# test_that("date order works with TSVs", {
#   
# #make Acquisition dates for NAs 
#   testTsv <- ms.qc.report:::makeAAdae(result = NULL, file = "~/git/phenological/ms-qc-report/inst/extdata/")
# 
# ###name order#####
# plotsAutoName <- 
#   plotQC(dae = testTsv,
#          scale = TRUE,
#          optns = list())
# 
# expect_true(object = is.null(levels(plotsAutoName[["1-methylhistidine"]][[1]][["data"]]$plateID)))
# 
# #fill the NAs
# for (i in 1:length(testTsv@obsDescr)) {
#   df <- testTsv@obsDescr[[i]]
#   
#   # Ensure 'Acquisition Date' exists and has values
#   if ("Acquisition Date" %in% names(df)) {
#     
#     # Find the first non-NA date as the starting point
#     start_time <- max(df$`Acquisition Date`, na.rm = TRUE)
#     
#     # Fill NAs with increasing time by one minute
#     if (!is.na(start_time)) {
#       na_indices <- which(is.na(df$`Acquisition Date`))
#       df$`Acquisition Date`[na_indices] <- start_time + (seq_along(na_indices) - 1) * 60
#     }
#   }
#   
#   # Update the slot with modified data
#   testTsv@obsDescr[[i]] <- df
# }
# 
# ####date order#######
# plotsAutoDate <- 
#   plotQC(dae = testTsv,
#          scale = TRUE,
#          plateOrder = NULL,
#          optns = list())
# 
# #homocysteine has some NAs so should work but orders plates by names
# expect_true(object = is.null( levels(plotsAutoDate[["Homocysteine"]][[1]][["data"]][["plateID"]])))
# 
# #for those with no NAs in dates, JUV plate should be first (was run first)
# expect_true(levels(plotsAutoDate[["3-methylhistidine"]][[1]][["data"]][["plateID"]])[1] == "JUVp003")
# 
# 
# #####manual order########
# plotsManual<- 
# plotQC(dae = testTsv,
#        scale = TRUE ,
#        plateOrder = c("COVp20", "JUVp003",  "COVp22",  "COVp23",  "COVp24",  "COVp25",  "COVp26" ),
#        optns = list())
# 
# #JUV should be second now
# expect_true(levels(plotsManual[["Alanine"]][[1]][["data"]][["plateID"]])[2] == "JUVp003")
# 
# })
