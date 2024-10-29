# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })
# load("~/Documents/LucyDatasets/bio.daE")
# test <- summaryQC(dae = bio)
# View(test[["COVp20"]])
# 
# #specific scenarios
# #LTR RSD because all LTR quantity was 0, still a fail/caution
# data <- as.data.frame(apply(bio@.Data, 2, as.numeric))
# eg <- cbind(data$`gamma aminobutyric acid`, bio@obsDescr[["gamma aminobutyric acid"]])
# 
# View(eg[which(eg$sampleType == "sample" & eg$plateID == "COVp27"),])
# 
# eg <- cbind(data$Sarcosine, bio@obsDescr[["Sarcosine"]])
# 
# View(eg[which(eg$sampleType == "ltr" & eg$plateID == "COVp27"),])
# 
# eg <- cbind(data$Cystine, bio@obsDescr[["Cystine"]])
# 
# View(eg[which(eg$sampleType == "qc" & eg$plateID == "COVp27"),])
