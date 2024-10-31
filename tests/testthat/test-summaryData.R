

test_that("single TSV file works", {
  test_path <- system.file("extdata", "PLA1.TSV", package = "ms.qc.report")
  expect_true(file.exists(test_path), info = paste("File path:", file_path))
  
  test <- ms.qc.report:::summaryData(file = test_path)
  expect_contains(object = names(test[[1]]), 
                  expected = c("AnalyteName", 
                               "totalQCInjectionPass", 
                               "totalQCPassPerc", 
                               "LTR_RSD", 
                               "R2", 
                               "PassFail"))
  
  expect_length(object = test, 
                n = 3)
  
  expect_contains(object = names(test), 
                  expected = "allPlates")
  
})

test_that("folder with multiple TSV files works", {
  test <- ms.qc.report:::summaryData(file = "~/git/phenological/ms-qc-report/inst/extdata/")

  expect_contains(object = names(test[[1]]), 
                  expected = c("AnalyteName", 
                               "totalQCInjectionPass", 
                               "totalQCPassPerc", 
                               "LTR_RSD", 
                               "R2", 
                               "PassFail"))
  
  expect_length(object = test, 
                n = 7)
  
  expect_contains(object = names(test), 
                  expected = "allPlates")
})



