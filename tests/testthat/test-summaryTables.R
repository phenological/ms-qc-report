
test_that("single file works", {
  test_path <- system.file("extdata", "PLA1.TSV", package = "ms.qc.report")
  expect_true(file.exists(test_path), info = paste("File path:", file_path))
  
  test <- ms.qc.report:::summaryTables(file = test_path)
  
  expect_contains(object = names(test$tables), 
                  expected = "allPlates")
})

test_that("single file works", {
  
  
  test <- ms.qc.report:::summaryTables(file = "~/git/phenological/ms-qc-report/inst/extdata/")
  
  expect_contains(object = names(test$tables), 
                  expected = "allPlates")
})