

test_that(" works", {

  testTsv <- fusion:::parseMS(folder = "~/git/phenological/ms-qc-report/inst/extdata/", assay = "AA", fileType = "tsv")
  test <- ms.qc.report:::summaryData(dae = testTsv)

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



