
test_that("file of TSV to dae works",{
  tsvTest <- ms.qc.report:::makeAAdae(result = NULL, file = "~/git/phenological/ms-qc-report/inst/extdata/")
  expect_true(object = is(tsvTest)[1] == "dataElement")
  
  #Acquisition date there for some TSV not all
  ##the column is still there
  expect_true(object = "Acquisition Date" %in% colnames(tsvTest@obsDescr[[1]]))
  
  ##the column contains NAs
  expect_contains(object = tsvTest@obsDescr[[1]][,"Acquisition Date"], expected = NA)
  
  ##should be POSIXct
  expect_contains(object = is(tsvTest@obsDescr[[1]][,"Acquisition Date"])[1], expected = "POSIXct")
  
})

test_that("single TSV to dae works",{
  tsvTest <- ms.qc.report:::makeAAdae(result = NULL, file = "~/git/phenological/ms-qc-report/inst/extdata/PLA1.TSV")
  expect_true(object = is(tsvTest)[1] == "dataElement")
  
  expect_false(object = "Acquisition Date" %in% colnames(tsvTest@obsDescr[[1]]))
})