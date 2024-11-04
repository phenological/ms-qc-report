
test_that("file of TSV to dae works",{
  tsvTest <- makeAAdae(result = NULL, file = "~/git/phenological/ms-qc-report/inst/extdata/")
  expect_true(object = is(tsvTest)[1] == "dataElement")
})

test_that("single TSV to dae works",{
  tsvTest <- makeAAdae(result = NULL, file = "~/git/phenological/ms-qc-report/inst/extdata/PLA1.TSV")
  expect_true(object = is(tsvTest)[1] == "dataElement")
})