
test_that("order observed for tables", {
  #make Acquisition dates for NAs
  testTsv <- fusion:::parseMS(folder = "~/git/phenological/ms-qc-report/inst/extdata/", assay = "AA", fileType = "tsv")

  AutoName <- summaryTables(dae = testTsv)
  #same order
  expect_true(object = (names(AutoName$data)[1] == names(AutoName$tables)[1]))

  Manual <- summaryTables(dae = testTsv,
                          plateOrder = c("PLASMA8B", "JUVp003"))
  expect_true(names(Manual$data)[2] == names(Manual$tables)[2])
  expect_true(names(Manual$data)[2] == "JUVp003")

  #fill the NAs
  for (i in 1:length(testTsv@obsDescr)) {
    df <- testTsv@obsDescr[[i]]

    # Ensure 'Acquisition Date' exists and has values
    if ("Acquisition Date" %in% names(df)) {

      # Find the first non-NA date as the starting point
      start_time <- max(df$`Acquisition Date`, na.rm = TRUE)

      # Fill NAs with increasing time by one minute
      if (!is.na(start_time)) {
        na_indices <- which(is.na(df$`Acquisition Date`))
        df$`Acquisition Date`[na_indices] <- start_time + (seq_along(na_indices) - 1) * 60
      }
    }

    # Update the slot with modified data
    testTsv@obsDescr[[i]] <- df
  }

  ####date order#######
  AutoDate <- summaryTables(dae = testTsv)

  expect_true(names(AutoDate$data)[1] == names(AutoDate$tables)[1])
  expect_true(names(AutoDate$data)[1] == "JUVp003")

  #####manual order########
  Manual <- summaryTables(dae = testTsv,
                          plateOrder = c("PLASMA8B", "JUVp003" ))

  expect_true(names(Manual$data)[2] == names(Manual$tables)[2])
  expect_true(names(Manual$data)[2] == "JUVp003")
})

