library(testthat)
library(SummarizedExperiment)

# Define the path to the test data directory
test_data_dir <- file.path("/Users/methungeorge/Desktop/nearspectRa/tests/asd_data")

test_that("SummarizedExperiment contains correct colData and rowData", {
  result <- read_summarizedexperiment_asd(test_data_dir)

  print("colData columns:")
  print(colnames(colData(result)))

  print("rowData columns:")
  print(colnames(rowData(result)))

  expect_true(nrow(colData(result)) > 0)
  expect_true(nrow(rowData(result)) > 0)

  expect_true(any(grepl("sample", colnames(colData(result)))))
  expect_true(any(grepl("Wavelength", colnames(rowData(result)))))
})
