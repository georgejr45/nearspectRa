library(testthat)
library(SummarizedExperiment)

# Function to Create Temporary Test Data
setup_test_sig_data <- function() {
  test_dir <- tempfile("test_sig_data")
  dir.create(test_dir)

  # Create dummy .sig files with minimal content
  writeLines(c(
    "name=Sample1",
    "data=",
    "400  0  0  0.12",
    "500  0  0  0.23",
    "600  0  0  0.34"
  ), file.path(test_dir, "test1.sig"))

  writeLines(c(
    "name=Sample2",
    "data=",
    "400  0  0  0.45",
    "500  0  0  0.56",
    "600  0  0  0.67"
  ), file.path(test_dir, "test2.sig"))

  return(test_dir)
}

#  Unit Tests
test_that("Function returns a SummarizedExperiment object", {
  test_dir <- setup_test_sig_data()
  result <- read_summarizedexperiment_sig(test_dir)
  expect_s4_class(result, "SummarizedExperiment")
})

test_that("SummarizedExperiment contains assay data", {
  test_dir <- setup_test_sig_data()
  result <- read_summarizedexperiment_sig(test_dir)
  expect_true("counts" %in% assayNames(result))
  expect_true(nrow(assay(result)) > 0)  # Check if rows exist (wavelengths)
  expect_true(ncol(assay(result)) > 0)  # Check if columns exist (samples)
})

test_that("SummarizedExperiment contains colData and rowData", {
  test_dir <- setup_test_sig_data()
  result <- read_summarizedexperiment_sig(test_dir)
  expect_true(nrow(colData(result)) > 0)  # Ensure sample metadata exists
  expect_true(nrow(rowData(result)) > 0)  # Ensure wavelength metadata exists
})

test_that("Correct sample names are extracted", {
  test_dir <- setup_test_sig_data()
  result <- read_summarizedexperiment_sig(test_dir)
  expect_true("Sample1" %in% colnames(assay(result)))
  expect_true("Sample2" %in% colnames(assay(result)))
})

test_that("Function handles a single .sig file input", {
  test_dir <- setup_test_sig_data()
  single_file <- file.path(test_dir, "test1.sig")
  result <- read_summarizedexperiment_sig(single_file)
  expect_s4_class(result, "SummarizedExperiment")
  expect_true("Sample1" %in% colnames(assay(result)))
})

