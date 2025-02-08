library(testthat)
library(SummarizedExperiment)

# Define a temporary directory for testing
setup_test_data <- function() {
  test_dir <- tempdir()
  file.create(file.path(test_dir, "test1.asd"))
  file.create(file.path(test_dir, "test2.asd"))
  return(test_dir)
}

test_that("Function returns a SummarizedExperiment object", {
  test_dir <- setup_test_data()
  result <- read_summarizedexperiment_asd(test_dir)
  expect_s4_class(result, "SummarizedExperiment")
})

test_that("SummarizedExperiment object contains assay data", {
  test_dir <- setup_test_data()
  result <- read_summarizedexperiment_asd(test_dir)
  expect_true("counts" %in% assayNames(result))
  expect_true(nrow(assay(result)) > 0)
  expect_true(ncol(assay(result)) > 0)
})

test_that("SummarizedExperiment contains colData and rowData", {
  test_dir <- setup_test_data()
  result <- read_summarizedexperiment_asd(test_dir)
  expect_true(nrow(colData(result)) > 0)
  expect_true(nrow(rowData(result)) > 0)
})

test_that("Function handles a single file input", {
  test_dir <- setup_test_data()
  single_file <- file.path(test_dir, "test1.asd")
  result <- read_summarizedexperiment_asd(single_file)
  expect_s4_class(result, "SummarizedExperiment")
})
