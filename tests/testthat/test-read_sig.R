library(testthat)


# Source the file containing the read_sig function
#source("https://github.com/georgejr45/nearspectRa/blob/main/R/read.sig.R")
source("/Users/methungeorge/Desktop/nearspectRa/R/read.sig.R")

test_that("read.sig function works correctly", {
  # Test with a non-existent file
  expect_error(read.sig("non_existent_file.sig"), "file does not exist")

  # Create a temporary .sig file for testing
  temp_file <- tempfile(fileext = ".sig")
  writeLines(c(
    "metadata=example",
    "data=",
    "400 0.1 0.2 50",
    "500 0.3 0.4 60",
    "600 0.5 0.6 70"
  ), temp_file)

  # Read the temporary .sig file
  data <- read.sig(temp_file)

  # Check the structure of the data frame
  expect_equal(ncol(data), 4)
  expect_equal(colnames(data), c("Wavelengths(nm)", "Reference Values", "Target Values", "Reflectance(%)"))

  # Check the content of the data frame
  expect_equal(data[1, ], c(400, 0.1, 0.2, 50))
  expect_equal(data[2, ], c(500, 0.3, 0.4, 60))
  expect_equal(data[3, ], c(600, 0.5, 0.6, 70))

  # Clean up the temporary file
  unlink(temp_file)
})
