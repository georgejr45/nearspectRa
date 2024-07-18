library(testthat)

# Source the file containing the read_sig function
source("/Users/methungeorge/Desktop/nearspectRa/R/extract.metadata.R")


  #create temporary file for testing
  temp_file <- tempfile(fileext = ".sig")
  writeLines(c(
    "key1 = value1",
    "key2 = value2",
    "data = some_data",
    "key3 = value3",
    "key4 = value4"
  ), temp_file)

 #define the test
 test_that("extract.metadata function works properly", {
   result <- extract.metadata(temp_file, instrument = "SVC")

   expected <- data.frame(
     Key = c("key1", "key2", "key3", "key4"),
     Value = c("value1", "value2", "value3", "value4"),
     stringsAsFactors = FALSE
   )

   expect_equal(result, expected)
   })


