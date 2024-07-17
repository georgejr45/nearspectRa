# function to extract the metadata from SVC

extract.metadata <- function(file, instrument = "SVC") {
  #read the content in the file
  lines <- readLines(file)

  # Initialize an empty dataframe to store the metadata
  metadata <- data.frame(Key = character(), Value = character(), stringsAsFactors = FALSE)
  #create an empty list to store the metadata
  metadata <- list()
  #loop through each line to extract data
  for (line in lines) {
    #skip empty line
    if (nchar(line) == 0 ) next
    #split the line to key and value
    parts <- strsplit(line, "=")[[1]]

    #check if split resulted in creating 2 parts
    if (length(parts) == 2) {
      key <- trimws(parts[1])
      value <- trimws(parts[2])

      # excluding the key called "data"
      if (key != "data") {
        # Add the key-value pair to the dataframe
        metadata <- rbind(metadata, data.frame(Key = key, Value = value, stringsAsFactors = FALSE))
      }
    }
  }
  return(metadata)
}


# example
#output <- extract.metadata("/Users/methungeorge/Desktop/IPB/playground/LILY_LEAF_i2145[86].sig", instrument = "SVC")
