#function to read the .sig files
read_sig <- function(file.path) {
  if(!file.exists(file_path)) {
    stop("file does not exist: ", file_path)
  }
  #read file contents
  file_content <- readLines(file_path)
  #find start of data
  data_start <- grep("^data=", file_content)
  #extract data lines
  data_lines <- file_content[(data_start + 1):length(file_content)]
  # convert it to data frame
  data <- read.table(text = data_lines, header = FALSE)
  # give names to columns
  colnames(data) <- c("Wavelengths(nm)", "Reference Values", "Target Values", "Reflectance(%)")
  #return the data frame
  return(data)
}

#example data
file_path <- "/Users/methungeorge/Desktop/IPB/playground/LILY_LEAF_i2145[86].sig"
data <- read_sig(file_path)
print(head(data))

