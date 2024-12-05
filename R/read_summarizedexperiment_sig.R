#' Read .sig Files and Create SummarizedExperiment Object
#'
#' This function reads .sig files from a specified directory or a file path, extracts wavelength and reflectance data,
#' and combines them into a \code{SummarizedExperiment} object. Each .sig file is expected to contain reflectance data
#' and a sample name in its content.
#'
#' @name read_summarizedexperiment_sig
#' @title Read the SIG files and return as SummarizedExperiment object.
#'
#' @param path A character vector of file paths or a directory path. If a directory path is provided, all .sig files
#'   within the directory are read. If a file path is provided, only that file is read.
#'
#' @return A \code{SummarizedExperiment} object where:
#' \item{assays}{A matrix of reflectance data, with wavelengths as row names and sample names as column names.}
#' \item{colData}{A \code{DataFrame} with sample names.}
#' \item{rowData}{A \code{DataFrame} with wavelengths.}
#'
#' @details
#' Each .sig file is read line by line. The function expects each file to contain:
#' \itemize{
#'   \item A line starting with "name=" indicating the sample name.
#'   \item A section starting with "data=" followed by tabular data where the first column is wavelengths and the fourth
#'   column is reflectance.
#' }
#' The combined data from all files is merged by wavelengths. Missing data is handled by filling with NA.
#'
#' @examples
#' # Example usage:
#' path <- "/path/to/sig/files"
#' se <- read_summarizedexperiment_asd(path)
#' print(se)
#'
#' @import SummarizedExperiment
#' @author Methun George, Dr.Steffen Neumann
#'
#' @export

library(SummarizedExperiment)

# Define the function
read_summarizedexperiment_sig <- function(path) {
  # Check if the input path is a directory or a single file
  path_info <- file.info(path)
  if (path_info$isdir) {
    sig_files <- list.files(path = path, pattern = "\\.sig$", full.names = TRUE)
  } else {
    sig_files <- path
  }

  # Create an empty list to store data frames and sample names
  all_data <- list()
  sample_names <- c()

  # Read each file and extract data
  for (file in sig_files) {
    file_content <- readLines(file)

    # Extract sample name
    name_line <- grep("^name=", file_content, value = TRUE)
    if (length(name_line) > 0) {
      sample_name <- sub("^name=", "", name_line)
    } else {
      sample_name <- basename(file)
    }
    sample_names <- c(sample_names, sample_name)

    # Extract data (reflectance %)
    data_start <- grep("^data=", file_content)
    data_lines <- file_content[(data_start + 1):length(file_content)]
    data <- read.table(text = data_lines, header = FALSE, fill = TRUE)

    # Select columns for wavelengths and reflectance
    data <- data[, c(1, 4)]
    colnames(data) <- c("Wavelengths(nm)", sample_name)

    # Add the data frame to the list
    all_data[[sample_name]] <- data
  }

  # Combine all data frames by wavelengths
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Wavelengths(nm)", all = TRUE), all_data)

  # Set row names to wavelengths and remove the "Wavelengths(nm)" column
  rownames(combined_data) <- combined_data$`Wavelengths(nm)`
  combined_data <- combined_data[, -1, drop = FALSE]

  # Create colData
  colData <- DataFrame(sampleNames = colnames(combined_data))

  # Create rowData
  rowData <- DataFrame(Wavelengths = rownames(combined_data))

  # Create the SummarizedExperiment object
  se <- SummarizedExperiment(assays = list(counts = as.matrix(combined_data)),
                             colData = colData,
                             rowData = rowData)

  return(se)
}

# Example usage
path <- "/Users/methungeorge/Desktop/IPB/test/sig_data"
se <- read_summarizedexperiment_sig(path)

# Check the SummarizedExperiment object
print(se)
# View(assay(se, "counts"))
