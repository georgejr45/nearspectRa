#' Read ASD Spectral Files and Create a SummarizedExperiment Object
#'
#' This function will read a single or multiple ASD files and extracts the spectral data
#' and associated metadata, combines them into a "SummarizedExperiment" object. This object
#' contains the spectral data as an assay matrix and metadata about the samples and wavelengths.
#'
#' @name read_summarizedexperiment_asd
#' @title Read the ASD files and return as SummarizedExperiment object.
#'
#' @param path A character string specifying the path to a directory containing the ".asd" files or a single
#'             ".asd" file.
#'
#' @return A `SummarizedExperiment` object containing:
#'   \itemize{
#'     \item{\code{assays}}{A list with a single assay matrix named \code{counts},
#'       where rows represent wavelengths and columns represent different samples.}
#'     \item{\code{colData}}{A `DataFrame` with sample metadata, including the sample names.}
#'     \item{\code{rowData}}{A `DataFrame` with wavelength information.}
#'   }
#'
#' @details
#' This function uses 2 functions such as "read.asd" and "extract.metadata" from the package "FieldSpectra"
#' to read the data and metadata from the given files. This function will then uses the data from the files
#' and make a SummarizedExperiment object out of it which can be used for further data analysis.
#'
#' @import FieldSpectra
#' @import SummarizedExperiment
#'
#'
#' @author Methun George, Dr.Steffen Neumann
#'
#' @examples
#' # path <- "path/to/your/asd/files"
#' # se <- read_asd_summar(path)
#' # se
#' # colnames(se)
#' # rownames(se)
#'
#' @export


library(FieldSpectra)
library(SummarizedExperiment)


  read_summarizedexperiment_asd <- function(path) {
    if (length(path) == 1 && file.info(path)$isdir) {
      asd_files <- list.files(path = path, pattern = "\\.asd$", full.names = TRUE)
    } else {
      asd_files <- path
    }


  # Initialize lists to store the data frame and metadata
  asd_data_list <- list()
  asd_metadata_list <- list()
  assay_data <- list()

  # Loop over each file and read it (in case of directory)
  for (file in asd_files) {
    asd_data <- read.asd(file.dir = file, out.dir = '~')
    wavelengths <- asd_data$Wavelength
    spectra <- asd_data$Spectra

    # Store data for the assay matrix
    assay_data[[basename(file)]] <- spectra
    combined_data <- data.frame(wavelengths, spectra)
    asd_data_list[[basename(file)]] <- combined_data

    # Extract the metadata
    asd_metadata <- extract.metadata(file.dir = file,  out.dir = '~' ,instrument = "ASD")
    file_name <- asd_metadata$Spectra_File_Name
    spectral_time <- asd_metadata$Spectrum_Time_UTC
    combined_metadata <- data.frame(file_name, spectral_time)
    asd_metadata_list <- append(asd_metadata_list, list(combined_metadata))
  }

  # Combine metadata for better readability
  asd_metadata_list <- do.call(rbind, asd_metadata_list)

  # Initialize the assay matrix data frame
  assay_matrix <- do.call(cbind, assay_data)
  wavelengths <- asd_data_list[[1]]$wavelengths
  rownames(assay_matrix) <- wavelengths
  assay_df <- data.frame(assay_matrix)

  # Set column names based on file names from metadata
  colnames(assay_df) <- sapply(asd_metadata_list$file_name, function(x) basename(x))

  # Create ColData
  colData <- DataFrame(samplenames = colnames(assay_df))

  # Create rowData
  rowData <- DataFrame(Wavelengths = rownames(assay_df))

  # Create the SummarizedExperiment object
  se <- SummarizedExperiment(assays = list(counts = as.matrix(assay_df)),
                             colData = colData,
                             rowData = rowData)

  return(se)
}

# Example
# path <- "/Users/methungeorge/Desktop/IPB/test/data"
# se <- read_summarizedexperiment_asd(path)
# se
#
# # # Check the SummarizedExperiment object
# print(se)
# colnames(se)
# rownames(se)
# # # Access the assay data
# assay_data <- assay(se, "counts")
# head(assay_data)
##note : now the coldata is 1D and we need to include the trait information, eg look at the article
