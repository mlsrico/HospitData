#' Import Multiple Excel Files from a Folder
#'
#' @description
#' Reads all Excel files (.xlsx or .xls) from a specified folder into a list of
#' data frames. Handles errors gracefully and provides import summary.
#'
#' @param folder_path Character string specifying the path to the folder containing
#'        Excel files
#'
#' @return A named list where:
#'   \itemize{
#'     \item Names are the original Excel file names
#'     \item Values are the corresponding data frames
#'   }
#'
#' @details
#' The function:
#'   \itemize{
#'     \item Searches for both .xlsx and .xls files
#'     \item Skips files that cannot be read
#'     \item Names each data frame with its source file name
#'     \item Provides console output summarizing the import results
#'   }
#'
#' @note
#' Files that fail to import are removed from the final list and generate warnings
#' but do not stop the overall process.
#'
#' @examples
#' \dontrun{
#' # Import all Excel files from a folder
#' excel_list <- import_excel_files("path/to/folder")
#'
#' # Check the names of successfully imported files
#' names(excel_list)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom purrr map set_names
#'
#' @seealso \code{\link{combine_excel_data}} for combining the imported data frames
#'
#' @export
#'


import_excel_files <- function(folder_path) {
  # Get list of all Excel files in the folder
  excel_files <- list.files(
    path = folder_path,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE
  )

  # Check if any Excel files were found
  if (length(excel_files) == 0) {
    stop("No Excel files found in the specified folder")
  }

  # Import each file and store in a named list
  excel_data <- excel_files %>%
    set_names(basename(.)) %>%
    map(function(file) {
      tryCatch({
        read_excel(file)
      }, error = function(e) {
        warning(sprintf("Error reading file %s: %s", file, e$message))
        return(NULL)
      })
    })

  # Remove any NULL entries (failed imports)
  excel_data <- excel_data[!sapply(excel_data, is.null)]

  # Print summary of imported files
  cat("Successfully imported", length(excel_data), "Excel files\n")

  return(excel_data)
}
