#' Combine Multiple Excel Data Frames
#'
#' @description
#' Combines multiple data frames from imported Excel files into a single data frame,
#' handling type mismatches and providing detailed feedback.
#'
#' @param data_list A list of data frames, typically the output from
#'        \code{\link{import_excel_files}}
#'
#' @return A single data frame with:
#'   \itemize{
#'     \item All rows from input data frames
#'     \item A new 'source_file' column identifying the origin file
#'     \item Consistent column types across all data
#'   }
#'
#' @details
#' The function performs the following steps:
#'   \itemize{
#'     \item Identifies columns with mixed types across data frames
#'     \item Converts mixed-type columns to character to ensure compatibility
#'     \item Combines all data frames using row binding
#'     \item Removes any automatically generated Excel index columns (e.g., "...1")
#'     \item Adds a source_file column to track the origin of each row
#'   }
#'
#' @section Warning:
#' When columns have mixed types across files, they are converted to character type.
#' This conversion is reported in the console output.
#'
#' @examples
#' \dontrun{
#' # First import the Excel files
#' excel_list <- import_excel_files("path/to/folder")
#'
#' # Then combine them into a single data frame
#' combined_df <- combine_excel_data(excel_list)
#'
#' # Check the source files in the combined data
#' table(combined_df$source_file)
#' }
#'
#' @importFrom dplyr bind_rows mutate across select any_of all_of
#' @importFrom purrr map map_lgl
#'
#' @seealso \code{\link{import_excel_files}} for importing the Excel files
#'
#' @export

combine_excel_data <- function(data_list) {
  # Function to check if column types are mixed across dataframes
  check_column_types <- function(data_list, col_name) {
    types <- map(data_list, ~class(.x[[col_name]]))
    return(length(unique(types)) > 1)
  }

  # Get all column names
  all_cols <- unique(unlist(map(data_list, names)))

  # Check which columns have mixed types
  mixed_cols <- all_cols[sapply(all_cols, function(col) {
    # Only check dataframes that have this column
    dfs_with_col <- data_list[map_lgl(data_list, ~col %in% names(.x))]
    if(length(dfs_with_col) > 1) {
      check_column_types(dfs_with_col, col)
    } else {
      FALSE
    }
  })]

  # Convert mixed type columns to character in all dataframes
  if(length(mixed_cols) > 0) {
    cat("Converting the following columns to character due to mixed types:\n",
        paste(mixed_cols, collapse = ", "), "\n")

    data_list <- map(data_list, function(df) {
      cols_to_convert <- intersect(names(df), mixed_cols)
      if(length(cols_to_convert) > 0) {
        df <- df %>% mutate(across(all_of(cols_to_convert), as.character))
      }
      return(df)
    })
  }

  # Combine all dataframes by rows
  combined_data <- data_list %>%
    bind_rows(.id = "source_file") %>%
    # Remove ...1 column if it exists
    select(-any_of("...1"))

  # Print summary
  cat("\nCombined", length(data_list), "files into one dataset\n")
  cat("Final dimensions:", nrow(combined_data), "rows and",
      ncol(combined_data), "columns\n")

  return(combined_data)
}
