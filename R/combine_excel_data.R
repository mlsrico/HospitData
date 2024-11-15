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
