
#' Standardize Biomarker Measurements
#'
#' @description
#' Standardizes biomarker measurements across different units of measurement commonly used in clinical
#' and laboratory settings. Converts biomarker values to standardized units based on literature standards.
#'
#' @param bio_data A data frame containing the biomarker measurements
#' @param meta_concept The name of the column containing biomarker identifiers (character)
#' @param unit_source_value The name of the column containing original units of measurement (character)
#' @param value_as_number The name of the column containing numerical values (numeric)
#'
#' @details Column requirements:
#'   \itemize{
#'     \item The column specified by meta_concept must contain biomarker identifiers as characters
#'     \item The column specified by unit_source_value must contain units of measurement as characters
#'     \item The column specified by value_as_number must contain numeric measurement values
#'   }
#'
#' @return A list containing two elements:
#'   \itemize{
#'     \item standardized_data: A data frame with original data plus additional columns:
#'       \itemize{
#'         \item standardized_value: The converted measurement in standard unit
#'         \item original_unit: The original unit preserved for reference
#'         \item standard_unit: The target standard unit
#'       }
#'     \item conversion_summary: A data frame summarizing conversions performed:
#'       \itemize{
#'         \item meta_concept: Biomarker name
#'         \item original_unit: Original unit of measurement
#'         \item standard_unit: Standard unit used
#'         \item n_values: Number of values converted
#'         \item n_na_units: Number of missing units
#'         \item n_no_standard: Number of values with no standard unit
#'         \item n_unknown_marker: Number of unknown markers
#'         \item mean_original: Mean of original values
#'         \item mean_standardized: Mean of standardized values
#'       }
#'   }
#'
#' @section Supported Biomarkers and Standard Units:
#'   \itemize{
#'     \item Enzymes (U/L): ALAT, ASAT, CK, LDH
#'     \item Proteins (g/L): ALB, FIB, PROT
#'     \item Blood Cells (10^9/L): BASO, EOSINO, LEUC, LYM, MONO, PNN, PLAQ
#'     \item Metabolites (μmol/L): BILI, CREAT
#'     \item Cytokines (pg/mL): BNP, IL6, IL10, IL1B, TNFA
#'     \item Electrolytes (mmol/L): BICAR, CA, GLUC, K, LACT_V, NA, PHOSPH, TCA, UREE
#'     \item Other:
#'       \itemize{
#'         \item TROPO (ng/L)
#'         \item CST (nmol/L)
#'         \item CRP (mg/L)
#'         \item DDI, FERRI (μg/L)
#'         \item HB (g/dL)
#'         \item HBA1C, PROC (%)
#'         \item NT-PROBNP (pg/mL)
#'         \item PROTU (g/L)
#'         \item PROTUCREATU (mg/mmol)
#'         \item TP (mmol/L)
#'         \item TSH (mIU/L)
#'       }
#'   }
#'
#' @section Unit Conversions:
#'   \itemize{
#'     \item Basic equivalencies: U/L ↔ IU/L, g/L ↔ g/L, mmol/L ↔ mmol/L
#'     \item Complex conversions:
#'       \itemize{
#'         \item g/dL → g/L (×10)
#'         \item mg/dL → g/L (×0.1)
#'         \item mg/dL → mmol/L (×0.0555)
#'         \item mg/dL → μmol/L (×17.1)
#'         \item cells/μL → 10^9/L (×0.001)
#'         \item cells/mm³ → 10^9/L (×0.001)
#'         \item ng/mL → ng/L (×1000)
#'         \item μg/L → ng/L (×1)
#'         \item pmol/L → ng/L (×0.118)
#'         \item ng/mL → pg/mL (×1000)
#'         \item mEq/L → mmol/L (×1)
#'         \item g/24h → g/L (×1)
#'         \item mg/g → mg/mmol (×0.113)
#'         \item μIU/mL → mIU/L (×1)
#'       }
#'   }
#'
#' @section Warning:
#' This function performs row-by-row processing using rowwise() which makes it very computationally
#' intensive. For large datasets (>10,000 rows), the processing time can be significant. Consider
#' breaking up large datasets into smaller chunks if possible.
#'
#' @note
#' \itemize{
#'   \item All biomarker names and units are case-insensitive
#'   \item Special characters in units are removed during processing
#'   \item The function maintains a detailed conversion history in the summary output
#'   \item Default units are assigned based on common laboratory standards when units are missing
#' }
#'
#' @examples
#' bio_data_pos <- data.frame(
#'   meta_concept = c("ALAT", "CREAT"),
#'   unit_source_value = c("IU/L", "mg/dL"),
#'   value_as_number = c(45, 1.2)
#' )
#'
#' result <- standardize_biomarkers(bio_data_pos)
#' standardized_data <- result$standardized_data
#' conversion_summary <- result$conversion_summary
#'
#' # Check for conversion issues
#' print(result$conversion_summary %>% filter(n_na_units > 0))
#' print(result$conversion_summary %>% filter(n_no_standard > 0))
#' print(result$conversion_summary %>% filter(n_unknown_marker > 0))
#'
#' @importFrom dplyr %>% mutate filter group_by summarise ungroup rowwise left_join
#' @importFrom tidyr convert_value
#'
#' @export


standardize_biomarkers <- function(bio_data) {
  library(dplyr)
  library(tidyr)

  no_standard_unit_markers <- c(
    "some_marker1",  # Add markers that don't have standard units
    "some_marker2"
  )

  # Define standard units mapping with common units based on literature
  unit_standards <- tibble(
    meta_concept = c(
      "alat", "asat", "ck", "ldh",
      "alb", "fib", "prot",
      "baso", "eosino", "leuc", "lym", "mono", "pnn", "plaq",
      "bili", "creat",
      "bnp", "il6", "il10", "il1b", "tnfa",
      "bicar", "ca", "gluc", "k", "lact_v", "na", "phosph", "tca", "uree",
      "tropo",
      "cst",
      "crp",
      "ddi",
      "ferri",
      "hb",
      "hba1c",
      "nt-probnp",
      "proc",
      "protu",
      "protucreatu",
      "tp",
      "tsh"
    ),
    standard_unit = c(
      rep("u/l", 4),
      rep("g/l", 3),
      rep("10^9/l", 7),
      rep("μmol/l", 2),
      rep("pg/ml", 5),
      rep("mmol/l", 9),
      "ng/l",
      "nmol/l",
      "mg/l",
      "μg/l",
      "μg/l",
      "g/dl",
      "%",
      "pg/ml",
      "%",
      "g/l",
      "mg/mmol",
      "mmol/l",
      "miu/l"
    )
  )

  # Define conversion factors (all in lower case)
  conversion_factors <- list(
    # Basic conversions where units are equivalent
    "u/l_u/l" = 1,
    "iu/l_u/l" = 1,
    "g/l_g/l" = 1,
    "mmol/l_mmol/l" = 1,
    "umol/l_μmol/l" = 1,
    "pg/ml_pg/ml" = 1,
    "ng/l_ng/l" = 1,
    "mg/l_mg/l" = 1,
    "ug/l_μg/l" = 1,

    # Specific conversions
    "g/dl_g/l" = 10,
    "mg/dl_g/l" = 0.1,
    "mg/dl_mmol/l" = 0.0555,
    "mg/dl_μmol/l" = 17.1,
    "cells/ul_10^9/l" = 0.001,
    "cells/mm3_10^9/l" = 0.001,
    "ng/ml_ng/l" = 1000,
    "ug/l_ng/l" = 1,
    "pmol/l_ng/l" = 0.118,
    "ng/ml_pg/ml" = 1000,
    "meq/l_mmol/l" = 1,
    "g/24h_g/l" = 1,
    "mg/g_mg/mmol" = 0.113,
    "uiu/ml_miu/l" = 1
  )

  # Most common units for each marker when unit is NA
  default_units <- list(
    "alat" = "u/l",
    "asat" = "u/l",
    "ck" = "u/l",
    "ldh" = "u/l",
    "alb" = "g/l",
    "fib" = "g/l",
    "prot" = "g/l",
    "baso" = "10^9/l",
    "eosino" = "10^9/l",
    "leuc" = "10^9/l",
    "lym" = "10^9/l",
    "mono" = "10^9/l",
    "pnn" = "10^9/l",
    "plaq" = "10^9/l",
    "bili" = "μmol/l",
    "creat" = "μmol/l",
    "bnp" = "pg/ml",
    "il6" = "pg/ml",
    "il10" = "pg/ml",
    "il1b" = "pg/ml",
    "tnfa" = "pg/ml",
    "bicar" = "mmol/l",
    "ca" = "mmol/l",
    "gluc" = "mmol/l",
    "k" = "mmol/l",
    "lact_v" = "mmol/l",
    "na" = "mmol/l",
    "phosph" = "mmol/l",
    "tca" = "mmol/l",
    "uree" = "mmol/l",
    "tropo" = "ng/l",
    "cst" = "nmol/l",
    "crp" = "mg/l",
    "ddi" = "μg/l",
    "ferri" = "μg/l",
    "hb" = "g/dl",
    "hba1c" = "%",
    "nt-probnp" = "pg/ml",
    "proc" = "%",
    "protu" = "g/l",
    "protucreatu" = "mg/mmol",
    "tp" = "mmol/l",
    "tsh" = "miu/l"
  )

  convert_value <- function(value, from_unit, to_unit, marker) {
    # Check if marker has no standard unit in literature
    if(marker %in% no_standard_unit_markers) {
      warning(sprintf("Marker %s has no standardized unit in literature. Keeping original value.", marker))
      return(as.numeric(value))
    }

    # Check if marker exists in the standards
    if(!marker %in% names(default_units)) {
      warning(sprintf("Unknown marker %s. Keeping original value.", marker))
      return(as.numeric(value))
    }

    # Handle NA units
    if(is.na(from_unit)) {
      warning(sprintf("Missing unit for marker %s. Using default unit: %s",
                      marker, default_units[[marker]]))
      from_unit <- default_units[[marker]]
    }

    # Standardize unit format
    from_unit <- tolower(gsub("[^a-zA-Z0-9/]", "", from_unit))
    to_unit <- tolower(gsub("[^a-zA-Z0-9/]", "", to_unit))
    conversion_key <- paste(from_unit, to_unit, sep="_")

    # If units are the same, return original value
    if(from_unit == to_unit) {
      return(as.numeric(value))
    }

    # Get conversion factor
    factor <- conversion_factors[[conversion_key]]

    # If no conversion factor found, warn and return original value
    if(is.null(factor)) {
      warning(sprintf("No conversion factor found for %s from %s to %s. Using original value.",
                      marker, from_unit, to_unit))
      return(as.numeric(value))
    }

    return(as.numeric(value) * factor)
  }

  # Main transformation
  standardized_data <- bio_data %>%
    mutate(
      meta_concept = tolower(meta_concept),
      unit_source_value = tolower(unit_source_value),
      value_as_number = as.numeric(value_as_number)
    ) %>%
    left_join(unit_standards, by = "meta_concept") %>%
    rowwise() %>%
    mutate(
      standardized_value = convert_value(
        value_as_number,
        unit_source_value,
        standard_unit,
        meta_concept
      ),
      original_unit = unit_source_value,
      .after = value_as_number
    ) %>%
    ungroup()

  # Generate summary of conversions
  conversion_summary <- standardized_data %>%
    group_by(meta_concept, original_unit, standard_unit) %>%
    summarise(
      n_values = n(),
      n_na_units = sum(is.na(original_unit)),
      n_no_standard = sum(meta_concept %in% no_standard_unit_markers),
      n_unknown_marker = sum(!meta_concept %in% names(default_units)),
      mean_original = mean(value_as_number, na.rm = TRUE),
      mean_standardized = mean(standardized_value, na.rm = TRUE),
      .groups = 'drop'
    )

  # Return both the standardized data and the summary
  return(list(
    standardized_data = standardized_data,
    conversion_summary = conversion_summary
  ))
}

# Usage example:
# result <- standardize_biomarkers(bio_data_pos)
# standardized_data <- result$standardized_data
# conversion_summary <- result$conversion_summary

# To check issues:
# View warnings for markers with no standard units:
# print(result$conversion_summary %>%
#       filter(n_no_standard > 0))

# View warnings for unknown markers:
# print(result$conversion_summary %>%
#       filter(n_unknown_marker > 0))

# View warnings for NA units:
# print(result$conversion_summary %>%
#       filter(n_na_units > 0))






