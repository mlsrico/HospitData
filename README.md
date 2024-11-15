
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HospitData

<!-- badges: start -->
<!-- badges: end -->

The goal of HospitData is to provide functions that streamline common
hospital data processing tasks. The package includes tools for:

- Importing and combining multiple Excel data files
- Standardizing biomarker measurements across different units
- Formatting regression results for publication
- Calculating and presenting group frequencies in clinical data

## Installation

You can install the development version of HospitData from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("mlsrico/HospitData")
```

## Examples

### Importing Multiple Excel Files

``` r
library(HospitData)
# Import all Excel files from a directory
excel_files <- import_excel_files("path/to/files")
# Combine them into a single dataset
combined_data <- combine_excel_data(excel_files)
```

### Standardizing Biomarker Units

``` r
# Standardize units for multiple biomarkers
standardized_results <- standardize_biomarkers(
  bio_data = your_data,
  meta_concept = "biomarker_name",
  unit_source_value = "original_unit",
  value_as_number = "measurement"
)
```

### Formatting Regression Results

``` r
# Format logistic regression results
model <- glm(outcome ~ predictor1 + predictor2, 
             family = binomial, 
             data = your_data)
formatted_results <- CleanLog(model)

# Format multinomial regression results
library(nnet)
multi_model <- multinom(outcome ~ predictor1 + predictor2, 
                       data = your_data)
multi_results <- CleanLogMulti(multi_model)
```

### Calculating Group Frequencies

``` r
# Calculate event frequencies by treatment group
freq_table <- FreqOutcome(
  gr_var = "treatment",
  status_var = "event",
  df = your_data
)
```

## Features

- **Excel Import**: Handles multiple Excel files with error checking and
  automatic type conversion
- **Biomarker Standardization**:
  - Supports 40+ common biomarkers
  - Handles multiple unit conversions
  - Provides detailed conversion summaries
- **Statistical Results Formatting**:
  - Clean formatting for logistic regression results
  - Supports both binary and multinomial models
  - Publication-ready output
- **Frequency Calculations**:
  - Flexible grouping variable support
  - Clear presentation of counts and percentages
  - Ideal for clinical outcome reporting

## Citation

If you use HospitData in your research, please cite:

    @software{HospitData2024,
      author = {Your Name},
      title = {HospitData: Hospital Data Processing Tools for R},
      year = {2024},
      url = {https://github.com/YourGitHubUsername/HospitData}
    }
