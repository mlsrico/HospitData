#' Clean and Format Multinomial Logistic Regression Results
#'
#' @description
#' Formats results from a multinomial logistic regression model, providing odds ratios,
#' confidence intervals, and p-values in a clean, publication-ready format.
#'
#' @param res A multinom object from nnet package (multinomial regression)
#'
#' @return A data frame with two columns:
#'   \itemize{
#'     \item rn: Variable names (excluding intercept)
#'     \item logres: Formatted string containing "OR (95% CI; p-value)"
#'   }
#'
#' @details
#' For each predictor variable and outcome level, the function:
#'   \itemize{
#'     \item Calculates exponential of coefficients (odds ratios)
#'     \item Computes 95% confidence intervals
#'     \item Rounds values to 2 decimal places
#'     \item Combines results in format: "OR (95% CI lower - upper; p-value)"
#'   }
#'
#' @examples
#' \dontrun{
#' # Fit a multinomial regression model
#' library(nnet)
#' model <- multinom(outcome ~ predictor1 + predictor2,
#'                   data = your_data)
#'
#' # Get formatted results
#' clean_results <- CleanLogMulti(model)
#' print(clean_results)
#' # Output example: "1.24 (1.10 - 1.40; 0.001)"
#' }
#'
#' @seealso \code{\link{CleanLog}} for binary logistic regression
#'
#' @importFrom dplyr mutate select filter left_join
#' @export


CleanLogMulti <- function(res){
  # ci95
  ci95 <- exp(confint.default(res, level = 0.95))
  # get objects
  sres <- summary(res)
  dres <- sres$coefficients %>% as.data.frame()
  # get exp & pvalue
  exp_p <- dres %>%
    mutate(rn = row.names(dres),
           expcoef = round(exp(`Estimate`), 2),
           pval = round(`Pr(>|z|)`, 3)) %>%
    dplyr::select(rn, expcoef, pval) %>%
    filter(rn!="(Intercept)")
  # get 95%CI
  coif <- ci95 %>%
    as.data.frame() %>%
    mutate(rn = row.names(dres),
           ci_inf = round(`2.5 %`, 2),
           ci_sup = round(`97.5 %`, 2)) %>%
    mutate(ci_both = paste("(", ci_inf, " - ", ci_sup, sep = "")) %>%
    dplyr::select(rn, ci_both) %>%
    filter(rn!="(Intercept)")
  # combine
  all_coefs <- left_join(exp_p, coif, by = "rn") %>%
    mutate(logres = paste(expcoef, " ", ci_both, "; ", pval, ")", sep = "")) %>%
    dplyr::select(rn, logres)
  return(all_coefs)
}

