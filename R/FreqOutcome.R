
#' Calculate Event Frequencies by Group
#'
#' @description
#' Computes the frequency of events for two groups (e.g., treatment vs control),
#' presenting results in a formatted table with counts and percentages.
#'
#' @param gr_var Character. Name of the column containing the grouping variable
#'        (e.g., treatment status). Must be binary (2 levels).
#' @param status_var Character. Name of the column containing the event status.
#'        Must be binary (2 levels).
#' @param df Data frame containing the variables.
#'
#' @return A data frame with two rows (with/without group) and two columns:
#'   \itemize{
#'     \item "level": Group labels ("with"/"without")
#'     \item "event / N (%)": Formatted string containing "n_events / N (percentage%)"
#'   }
#'
#' @details
#' The function:
#'   \itemize{
#'     \item Assumes binary coding for both variables
#'     \item Uses second level of gr_var as "exposed" group
#'     \item Calculates percentages rounded to 1 decimal place
#'     \item Returns results in format "n / N (xx.x%)"
#'   }
#'
#' @examples
#' \dontrun{
#' # Example with treatment variable
#' df <- data.frame(
#'   treatment = c(1,1,0,0,1,0),
#'   outcome = c(1,0,1,0,1,0)
#' )
#' result <- FreqOutcome(gr_var = "treatment",
#'                      status_var = "outcome",
#'                      df = df)
#' print(result)
#' # Example output:
#' #           event / N (%)
#' # with      2 / 3 (66.7%)
#' # without   1 / 3 (33.3%)
#'
#' # Example with different grouping variable
#' result <- FreqOutcome(gr_var = "gender",
#'                      status_var = "event",
#'                      df = patient_data)
#' }
#'
#' @note
#' The grouping variable (gr_var) can be any binary variable, not just treatment.
#' Common use cases include comparing event frequencies between:
#'   \itemize{
#'     \item Treatment groups
#'     \item Gender
#'     \item Exposure status
#'     \item Disease status
#'   }
#'
#' @importFrom stats table
#' @export

FreqOutcome <- function(gr_var, status_var, df){

  colnames(df)[which(colnames(df)==gr_var)] <- "gr_var"
  colnames(df)[which(colnames(df)==status_var)] <- "status_var"

  tb1 <- table(df$gr_var)
  tb2 <- table(df$gr_var, df$status_var)

  n_exposed <- tb1[2]
  n_ctrl <- tb1[1]

  o_exposed <- tb2[4]
  o_ctrl <- tb2[3]

  p_exposed <- paste(round((o_exposed/n_exposed)*100, 1), "%", sep = "")
  p_ctrl <- paste(round((o_ctrl/n_ctrl)*100, 1), "%", sep = "")


  res_exposed <- paste(o_exposed, " / ", n_exposed, " (", p_exposed, ")", sep = "")
  res_ctrl <- paste(o_ctrl, " / ", n_ctrl, " (", p_ctrl, ")", sep = "")

  resdf <- data.frame(
    var = c("with", "without"),
    res = c(res_exposed, res_ctrl))

  colnames(resdf) <- c("", "event / N (%)")

  return(resdf)

}

