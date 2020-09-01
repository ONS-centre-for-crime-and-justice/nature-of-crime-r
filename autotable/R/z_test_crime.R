#'@title z_test_crime
#'
#'@description Documentation to be added later.
#'
#'@details Documentation to be added later.
#'
#'@param estimate_1 Documentation to be added later.
#'
#'@param estimate_2 Documentation to be added later.
#'
#'@param base_1 Documentation to be added later.
#'
#'@param base_2 Documentation to be added later.
#'
#'@param design_1 Documentation to be added later.
#'
#'@param design_2 Documentation to be added later.
#'
#'@param threshold Documentation to be added later.
#'
#'@param significance_indicator Documentation to be added later.
#'
#'@param insignificance_indicator Documentation to be added later.
#'
#'@return Returns the back-series as a data frame of percentages with the unweighted base as the last row.
#'
#'@examples
#'
#'@export

z_test_crime <- function(estimate_1,
                         estimate_2,
                         base_1,
                         base_2,
                         design_1 = 1.2,
                         design_2 = 1.2,
                         threshold = 1.96,
                         significance_indicator = 1,
                         insignificance_indicator = 0) {
  if (is.na(estimate_1 ) == 1 | is.na(estimate_2 ) == 1){
    warning("estimate is NA")
    return(NA)
  }
  
  else if (estimate_1 <= 0 | estimate_2 <= 0)
  {
    warning("estimate is equal to or less than 0")
    return(NA)
  }
  else  if (is.numeric(estimate_1) == 0 |
            is.numeric(estimate_2) == 0)
  {
    warning("estimate is non numeric")
    return(NA)
  }
  else if (base_1 <= 0 | base_2 <= 0)
  {
    warning("base is equal to or less than 0")
    return(NA)
  }
  else if (estimate_1 >= 100 | estimate_2 >= 100)
  {
    warning("estimate is equal to or greater than 100")
    return(NA)
  }
  else if (estimate_1 == estimate_2)
  {
    warning("estimates are the same")
    return(NA)
  }
  
  else{
    est_1_val <- estimate_1 / 100
    est_2_val <- estimate_2 / 100
    est_1_sqrt <- sqrt(est_1_val * (1 - est_1_val))
    est_2_sqrt <- sqrt(est_2_val * (1 - est_2_val))
    base_est_1 <- est_1_sqrt / sqrt(base_1)
    base_est_2 <- est_2_sqrt / sqrt(base_2)
    with_effect_1 <- base_est_1 * design_1
    with_effect_2 <- base_est_2 * design_2
    est_est <- est_1_val - est_2_val
    z_score <- est_est / sqrt((with_effect_1 ^ 2) + (with_effect_2 ^ 2))
    if (threshold <= z_score || (threshold * -1) >= z_score) {
      sig <- significance_indicator
    }
    else {
      sig <- insignificance_indicator
    }
    return(sig)
  }
}
