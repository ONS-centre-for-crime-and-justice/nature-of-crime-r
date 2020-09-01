#'@title mean_sig_test
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

mean_sig_test <- function(estimate_1,
                          estimate_2,
                          base_1,
                          base_2,
                          sd_1,
                          sd_2,
                          design = 1.2,
                          threshold = 1.96,
                          significance_indicator = 1,
                          insignificance_indicator = 0) {
  
  if (estimate_1 <= 0 | estimate_2 <= 0)
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
  else if (estimate_1 == estimate_2)
  {
    warning("estimates are the same")
    return(NA)
  }
  
  else{
   sdsqr_1 = sd_1 * sd_1
   sdsqr_2 = sd_2 * sd_2
   sqr_over_base_1 = sdsqr_1 / base_1
   sqr_over_base_2 = sdsqr_2 / base_2
   variance = sqr_over_base_1 + sqr_over_base_2
   se_diff = sqrt(variance)
   mean_diff = estimate_1 - estimate_2
   z_score_pre_de = mean_diff / se_diff
   z_score = z_score_pre_de / design
   
  sig =  ifelse(threshold <= z_score || (threshold * -1) >= z_score, significance_indicator, insignificance_indicator)
    return(sig)
  }
}