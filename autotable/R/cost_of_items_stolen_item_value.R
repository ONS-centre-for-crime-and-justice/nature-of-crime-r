#' @title cost_of_items_stolen_item_value
#'
#' @description Builds the only part of one subtable of a "Cost of items stolne" table for a Nature of Crime dataset.
#'
#' @details This function builds the only part of a "Cost of items stolen" subtable (specified by \code{subtable}) based
#' predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for
#' more details about these two config files.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate
#' "Nature of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#' then the subtable considered will be that corresponding to the \code{n}th item in \code{config$OFFENCE_CODES}.
#'
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/cost_of_items_stolen/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' cost_of_items_stolen(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

cost_of_items_stolen_item_value <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
years_of_data = lapply(config$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
  VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
  weight_variable = config$WEIGHT_VARIABLES[[i]][[2]]

  # 5. Remove zero-weighted cases from the dataset:
  SIV_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
  # (Regarding the choice of variable name here: This just stands for "Stolen item value", which describes the
  #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
  #  onwards; SIV_1 will become SIV_2, and then SIV_2 will become SIV_3, and so on, until we have the dataframe in the form
  #  that we want)
  futile.logger::flog.trace("offender_characteristics - Removed %s cases out of %s because their weights were 0",
                           nrow(VF_dataset) - nrow(SIV_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
  SIV_2 = dplyr::filter(SIV_1, victarea == 1 | wherhapp == 1)

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
  offence_codes = config$OFFENCE_CODES[[subtable]][[2]]
  SIV_3 = dplyr::mutate(SIV_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))

  # 8. Select only the cases where relevant_incident = 1:
  SIV_4 = dplyr::filter(SIV_3, relevant_incident == 1)
  futile.logger::flog.trace("offender_characteristics - Selected %s cases where relevant_incident = 1",
                            nrow(SIV_4))

  # 9. Select only cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
  SIV_5 = dplyr::filter(SIV_4, vftype == 1)

  # 10. Filter out those who responded "Don't know" or who refused to answer the questions of interest:
  SIV_6 = dplyr::filter(SIV_5, totval2 < 9)

  # 11. Extract the unweighted base:
  unweighted_base = base::sum(SIV_6$relevant_incident)

  # 12. Weight the data:
  SIV_7 = autotable::apply_weight(SIV_6, c("relevant_incident"), weight = weight_variable)

  # 13. Group cases by their values for totvalue, and then summarise:
  SIV_7$totval2 = factor(SIV_7$totval2, levels = c(1:8))
  SIV_8         = dplyr::group_by(SIV_7, totval2, .drop = FALSE)
  SIV_9         = dplyr::summarise_at(SIV_8, "relevant_incident", sum)
  SIV_9$totval2 = c(1:8)

  # 14. Generate the unrounded percentages for this year:
  SIV_10 = (SIV_9$relevant_incident / base::sum(SIV_9$relevant_incident)) * 100
  futile.logger::flog.trace("emotional_impact_extent_of_impact - Generated the unrounded percentages")

  # 15. Calculate the weighted mean cost:
  SIV_mean_1 = dplyr::filter(SIV_7, !is.na(totvalue) & totvalue != 99998 & totvalue != 99999)
  SIV_mean = stats::weighted.mean(SIV_mean_1$totvalue, SIV_mean_1[, weight_variable], na.rm=TRUE)

  # And we'll need the standard deviation of totvalue for the means sig-testing that will be done later.
    if (i == 1) {
      standard_deviations_of_total_value = sqrt(sum(SIV_7$relevant_incident * ((SIV_7$totvalue - SIV_mean)^2)) / (sum(SIV_7$relevant_incident) - 1))

    } else {
      standard_deviations_of_total_value = c(standard_deviations_of_total_value,
                                            sqrt(sum(SIV_7$relevant_incident * ((SIV_7$totvalue - SIV_mean)^2)) / (sum(SIV_7$relevant_incident) - 1)))
    }

  # 15. Calculate the weighted median:
    SIV_median_1 = dplyr::filter(SIV_7, !is.na(totvalue) & totvalue != 99998 & totvalue != 99999)
    SIV_median_2 = dplyr::group_by(SIV_7, totvalue, .drop = FALSE)
    SIV_median_3 = dplyr::summarise_at(SIV_median_2, "relevant_incident", sum)
    SIV_median_3$cumulative_frequency = base::cumsum(SIV_median_3$relevant_incident)
    SIV_median = (SIV_median_3$totvalue)[grep(sum(SIV_median_3$relevant_incident)/2, sort(c(SIV_median_3$cumulative_frequency, sum(SIV_median_3$relevant_incident)/2)))]

  # 16. Combine the three parts as necessary to get one column of our dataframe:
  SIV_11 = as.data.frame(base::c(SIV_10, SIV_mean, SIV_median))

  # 17. Generate a name for this column, which we will later apply in Step 23:
  start_year              = substr(years_of_data[[i]], 3, 4)
  end_year                = as.numeric(start_year) + 1
  names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)

  # 18. We now want to add this column into our final dataframe:
  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
    # that case, we must create one:

    SIV_12                  = base::as.data.frame(SIV_11)
    row_of_unweighted_bases = unweighted_base

  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

    SIV_12 = base::cbind(SIV_12, base::as.data.frame(SIV_11))
    row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

  }
}


# 19. Bind the row of unweighted bases to the bottom of the table:
SIV_13 = base::rbind(SIV_12, row_of_unweighted_bases)

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  SIV_14 = append_significance_column(dataframe = SIV_13, column_1 = ncol(SIV_13), column_2 = ncol(SIV_13) - 1,
                                      row_of_unweighted_bases = nrow(SIV_13))
  SIV_15 = append_significance_column(dataframe = SIV_14, column_1 = ncol(SIV_14) - 1, column_2 = 1,
                                      row_of_unweighted_bases = nrow(SIV_14))

  # Means sig-testing is done a bit differently however.
  means_test_for_10_years = autotable::mean_sig_test(estimate_1 = SIV_15[nrow(SIV_15) - 2, ncol(SIV_15) - 2],
                                                     estimate_2 = SIV_15[nrow(SIV_15) - 2, 1],
                                                     base_1     = SIV_15[nrow(SIV_15), ncol(SIV_15) - 2],
                                                     base_2     = SIV_15[nrow(SIV_15), 1],
                                                     sd_1       = standard_deviations_of_total_value[ncol(SIV_15) - 2],
                                                     sd_2       = standard_deviations_of_total_value[1],
                                                     significance_indicator   = "*",
                                                     insignificance_indicator = " ")
  means_test_for_1_year   = autotable::mean_sig_test(estimate_1 = SIV_15[nrow(SIV_15) - 2, ncol(SIV_15) - 2],
                                                     estimate_2 = SIV_15[nrow(SIV_15) - 2, ncol(SIV_15) - 3],
                                                     base_1     = SIV_15[nrow(SIV_15), ncol(SIV_15) - 2],
                                                     base_2     = SIV_15[nrow(SIV_15), ncol(SIV_15) - 3],
                                                     sd_1       = standard_deviations_of_total_value[ncol(SIV_15) - 2],
                                                     sd_2       = standard_deviations_of_total_value[ncol(SIV_15) - 3],
                                                     significance_indicator   = "*",
                                                     insignificance_indicator = " ")

  SIV_15[] = lapply(SIV_15, function(x) if(is.factor(x)) as.character(x) else x)
  SIV_15[nrow(SIV_15) - 2, ncol(SIV_15) - 1] = means_test_for_10_years
  SIV_15[nrow(SIV_15) - 2, ncol(SIV_15)]     = means_test_for_1_year

  # And medians cannot be sig-tested, so we need to ensure that there is a ":" in that row indicating this.
  SIV_15[nrow(SIV_15) - 1, ncol(SIV_15) - 1] = ":"
  SIV_15[nrow(SIV_15) - 1, ncol(SIV_15)]     = ":"

# 21. Name the rows:
rownames(SIV_15) = config$ROW_LABELS

# 22. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$FOOTNOTES), "$$")

  colnames(SIV_15) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)

# 23. Return the final data frame:
return(SIV_15)

}
