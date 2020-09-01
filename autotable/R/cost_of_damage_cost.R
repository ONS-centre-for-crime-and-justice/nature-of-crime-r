#' @title cost_of_damage
#'
#' @description Builds the only part of one subtable of an "Cost of damage" table for a Nature of Crime dataset.
#'
#' @details This function builds the only part of a "Cost of damage" subtable (specified by \code{subtable}) based
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
#' @examples config = autotable::process_config(yaml::read.yaml("configs/cost_of_damage/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' cost_of_damage_cost(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

cost_of_damage_cost = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
years_of_data = lapply(config$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("cost_of_damage_cost - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("cost_of_damage_cost - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    CoD_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage Emotionally Affected", which describes the
    #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; CoD_1 will become CoD_2, and then CoD_2 will become CoD_3, and so on, until we have the dataframe in the form
    #  that we want)
    futile.logger::flog.trace("cost_of_damage_cost - Removed %s cases out of %s because their weights were 0",
                              nrow(VF_dataset) - nrow(CoD_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    CoD_2 = dplyr::filter(CoD_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("cost_of_damage_cost - Selected %s cases where victarea = 1 or wherapp = 1",
                              nrow(CoD_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$OFFENCE_CODES[[subtable]][[2]]
    CoD_3 = dplyr::mutate(CoD_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("cost_of_damage_cost - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    CoD_4 = dplyr::filter(CoD_3, relevant_incident == 1)
    futile.logger::flog.trace("cost_of_damage_cost - Selected %s cases where relevant_incident = 1",
                              nrow(CoD_4))

  # 9. Filter out NAs and missings/DKs/Refusals from the total cost of damage variable:
  #  if (config$GROUPED_TOTAL_DAMAGE_VARIABLE == "totdamx") {
  #    CoD_4 = dplyr::mutate(CoD_4, totdamx = ifelse(totdamag %in% c(0:49), 1,
  #                                           ifelse(totdamag %in% c(50:99), 2,
  #                                           ifelse(totdamag %in% c(100:499), 3,
  #                                           ifelse(totdamag %in% c(500:999), 4,
  #                                           ifelse(totdamag %in% c(1000:4999), 5,
  #                                           ifelse(totdamag %in% c(5000:9999), 6,
  #                                           ifelse(totdamag %in% c(10000:14999), 7,
  #                                           ifelse(totdamag %in% c(15000:99997), 8, NA)))))))))
  #  }
    CoD_5 = dplyr::filter(CoD_4, !base::is.na(get(config$GROUPED_TOTAL_DAMAGE_VARIABLE)))
    CoD_5 = dplyr::filter(CoD_5, totdamgp3 < 38888)

  # 10. Grab the unweighted base:
    unweighted_base = base::nrow(CoD_5)
    futile.logger::flog.trace("Removed %s cases where total damage was NA, yielding an unweighted base of %s to report",
                              nrow(CoD_5) - nrow(CoD_4), unweighted_base)
  # 11. Weight the data:
    CoD_6 = autotable::apply_weight(CoD_5, c("relevant_incident"), weight = weight_variable)
    futile.logger::flog.trace("cost_of_damage_cost - Weighted the data by %s", weight_variable)

  # 12. Group cases by their values for the total cost of damage variable, and then summarise:
    if (config$GROUPED_TOTAL_DAMAGE_VARIABLE == "totdamgp3") {
      min_level = 0
      max_level = 7
    } else if (config$GROUPED_TOTAL_DAMAGE_VARIABLE == "totdam2") {
      min_level = 1
      max_level = 8
    } else if (config$GROUPED_TOTAL_DAMAGE_VARIABLE == "totdamx") {
      min_level = 1
      max_level = 8
    }

    CoD_6[, config$GROUPED_TOTAL_DAMAGE_VARIABLE] = factor(CoD_6[, config$GROUPED_TOTAL_DAMAGE_VARIABLE], levels = c(min_level:max_level))
    CoD_7 = dplyr::group_by(CoD_6, get(config$GROUPED_TOTAL_DAMAGE_VARIABLE), .drop = FALSE)
    CoD_8 = dplyr::summarise_at(CoD_7, "relevant_incident", sum)
    CoD_8[, 1] = c(min_level:max_level)

    futile.logger::flog.trace("cost_of_damage_cost - Grouped cases by their values for totdamgp3")

  # 13. Generate the unrounded percentages for this year:
    CoD_9 = (CoD_8 / base::sum(CoD_8$relevant_incident)) * 100
    futile.logger::flog.trace("cost_of_damage_cost - Generated the unrounded percentages")

  # 14. Calculate the weighted mean:
    CoD_mean_1 = dplyr::filter(CoD_6, !is.na(totdamag) & totdamag < 99998)
    CoD_mean_2 = sum(CoD_mean_1$totdamag * CoD_mean_1$relevant_incident)
    CoD_mean   = CoD_mean_2 / sum(CoD_mean_1$relevant_incident)

    # And we'll need the standard deviation of totdamag for the means sig-testing that will be done later.
    if (i == 1) {
      standard_deviations_of_total_cost = sqrt(sum(CoD_mean_1$relevant_incident * ((CoD_mean_1$totdamag - CoD_mean)^2)) / (sum(CoD_mean_1$relevant_incident) - 1))

    } else {
      standard_deviations_of_total_cost = c(standard_deviations_of_total_cost,
                                            sqrt(sum(CoD_mean_1$relevant_incident * ((CoD_mean_1$totdamag - CoD_mean)^2)) / (sum(CoD_mean_1$relevant_incident) - 1)))
    }

  # 15. Calculate the weighted median:
    CoD_median_1 = dplyr::filter(CoD_6, !is.na(totdamag) & totdamag < 99998)
    CoD_median_2 = dplyr::group_by(CoD_median_1, totdamag, .drop = FALSE)
    CoD_median_3 = dplyr::summarise_at(CoD_median_2, "relevant_incident", sum)
    CoD_median_3$cumulative_frequency = base::cumsum(CoD_median_3$relevant_incident)
    CoD_median = (CoD_median_3$totdamag)[grep(sum(CoD_median_3$relevant_incident)/2, sort(c(CoD_median_3$cumulative_frequency, sum(CoD_median_3$relevant_incident)/2)))]

  # 16. Combine the three parts as necessary to get one column of our dataframe:
    CoD_10 = as.data.frame(base::c(CoD_9$relevant_incident, CoD_mean, CoD_median))

  # 17. Generate a name for this column, which we will later apply in Step 23:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("cost_of_damage_cost - Finished column \"%s\"", names(years_of_data)[i])

  # 18. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      CoD_11 = base::as.data.frame(CoD_10)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      CoD_11 = base::cbind(CoD_11, base::as.data.frame(CoD_10))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}


# 19. Bind the row of unweighted bases to the bottom of the table:
  CoD_12 = base::rbind(CoD_11, row_of_unweighted_bases)
  futile.logger::flog.trace("cost_of_damage_cost - Added the row of unweighted bases")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  CoD_13 = append_significance_column(dataframe = CoD_12, column_1 = ncol(CoD_12), column_2 = ncol(CoD_12) - 1,
                                      row_of_unweighted_bases = nrow(CoD_12))
  futile.logger::flog.trace("cost_of_damage_cost - Appended a significance column, comparing the latest year of data with the previous year")
  CoD_14 = append_significance_column(dataframe = CoD_13, column_1 = ncol(CoD_13) - 1, column_2 = 1,
                                      row_of_unweighted_bases = nrow(CoD_13))
  futile.logger::flog.trace("cost_of_damage_cost - Appended a significance column, comparing the latest year of data with the earliest year")

  # Means sig-testing is done a bit differently however.
  means_test_for_10_years = autotable::mean_sig_test(estimate_1 = CoD_14[nrow(CoD_14) - 2, ncol(CoD_14) - 2],
                                                     estimate_2 = CoD_14[nrow(CoD_14) - 2, 1],
                                                     base_1     = CoD_14[nrow(CoD_14), ncol(CoD_14) - 2],
                                                     base_2     = CoD_14[nrow(CoD_14), 1],
                                                     sd_1       = standard_deviations_of_total_cost[ncol(CoD_14) - 2],
                                                     sd_2       = standard_deviations_of_total_cost[1],
                                                     significance_indicator   = "*",
                                                     insignificance_indicator = " ")
  means_test_for_1_year   = autotable::mean_sig_test(estimate_1 = CoD_14[nrow(CoD_14) - 2, ncol(CoD_14) - 2],
                                                     estimate_2 = CoD_14[nrow(CoD_14) - 2, ncol(CoD_14) - 3],
                                                     base_1     = CoD_14[nrow(CoD_14), ncol(CoD_14) - 2],
                                                     base_2     = CoD_14[nrow(CoD_14), ncol(CoD_14) - 3],
                                                     sd_1       = standard_deviations_of_total_cost[ncol(CoD_14) - 2],
                                                     sd_2       = standard_deviations_of_total_cost[ncol(CoD_14) - 3],
                                                     significance_indicator   = "*",
                                                     insignificance_indicator = " ")

  CoD_14[] = lapply(CoD_14, function(x) if(is.factor(x)) as.character(x) else x)
  CoD_14[nrow(CoD_14) - 2, ncol(CoD_14) - 1] = means_test_for_10_years
  CoD_14[nrow(CoD_14) - 2, ncol(CoD_14)]     = means_test_for_1_year

  # And medians cannot be sig-tested, so we need to ensure that there is a ":" in that row indicating this.
  CoD_14[nrow(CoD_14) - 1, ncol(CoD_14) - 1] = ":"
  CoD_14[nrow(CoD_14) - 1, ncol(CoD_14)]     = ":"

# 21. Name the rows:
  rownames(CoD_14) = config$ROW_LABELS
  futile.logger::flog.trace("cost_of_damage_cost - Added row labels")

# 22. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$FOOTNOTES), "$$")

  colnames(CoD_14) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  futile.logger::flog.trace("cost_of_damage_cost - Added column labels")

# 23. Return the final data frame:
  return(CoD_14)

}

