#' @title timing_time_of_day
#'
#' @description Builds the "Time of day" part of one subtable of a "Timing of when incidents occurred" table for a
#' Nature of Crime dataset.
#'
#' @details This function builds the "Time of day" part of one subtable (specified by \code{subtable}) based
#' predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for more
#' details about these two config files.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate
#' "Nature of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#' then the subtable considered will be that corresponding to the \code{n}th item in \code{config$ALL_PARTS$OFFENCE_CODES}.
#'
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Time of day" part of
#' the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/timing/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' timing_time_of_day(config = config, master_config = master_config, subtable = 4)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

timing_time_of_day <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("build_time_of_day - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("build_time_of_day - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    ToD_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Time of the Week", which describes the subtable
    #  component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; ToD_1 will become ToD_2, and then ToD_2 will become ToD_3, and so on, until we have the dataframe in
    #  the form that we want)
    futile.logger::flog.trace("build_time_of_day - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(ToD_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    ToD_2 = dplyr::filter(ToD_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("build_time_of_day - Selected %s cases where victarea = 1 or wherapp = 1", nrow(ToD_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    ToD_3 = dplyr::mutate(ToD_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("build_time_of_day - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    ToD_4 = dplyr::filter(ToD_3, relevant_incident == 1)
    futile.logger::flog.trace("build_time_of_day - Selected %s cases where relevant_incident = 1", nrow(ToD_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    ToD_5 = dplyr::filter(ToD_4, vftype == 1)
    futile.logger::flog.trace("build_time_of_day - Selected %s cases where vftype = 1", nrow(ToD_5))

  # 10. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(ToD_5)
      ToD_5 = autotable::apply_additional_violence_filters(dataframe = ToD_5,
                                                           subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("build_time_of_day - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(ToD_5))
    }

  # 11. We now want to exclude the missing cases. Because rows of the table are based on values of different variables,
  #     we will have to handle them separately, since their numbers of valid cases differ in general:
    ToD_6_time1  = dplyr::filter(ToD_5, !is.na(time1))
    ToD_6_time2  = dplyr::filter(ToD_5, !is.na(time2))
    ToD_6_time3  = dplyr::filter(ToD_5, !is.na(time3))

  # 12. According to historical production of these tables, the unweighted base to be listed in the table is that which
  #     underlies the time1 rows.
    unweighted_base = base::sum(ToD_6_time1$relevant_incident)
    futile.logger::flog.trace("build_time_of_day - Determined the unweighted base as %s", unweighted_base)
  # However, we expect that the data for the time2 and time3 rows should have the same underlying base also. In the event
  # that they do NOT, we will flog a warning to the user, suggesting that they may wish to add a footnote or investigate:
    if (length(unique(c(unweighted_base, base::sum(ToD_6_time2$relevant_incident), base::sum(ToD_6_time3$relevant_incident)))) != 1) {
      futile.logger::flog.warn("build_time_of_day - WARNING: The data in this table are based off different unweighted bases; there are not the same number of valid cases on the time1, time2 and time3 variables. You may wish to add a footnote acknolwedging this, or to investigate this anomaly")
    }

  # 13. Weight the data:
    ToD_7_time1 = autotable::apply_weight(ToD_6_time1, "relevant_incident", weight = weight_variable)
    ToD_7_time2 = autotable::apply_weight(ToD_6_time2, "relevant_incident", weight = weight_variable)
    ToD_7_time3 = autotable::apply_weight(ToD_6_time3, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("build_time_of_day - Weighted the data by %s", weight_variable)

  # 14. Group cases by their values for time1, time2 and time3, and then summarise:
    ToD_7_time1$time1 = factor(ToD_7_time1$time1, levels = c(1:7))
    ToD_7_time2$time2 = factor(ToD_7_time2$time2, levels = c(1:6))
    ToD_7_time3$time3 = factor(ToD_7_time2$time3, levels = c(1, 2))

    ToD_8_time1 = dplyr::group_by(ToD_7_time1, time1, .drop = FALSE)
    ToD_8_time2 = dplyr::group_by(ToD_7_time2, time2, .drop = FALSE)
    ToD_8_time3 = dplyr::group_by(ToD_7_time3, time3, .drop = FALSE)

    ToD_9_time1 = dplyr::summarise_at(ToD_8_time1, "relevant_incident", sum)
    ToD_9_time2 = dplyr::summarise_at(ToD_8_time2, "relevant_incident", sum)
    ToD_9_time3 = dplyr::summarise_at(ToD_8_time3, "relevant_incident", sum)

    ToD_9_time1$time1 = c(1:7)
    ToD_9_time2$time2 = c(1:6)
    ToD_9_time3$time3 = c(1, 2)

    futile.logger::flog.trace("build_time_of_day - Grouped cases by their values for time1, time2 and time3")

  # 15. Generate the unrounded percentages for this year:
    ToD_10_time1 = (ToD_9_time1 / base::sum(ToD_9_time1$relevant_incident)) * 100
    ToD_10_time2 = (ToD_9_time2 / base::sum(ToD_9_time2$relevant_incident)) * 100
    ToD_10_time3 = (ToD_9_time3 / base::sum(ToD_9_time3$relevant_incident)) * 100
    futile.logger::flog.trace("build_time_of_day - Generated the unrounded percentages")

  # 16. Combine the two parts as necessary to get one column of our dataframe:
    ToD_11 = base::c(ToD_10_time3[1,2], ToD_10_time1[base::c(1:3),2], ToD_10_time3[2,2],
                     ToD_10_time2[4,2], ToD_10_time1[base::c(4:7),2])

  # 16. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_time_of_day - Finished column \"%s\"", names(years_of_data)[i])

  # 17. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      ToD_12 = base::as.data.frame(ToD_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      ToD_12 = base::cbind(ToD_12, base::as.data.frame(ToD_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 17. Bind the row of unweighted bases to the bottom of the table:
  ToD_13 = base::rbind(ToD_12, row_of_unweighted_bases)
  futile.logger::flog.trace("build_time_of_day - Added the row of unweighted bases")

# 18. Name the rows:
  rownames(ToD_13) = config$PART_TWO$ROW_LABELS
  futile.logger::flog.trace("build_time_of_day - Added row labels")

# 19. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  ToD_14 = append_significance_column(dataframe = ToD_13, column_1 = ncol(ToD_13), column_2 = ncol(ToD_13) - 1,
                                      row_of_unweighted_bases = nrow(ToD_13))
  futile.logger::flog.trace("build_time_of_day - Appended a significance column, comparing the latest year of data with the previous year")
  ToD_15 = append_significance_column(dataframe = ToD_14, column_1 = ncol(ToD_14) - 1, column_2 = 1,
                                      row_of_unweighted_bases = nrow(ToD_14))
  futile.logger::flog.trace("build_time_of_day - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

  colnames(ToD_15) = c(names(years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)

  futile.logger::flog.trace("build_time_of_day - Added column labels")

# 22. Return the final dataframe:
  return(ToD_15)

}

