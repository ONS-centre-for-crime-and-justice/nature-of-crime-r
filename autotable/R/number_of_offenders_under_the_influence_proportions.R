#' @title number_of_offenders_under_the_influence_proportion
#'
#' @description Builds the "Proportion of all incidents" part of one subtable of a "Number of offenders under the
#' influence" table for a Nature of Crime dataset.
#'
#' @details This function builds the "Proportion of all incidents" part of one subtable (specified by \code{subtable})
#' based predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for
#' more details about these two config files.
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Proportion of all incidents"
#' part of the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/number_of_offenders_under_the_influence/ROBBERY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' number_of_offenders_under_the_influence_proportions(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

number_of_offenders_under_the_influence_proportions <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)


# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF datasets:
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Reading in source data from %s", list_of_VF_datasets[i])
  VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variables:
  weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]

  # 5. Remove zero-weighted cases from the dataset:
  PoAI_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
  # (Regarding the choice of variable name here: This just stands for "Proportion of All Incidents", which describes the table component that we want
  #  this function to build. Each step in the code will build on the previous one from now; PoAI_1 will become PoAI_2, and then become PoAI_3, and then
  #  PoAI_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
  # Fetch the offence type variables from the yaml config file for the loop
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Removed %s cases out of %s because their weights were 0",
                           nrow(VF_dataset) - nrow(PoAI_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
  PoAI_2 = dplyr::filter(PoAI_1, victarea == 1 | wherhapp == 1)
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Selected %s cases where victarea = 1 or wherapp = 1",
                           nrow(PoAI_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
  offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
  PoAI_3 = dplyr::mutate(PoAI_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
  PoAI_4 = dplyr::filter(PoAI_3, relevant_incident == 1)
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Selected %s cases where relevant_incident = 1",
                           nrow(PoAI_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
  PoAI_5 = dplyr::filter(PoAI_4, vftype == 1)
  futile.logger::flog.trace("build_time_of_day - Selected %s cases where vftype = 1", nrow(PoAI_5))

  # 10. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
  if (grepl("violen", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
    number_of_cases_pre_filter = nrow(PoAI_5)
    PoAI_5 = autotable::apply_additional_violence_filters(dataframe = PoAI_5,
                                                         subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
    futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Applied additional violence filters, removing a further %s cases",
                              number_of_cases_pre_filter - nrow(PoAI_5))
  }

  # 11. Filter out cases which have missing values for the 'drink' variable, and then the same for the 'drug' variable:
  PoAI_6_drink = dplyr::filter(PoAI_5, !is.na(drink))
  PoAI_6_drug  = dplyr::filter(PoAI_5, !is.na(drug))

  # 12. Historically, the unweighted base that is listed for this part of the table is that which underlies the 'drink' row:
  unweighted_base = base::sum(PoAI_6_drink$relevant_incident)

  # 13. Weight the data:
  PoAI_7_drink = autotable::apply_weight(PoAI_6_drink, "relevant_incident", weight = weight_variable)
  PoAI_7_drug  = autotable::apply_weight(PoAI_6_drug, "relevant_incident", weight = weight_variable)
  futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Weighted the data by %s", weight_variable)

  # 14. Group cases by their values, and then summarise for both parts:
  PoAI_7_drink$drink = factor(PoAI_7_drink$drink, levels = c(1, 2, 9))
  PoAI_7_drug$drug   = factor(PoAI_7_drug$drug, levels = c(1, 2, 9))

  PoAI_8_drink = dplyr::group_by(PoAI_7_drink, drink, .drop = FALSE)
  PoAI_8_drug  = dplyr::group_by(PoAI_7_drug, drug, .drop = FALSE)

  PoAI_9_drink = dplyr::summarise_at(PoAI_8_drink, "relevant_incident", sum)
  PoAI_9_drug  = dplyr::summarise_at(PoAI_8_drug, "relevant_incident", sum)

  PoAI_9_drink$drink = c(1, 2, 9)
  PoAI_9_drug$drug   = c(1, 2, 9)

  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Grouped cases by their values for 'drink' and 'drug'")

  # 15. Generate the proportions for this year:
  PoAI_10_drink = (PoAI_9_drink / base::sum(PoAI_9_drink$relevant_incident)) * 100
  PoAI_10_drug  = (PoAI_9_drug  / base::sum(PoAI_9_drug$relevant_incident))  * 100
  futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Generated the unrounded percentages")

  PoAI_11 = base::as.data.frame(c(PoAI_10_drink[1, "relevant_incident"], PoAI_10_drug[1, "relevant_incident"]))

  # 16. Generate a name for this column, which we will later apply in Step :
  start_year              = substr(years_of_data[[i]], 3, 4)
  end_year                = as.numeric(start_year) + 1
  names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
  futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Finished column \"%s\"", names(years_of_data)[i])

  # 17. We now want to add this column into our final dataframe:

  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                # that case, we must create one:

    PoAI_12 = base::as.data.frame(PoAI_11)
    row_of_unweighted_bases = unweighted_base

  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

    PoAI_12 = base::cbind(PoAI_12, base::as.data.frame(PoAI_11))
    row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

  }

}


# 18. And add the row of unweighted bases to the bottom of the table
PoAI_13 = base::rbind(PoAI_12, row_of_unweighted_bases)

# 19. Name the rows:
rownames(PoAI_13) = config$PART_ONE$ROW_LABELS
futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Added row labels")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
PoAI_14 = append_significance_column(dataframe = PoAI_13, column_1 = ncol(PoAI_13), column_2 = ncol(PoAI_13) - 1,
                                    row_of_unweighted_bases = nrow(PoAI_13))
futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Appended a significance column, comparing the latest year of data with the previous year")
PoAI_15 = append_significance_column(dataframe = PoAI_14, column_1 = ncol(PoAI_14) - 1, column_2 = 1,
                                    row_of_unweighted_bases = nrow(PoAI_14))
futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions  - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the columns:

# For the sig-testing columns, we'll need to attach the correct footnote number:
name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                     grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                     grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

colnames(PoAI_15) = c(names(years_of_data),
                     name_of_10_year_test_column,
                     name_of_1_year_test_column)

futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Added column labels")

return(PoAI_15)

}
