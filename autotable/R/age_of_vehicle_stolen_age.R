#'@title age_of_vehicle_stolen_age
#'
#'@description To do.
#'
#'@details To do.
#'
#'@param config To do.
#'
#'@param master_config To do.
#'
#'@param subtable To do.
#'
#'@return To do.
#'
#'@examples To do.
#'
#'@export

age_of_vehicle_stolen_age = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
years_of_data = sapply(config$YEARS_AND_WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
  
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Reading in source data from %s", list_of_VF_datasets[i])
  VF_dataset <- base::as.data.frame(data.table::fread(base::paste(config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
  
  weight_variable = config$YEARS_AND_WEIGHT_VARIABLES[[i]][[2]]
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
  
  AoVS_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
  # (Regarding the choice of variable name here: This just stands for "Age of vehicle stolen", which describes the
  #  part that we want this function to build. Each step in the code will build on the previous one from now onwards;
  #  AoVS_1 will become AoVS_2, and then AoVS_2 will become AoVS_3, and so on, until we have the dataframe in the form
  #  that we want)
  
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Removed %s cases out of %s because their weights were 0",
                            nrow(VF_dataset) - nrow(AoVS_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
  
  AoVS_2 = dplyr::filter(AoVS_1, victarea == 1 | wherhapp == 1)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Selected %s cases where victarea = 1 or wherapp = 1", nrow(AoVS_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
  
  offence_codes = config$OFFENCE_CODES[[subtable]][[2]]
  AoVS_3 = dplyr::mutate(AoVS_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Computed the relevant_incident variable")

  # 8. Select only the cases where relevant_incident = 1:
  
  AoVS_4 = dplyr::filter(AoVS_3, relevant_incident == 1)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Selected %s cases where relevant_incident = 1", nrow(AoVS_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
  AoVS_5 = dplyr::filter(AoVS_4, vftype == 1)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Selected %s cases where vftype = 1", nrow(AoVS_5))

  # 10. Filter out invalid values (e.g. missings, don't knows and refusals) from the vehage variable:
  AoVS_6 = dplyr::filter(AoVS_5, !base::is.na(vehage) & vehage < 8)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Removed %s cases where vehage had an invalid value",
                            nrow(AoVS_5) - nrow(AoVS_6))

  # 11. Extract the unweighted base:
  unweighted_base = base::sum(AoVS_6$relevant_incident)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Determined the unweighted base as %s", unweighted_base)

  # 12. Weight the data:
  AoVS_7 = autotable::apply_weight(AoVS_6, "relevant_incident", weight = weight_variable)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Weighted the data by %s", weight_variable)

  # 13. Group cases by their values for vehage, and then summarise:
  AoVS_7$vehage  = factor(AoVS_7$vehage, levels = c(1:4))
  AoVS_8         = dplyr::group_by(AoVS_7, vehage, .drop = FALSE)
  AoVS_9         = dplyr::summarise_at(AoVS_8, "relevant_incident", sum)
  AoVS_9$vehage  = c(1:4)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Grouped cases by their values for vehage")

  # 14. Generate the unrounded percentages for this year:
  AoVS_10 = (AoVS_9$relevant_incident / base::sum(AoVS_9$relevant_incident)) * 100
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Generated the unrounded percentages")
  
  # 15. Bind the unweighted base:
  AoVS_11 = rbind(as.data.frame(AoVS_10), unweighted_base)

  # 16. Generate a name for this column, which we will later apply:
  start_year              = substr(years_of_data[[i]], 3, 4)
  end_year                = as.numeric(start_year) + 1
  names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Finished column \"%s\"", names(years_of_data)[i])

  # 17. We now want to add this column into our final dataframe:

  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                # that case, we must create one:

    AoVS_12 = AoVS_11

  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

    AoVS_12 = base::cbind(AoVS_12, AoVS_11)

  }

}

# 18. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  AoVS_13 = append_significance_column(dataframe = AoVS_12, column_1 = ncol(AoVS_12), column_2 = 1,
                                      row_of_unweighted_bases = nrow(AoVS_12))
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Appended a significance column, comparing the latest year of data with the previous year")
  AoVS_14 = append_significance_column(dataframe = AoVS_13, column_1 = ncol(AoVS_13) - 1, ncol(AoVS_13) - 2,
                                      row_of_unweighted_bases = nrow(AoVS_13))
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the rows:
  rownames(AoVS_14) = sapply(config$ROWS, `[[`, 1)
  futile.logger::flog.trace("age_of_vehicle_stolen_age - Added row labels")

# 22. Name the columns:
  
# For the sig-testing columns, we'll need to attach the correct footnote number:
  
name_of_10_year_test_column = paste0(config$SIGNIFICANCE_COLUMN_LABELS$TEN_YEAR_TEST, "$$Note_",
                                     length(config$FOOTNOTES), "$$")
name_of_1_year_test_column  = paste0(config$SIGNIFICANCE_COLUMN_LABELS$ONE_YEAR_TEST, "$$Note_",
                                     length(config$FOOTNOTES), "$$")

colnames(AoVS_14) = c(names(years_of_data),
                     name_of_10_year_test_column,
                     name_of_1_year_test_column)

# 23. Return the final data frame:
  return(AoVS_14)

}

