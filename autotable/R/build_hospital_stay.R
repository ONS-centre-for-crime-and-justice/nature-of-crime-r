#'@title build_hospital_stay
#'
#'@description Builds the "Build_hospital_stay" (HS) part of one subtable of an "Injuries sustained in
#'incidents" Table for a Nature of Crime dataset.
#'
#'@details This function builds the HS part of one subtable (specified by \code{subtable}) based largely on a configuration
#'file specified by \code{config}.
#'
#'@param config A string corresponding to the path of a YAML file, where this YAML serves as the configuration file for the
#'function. This YAML must exhibit a particular structure/format, since the function accesses certain levels and fields of it
#'by name. For more information on the format required, please see the User Guide (found in the "resources" folder of the
#'project) or inspect a valid YAML file from previous successful runs.
#'
#'#'@param master_config A string corresponding to a folder path whereat the VF datasets for each year are stored as .csv
#'files.
#'
#'#'@param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#'then the subtable considered will be that corresponding to the \code{n}th item in
#'\code{config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES}.
#'
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the PIS part of the specified subtable.
#'
#'@examples build_hospital_stay(config = "configs/injuries_sustained/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_hospital_stay <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

  # Below, we will use a for-loop to process the years of data one-by-one.

  for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF datasets:
    futile.logger::flog.info("build_hospital_stay - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i+3], sep = '/')))
    
  # 4. Fetch the correct weight variables:
    weight_variable = config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]

  # (Regarding the choice of variable name here: This just stands for Hospital stay, which describes the table component that we want
  #  this function build. Each step in the code will build on the previous one from now; HS_1 will become HS_2, and then become HS_3, and then
  #  HS_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)

  # 5. Remove zero-weighted cases from the dataset:
    HS_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                             weightvar = weight_variable)
    futile.logger::flog.info("build_hospital_stay - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(HS_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    HS_2 = dplyr::filter(HS_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_hospital_stay - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(HS_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    HS_3 = dplyr::mutate(HS_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_hospital_stay - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    HS_4 = dplyr::filter(HS_3, relevant_incident == 1)
    futile.logger::flog.info("build_hospital_stay - Selected %s cases where relevant_incident = 1",
                             nrow(HS_4))

  # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$ALL_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      # [NOTE: This "&..." part of the if-condition is TEMPORARY, for reasons outlined in the documentation of apply_additional_violence_filters]
      number_of_cases_pre_filter = nrow(HS_4)
      HS_4 = autotable::apply_additional_violence_filters(dataframe = HS_4,
                                                           subtable_name = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.info("build_hospital_stay - Applied additional violence filters, removing a further %s cases",
                               number_of_cases_pre_filter - nrow(HS_4))
    }

  # 10. Filter on incidents which are 1
  HS_5a <- dplyr::filter(HS_4, (!!as.symbol("relevant_incident")) != 0)

  # 11. Filter on whether force or violence threatened/used is 1
  HS_5b <- dplyr::filter(HS_5a, throrfor == 1)

  # 12. Filter out those who responded "Don't know" to "Did YOU have any medical attention?" (i.e. whemotk = 1) and those who refused to answer (whemotl = 1):
  HS_6 <- dplyr::filter(HS_5b, docatt3g != 1)
  HS_7 <- dplyr::filter(HS_6, docatt3h != 1)

  # 13. Recode those who said no to 'att' (whether received any medical attention) to 1 for nomeds (as they did not receive any medical attention)
  HS_8 <- dplyr::mutate(HS_7,
                         nomeds = dplyr::if_else(att == 0,  1, 0))

  # 14. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form) - this step is required as the question asked about whether stayed in hospital is asked on long victim form:
  HS_9 <- dplyr::filter(HS_8, vftype == 1)
  futile.logger::flog.info(base::paste0("build_hospital_stay - selected ", base::nrow(HS_9), " cases where vftype = 1"))

  # 15. The unweighted base to be listed in the table:
  unweighted_base = base::sum(HS_9$relevant_incident)
  futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                           nrow(HS_5a) - nrow(HS_5b), unweighted_base)

  # 16. Weight the data:
  HS_10 = autotable::apply_weight(HS_9, "relevant_incident", weight = weight_variable)
  futile.logger::flog.info("build_hospital_stay - Weighted the data by %s", weight_variable)

  # 17. Group cases by their values for hosp2, and then summarise for both parts:
  HS_10$hosp2  = factor(HS_10$hosp2, levels = c(0, 1))

  HS_11          = dplyr::group_by(HS_10, hosp2, .drop = FALSE)

  HS_12          = dplyr::summarise_at(HS_11, "relevant_incident", sum)

  HS_12$hosp2 = c(0, 1)
  futile.logger::flog.info("build_hospital_stay - Grouped cases by their values for hosp2")

  # 18. Generate the unrounded percentages for this year:
  HS_13 = (HS_12 / base::sum(HS_12$relevant_incident)) * 100
  futile.logger::flog.info("build_hospital_stay - Generated the unrounded percentages")

  # 19. We now want to add these unrounded percentages into our table and remove column:
  HS_14a <- base::as.data.frame(HS_13[, -1])
  
  HS_14a <- base::as.data.frame(HS_14a[-1,])
  
  # 20. Generate a name for this column, which we will later apply in Step 26:
  start_year              = substr(years_of_data[[i]], 3, 4)
  end_year                = as.numeric(start_year) + 1
  names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
  futile.logger::flog.trace("build_hospital_stay - Finished column \"%s\"", names(years_of_data)[i])
  
  # 21. We now want to add this column into our final dataframe:
  
  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
    # that case, we must create one:
    
    HS_15 = base::as.data.frame(HS_14a)
    row_of_unweighted_bases = unweighted_base
    
  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
    
    HS_15 = base::cbind(HS_15, base::as.data.frame(HS_14a))
    row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
    
  }
  
  }
  

  # 22. And add the row of unweighted bases to the bottom of the table
  TableHS <- base::rbind(HS_15, row_of_unweighted_bases)

  # 23. Changing the labels of the rownames in the current table so that the row labels in the config file can be read in correctly
  rownames(TableHS) [2] = "Unweighted_base"

  # 24. Name the rows:
  rownames(TableHS) = lapply(config$PART_THREE$VARIABLES_AND_THEIR_ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_hospital_stay - Added row labels")

  # 25. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  HS_SIG = append_significance_column(dataframe = TableHS, column_1 = ncol(TableHS), column_2 = 1,
                                       row_of_unweighted_bases = nrow(TableHS))
  futile.logger::flog.trace("build_hospital_stay - Appended a significance column, comparing the latest year of data with the previous year")
  HS_SIG2 = append_significance_column(dataframe = HS_SIG, column_1 = ncol(HS_SIG) -1, column_2 = ncol(HS_SIG) - 2,
                                       row_of_unweighted_bases = nrow(HS_SIG))
  futile.logger::flog.trace("build_hospital_stay  - Appended a significance column, comparing the latest year of data with the earliest year")  
  
  # 26. Name the columns:
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_7_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`7_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(HS_SIG2) = c(names(years_of_data),
                      name_of_7_year_test_column,
                      name_of_1_year_test_column)
  

return(HS_SIG2)  
}
