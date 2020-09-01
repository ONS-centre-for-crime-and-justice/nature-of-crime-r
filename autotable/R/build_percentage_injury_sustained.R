#'@title build_percentage_injury_sustained
#'
#'@description Builds the "Percentage injury sustained" (PIS) part of one subtable of an "Injuries sustained in
#'incidents" Table for a Nature of Crime dataset.
#'
#'@details This function builds the PIS part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'@examples build_percentage_injury_sustained(config = "configs/injuries_sustained/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_percentage_injury_sustained <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

  # Below, we will use a for-loop to process the years of data one-by-one.

  for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF datasets:
    futile.logger::flog.info("build_percentage_injury_sustained - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i+3], sep = '/')))
    
  # 4. Fetch the correct weight variables:
    weight_variable = config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]

  # 5. Remove zero-weighted cases from the dataset:
    PIS_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                               weightvar = weight_variable)
  # (Regarding the choice of variable name here: This just stands for "Percentage Injury Sustained", which describes the table component that we want
  #  this function to build. Each step in the code will build on the previous one from now; PIS_1 will become PIS_2, and then become PIS_3, and then
  #  PIS_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
  # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_percentage_injury_sustained - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(PIS_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    PIS_2 = dplyr::filter(PIS_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_percentage_injury_sustained - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(PIS_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    PIS_3 = dplyr::mutate(PIS_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_percentage_injury_sustained - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    PIS_4 = dplyr::filter(PIS_3, relevant_incident == 1)
    futile.logger::flog.info("build_percentage_injury_sustained - Selected %s cases where relevant_incident = 1",
                             nrow(PIS_4))

  # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$ALL_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      # [NOTE: This "&..." part of the if-condition is TEMPORARY, for reasons outlined in the documentation of apply_additional_violence_filters]
      number_of_cases_pre_filter = nrow(PIS_4)
      PIS_4 = autotable::apply_additional_violence_filters(dataframe = PIS_4,
                                                           subtable_name = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.info("build_percentage_injury_sustained - Applied additional violence filters, removing a further %s cases",
                               number_of_cases_pre_filter - nrow(PIS_4))
    }


    # 10. Filter out missings for 'respinj' (whether respondents suffered an injury):
    PIS_6 <- dplyr::filter(PIS_4, !is.na(respinj))

    # 11. The unweighted base to be listed in the table:
    unweighted_base = base::sum(PIS_6$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(PIS_4) - nrow(PIS_6), unweighted_base)

    # 12. Weight the data:
    PIS_7 = autotable::apply_weight(PIS_6, "relevant_incident", weight = weight_variable)
    futile.logger::flog.info("build_percentage_injury_sustained - Weighted the data by %s", weight_variable)

    # 13. Group cases by their values for respinj, and then summarise for both parts:
    PIS_7$respinj  = factor(PIS_7$respinj, levels = c(0, 1))

    PIS_8          = dplyr::group_by(PIS_7, respinj, .drop = FALSE)

    PIS_9          = dplyr::summarise_at(PIS_8, "relevant_incident", sum)

    PIS_9$respinj = c(0, 1)
    futile.logger::flog.info("build_percentage_injury_sustained - Grouped cases by their values for respinj")

    # 14. Generate the unrounded percentages for this year:
    PIS_10 = (PIS_9 / base::sum(PIS_9$relevant_incident)) * 100
    futile.logger::flog.info("build_percentage_injury_sustained - Generated the unrounded percentages")

  # 15. Start to build type of injury section of sub-section of table. Creating a 'noinj' variable for those who said yes to 'respinj' recode to no for 'noinj' and vice-versa
    TOI_1 <- dplyr::mutate(PIS_6,
                       noinj = dplyr::if_else(respinj == 0,  1, 0))

  # 16. Filter on all types of injury variables - if said 0 to all types then recode noinj to 0 - there was one case where a respondent said yes to respinj but answered 0 to all types of injury
  TOI_2 = dplyr::filter(TOI_1, !(noinj    == 0 &
                                   minbruis == 0 &
                                   sevbruis == 0 &
                                   scratch  == 0 &
                                   cuts     == 0 &
                                   stab     == 0 &
                                   bones    == 0 &
                                   Nbleed   == 0 &
                                   nose     == 0 &
                                   brteeth  == 0 &
                                   chipped  == 0 &
                                   dislo    == 0 &
                                   concuss  == 0 &
                                   facial   == 0 &
                                   internal == 0 &
                                   eyeacid  == 0 &
                                   otherinj == 0))

  # 17. Fetch the variables needed for this part of the table:
  TOI_variables = unlist(lapply(config$PART_ONE$VARIABLES_AND_THEIR_ROW_LABELS, `[[`, 1))

  # 18. Weight the data:
  TOI_3 = autotable::apply_weight(TOI_2, base::c("relevant_incident", TOI_variables), weight = weight_variable)
  futile.logger::flog.info("build_percentage_injury_sustained - Weighted the data by %s", weight_variable)

  weighted_totals <- dplyr::summarise_at(TOI_3, TOI_variables, sum, na.rm = TRUE)

  # ... and then divide through by the sum of the weights, before  multiplying by 100:
  TOI_4 = (weighted_totals / base::sum(TOI_3$relevant_incident)) * 100

  # 19. We now want to add these unrounded percentages into our table
  TOI_4a <- base::as.data.frame(base::t(TOI_4))

  # 20. Generate a name for this column, which we will later apply in Step 28:
  start_year              = substr(years_of_data[[i]], 3, 4)
  end_year                = as.numeric(start_year) + 1
  names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
  futile.logger::flog.trace("build_percentage_injury_sustained - Finished column \"%s\"", names(years_of_data)[i])
  
  # 21. We now want to add this column into our final dataframe:
  
  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
    # that case, we must create one:
    
    TOI_5 = base::as.data.frame(TOI_4a)
    row_of_unweighted_bases = unweighted_base
    
  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
    
    TOI_5 = base::cbind(TOI_5, base::as.data.frame(TOI_4a))
    row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
    
  }
  
  }
  
  # 22. Bind the row of unweighted bases to the bottom of the table:
  TablePIS = base::rbind(TOI_5, row_of_unweighted_bases)

  # 23. Changing the labels of the rownames in the current table so that the row labels in the config file can be read in correctly
  rownames(TablePIS) [19] = "Unweighted_base"

  # 24. Name the rows:
  rownames(TablePIS) = lapply(config$PART_ONE$VARIABLES_AND_THEIR_ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_percentage_injury_sustained - Added row labels")
  
  # 25. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of 
  #     magnitudes, we always want the bottom two rows to be the "Other injury", "No physical injury" and then the unweighted bases:
  index_of_other_row = grep("otherinj", rownames(TOI_5))
  index_of_noinj_row = grep("noinj", rownames(TOI_5))
  other_row          = TablePIS[index_of_other_row, ]
  noinj_row          = TablePIS[index_of_noinj_row, ]
  bases              = TablePIS[nrow(TablePIS), ]
  
  TablePIS_without_other_row_noinj_and_bases = TablePIS[-c(index_of_other_row, index_of_noinj_row, nrow(TablePIS)),]
  TablePIS_with_other_row_noinj_and_bases    = TablePIS_without_other_row_noinj_and_bases[base::order(-TablePIS_without_other_row_noinj_and_bases[, base::ncol(TablePIS_without_other_row_noinj_and_bases)]), ]  
  TablePIS_ordered                             = rbind(TablePIS_with_other_row_noinj_and_bases, other_row, noinj_row, bases)

  # 26. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  PIS_SIG = append_significance_column(dataframe = TablePIS_ordered, column_1 = ncol(TablePIS_ordered), column_2 = 1,
                                       row_of_unweighted_bases = nrow(TablePIS_ordered))
  futile.logger::flog.trace("build_percentage_injury_sustained - Appended a significance column, comparing the latest year of data with the previous year")
  PIS_SIG2 = append_significance_column(dataframe = PIS_SIG, column_1 = ncol(PIS_SIG) - 1, column_2 = ncol(PIS_SIG) - 2,
                                       row_of_unweighted_bases = nrow(PIS_SIG))
  futile.logger::flog.trace("build_percentage_injury_sustained- Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 27. Name the columns:
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_7_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`7_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(PIS_SIG2) = c(names(years_of_data),
                      name_of_7_year_test_column,
                      name_of_1_year_test_column)
  
return(PIS_SIG2)
  }
