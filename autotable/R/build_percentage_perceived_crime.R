#'@title build_percentage_perceived_crime
#'
#'@description Builds the "Whether perceived incident as crime" (PPC) part of one subtable of an "Perceived seriousness of incidents" Table for a Nature of Crime dataset.
#'
#'@details This function builds the PPC part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'\code{config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES}.
#'
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the PIS part of the specified subtable.
#'
#'@examples build_percentage_perceived_crime(config = "configs/perceived_seriousness/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_percentage_perceived_crime <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(years_of_data))) {
    
    # 3. Read in the VF datasets:
    futile.logger::flog.info("build_percentage_perceived_crime - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variables:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    
    # 5. Remove zero-weighted cases from the dataset:
    PPC_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                                weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage perceived crime", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; PPC_1 will become PPC_2, and then become PPC_3, and then
    #  PPC_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_percentage_perceived_crime - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(PPC_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    PPC_2 = dplyr::filter(PPC_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_percentage_perceived_crime - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(PPC_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    PPC_3 = dplyr::mutate(PPC_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_percentage_perceived_crime - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    PPC_4 = dplyr::filter(PPC_3, relevant_incident == 1)
    futile.logger::flog.info("build_percentage_perceived_crime - Selected %s cases where relevant_incident = 1",
                             nrow(PPC_4))
    
    # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violen" in the Table's title:
    if (grepl("violen", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      # [NOTE: This "&..." part of the if-condition is TEMPORARY, for reasons outlined in the documentation of apply_additional_violence_filters]
      number_of_cases_pre_filter = nrow(PPC_4)
      PPC_4 = autotable::apply_additional_violence_filters(dataframe = PPC_4,
                                                          subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.info("build_percentage_perceived_crime - Applied additional violence filters, removing a further %s cases",
                               number_of_cases_pre_filter - nrow(PPC_4))
    }
    
    # 10. Filter out missings for 'crime' and any coded as '9' (whether incident perceived as a crime):
    PPC_5 <- dplyr::filter(PPC_4, !is.na(crime))
    
    PPC_5 <- dplyr::filter(PPC_5, (crime <8))
    
    # 11. The unweighted base to be listed in the table:
    unweighted_base = base::sum(PPC_5$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(PPC_4) - nrow(PPC_5), unweighted_base)
    
    # 12. Weight the data:
    PPC_6 = autotable::apply_weight(PPC_5, "relevant_incident", weight = weight_variable)
    futile.logger::flog.info("build_percentage_perceived_crime - Weighted the data by %s", weight_variable)
    
    # 13. Group cases by their values for crime, and then summarise for both parts:
    PPC_6$crime  = factor(PPC_6$crime, levels = c(1, 2, 3))
    
    PPC_7          = dplyr::group_by(PPC_6, crime, .drop = FALSE)
    
    PPC_8          = dplyr::summarise_at(PPC_7, "relevant_incident", sum)
    
    PPC_8$crime = c(1, 2, 3)
    futile.logger::flog.info("build_percentage_perceived_crime - Grouped cases by their values for crime")
    
    # 14. Generate the unrounded percentages for this year:
    PPC_9 = (PPC_8 / base::sum(PPC_8$relevant_incident)) * 100
    futile.logger::flog.info("build_percentage_perceived_crime - Generated the unrounded percentages")
    
    # 15. We now want to add these unrounded percentages into our table and remove column:
    PPC_10 <- base::as.data.frame(PPC_9[ ,-1])
    
    # 16. Generate a name for this column, which we will later apply in Step 24:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_percentage_perceived_crime - Finished column \"%s\"", names(years_of_data)[i])
    
    # 17. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      PPC_11 = base::as.data.frame(PPC_10)
      row_of_unweighted_bases = unweighted_base
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      PPC_11 = base::cbind(PPC_11, base::as.data.frame(PPC_10))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
      
    }
    
  }
  
  # 21. And add the row of unweighted bases to the bottom of the table
  TablePPC <- base::rbind(PPC_11, row_of_unweighted_bases)
  
  # 22. Name the rows:
  rownames(TablePPC) = lapply(config$PART_TWO$ROW_LABELS, `[[`, 1)
  futile.logger::flog.info("build_percentage_perceived_crime - Added row labels")
  
  # 23. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  PPC_SIG = append_significance_column(dataframe = TablePPC, column_1 = ncol(TablePPC), column_2 = 1,
                                      row_of_unweighted_bases = nrow(TablePPC))
  futile.logger::flog.trace("build_percentage_perceived_crime - Appended a significance column, comparing the latest year of data with the previous year")
  PPC_SIG2 = append_significance_column(dataframe = PPC_SIG, column_1 = ncol(PPC_SIG) -1, column_2 = ncol(PPC_SIG) - 2,
                                       row_of_unweighted_bases = nrow(PPC_SIG))
  futile.logger::flog.trace("build_percentage_perceived_crime  - Appended a significance column, comparing the latest year of data with the earliest year")  
                                                                      
  # 24. Name the columns:
  
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(PPC_SIG2) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  
  
  return(PPC_SIG2)  
}