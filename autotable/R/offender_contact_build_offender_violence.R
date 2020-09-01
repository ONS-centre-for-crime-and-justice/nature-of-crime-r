#'@title build_offender_violence
#'
#'@description Builds the "Of those who were at home, aware and saw offender(s)" (OV) part/half of one subtable of an "Contact with Offender"
#'Table for a Nature of Crime dataset.
#'
#'@details This function builds the OC part of one subtable (specified by \code{subtable}) based largely on a configuration
#'file specified by \code{config}.
#'
#'@param config A string corresponding to the path of a YAML file, where this YAML serves as the configuration file for the
#'function. This YAML must exhibit a particular structure/format, since the function accesses certain levels and fields of it
#'by name. For more information on the format required, please see the User Guide (found in the "resources" folder of the
#'project) or inspect a valid YAML file from previous successful runs.
#'
#'@param master_config A string corresponding to a folder path whereat the VF datasets for each year are stored as .csv
#'files.
#'
#'@param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#'then the subtable considered will be that corresponding to the \code{n}th item in
#'\code{config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES}.
#'
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the OC part of the specified subtable.
#'
#'@examples build_offender_violence(config = "configs/type_of_damage/BURGLARY.yaml", master_config = master_config, subtable = 1)
#'
#'@export


offender_contact_build_offender_violence <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_offender_violence - Reading in source data from %s",
                             List_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, List_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_offender_violence - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    OV_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = Weight_variable)
    futile.logger::flog.info("build_offender_violence - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(OV_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    OV_2 = dplyr::filter(OV_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_offender_violence - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(OV_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    Offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    OV_3 = dplyr::mutate(OV_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.info("build_offender_violence - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    OV_4 = dplyr::filter(OV_3, relevant_incident == 1)
    futile.logger::flog.info("build_offender_violence - Selected %s cases where relevant_incident = 1",
                             nrow(OV_4))

    
    # 9. Filter out further relevant cases by their response to contype1:
    OV_5 = dplyr::filter(OV_4, contype1 == 4)
    futile.logger::flog.info("build_offender_violence - Selected %s cases where contype1 == 4",
                             nrow(OV_5))
    
    # 10. Extract the unweighted base:
    # Unweighted_base = base::sum(OV_5$relevant_incident)
    # futile.logger::flog.info("build_offender_violence - Determined the unweighted base as %s",
    #                          Unweighted_base)
    # 
    
    # 10. 1 Recode the appropriate variables for the tables based on their response to athome2 and contype1 
    
    OV_5 = dplyr::mutate(OV_5, yesviofor = dplyr::if_else(throrfor == 1, 1, NULL))
    OV_5 = dplyr::mutate(OV_5, yesthreat = dplyr::if_else(thrforvio == 1, 1, NULL))
    OV_5 = dplyr::mutate(OV_5, nothreat = dplyr::if_else(thrforvio == 2, 1, NULL))
    OV_5 = dplyr::mutate(OV_5, yesuse = dplyr::if_else(useforvio == 1, 1, NULL))
    OV_5 = dplyr::mutate(OV_5, nouse = dplyr::if_else(useforvio == 2, 1, NULL))
    OV_5 = dplyr::mutate(OV_5, noviofor = dplyr::if_else(throrfor == 2, 1, NULL))
    
    # ...and extract the unweighted base
    
    Unweighted_base = filter(OV_5, yesviofor ==  1 | yesthreat == 1 | nothreat == 1, yesuse == 1 | nouse == 1 | noviofor)
    Unweighted_base = nrow(Unweighted_base)
    futile.logger::flog.info("build_offender_violence - Determined the unweighted base as %s",
                                                    Unweighted_base)
    
    # 10. 2 Extract the bases for the threat and use violence variables to calculate percentages accurately 
    
    yesviofor = filter(OV_5, yesviofor == 1)
    yesthreat = filter(OV_5, yesthreat == 1)
    yesuse = filter(OV_5, yesuse == 1)
    noviofor = filter(OV_5, noviofor == 1)
    
    threat_base = filter(OV_5, yesthreat == 1 | nothreat == 1)
    threat_base = nrow(threat_base)
    
    use_base = filter(OV_5, yesuse == 1 | nouse == 1)
    use_base = nrow(use_base)
    futile.logger::flog.info("build_offender_violence - Recoded variables and extracted bases")
    
    # 11. Weight the data:
    # Note: Due to differences in the variable labels, this if statement helps the code run the correct variables for the correct years
    # Part two of the config reference to the 2016 - 2017/ 2017-2018 variables and part three uses the variables for the preceding years
    # and following years 
    
    Variables_for_rows = unlist(lapply(config$PART_TWO$ROW_LABELS, `[[`, 1))
    OV_6 = autotable::apply_weight(OV_5, base::c("relevant_incident", Variables_for_rows), weight = Weight_variable)
    
  
    # 12. Generate the unrounded percentages for this year:
    Weighted_totals = dplyr::summarise_at(OV_5, Variables_for_rows, sum, na.rm = TRUE)
    threat_totals = Weighted_totals[2:3]
    use_totals    = Weighted_totals[4:5]
    OV_6        = (Weighted_totals / base::sum(OV_5$relevant_incident)) * 100
    OV_6_threat = (threat_totals / threat_base) * 100
    OV_6_use = (use_totals / use_base) * 100
    OV_6[2:3] <- OV_6_threat
    OV_6[4:5] <- OV_6_use
    futile.logger::flog.info("build_offender_violence - Generated the unrounded percentages")
    
    
    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_offender_violence - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      OV_7 = base::as.data.frame(base::t(OV_6))
      Row_of_unweighted_bases = Unweighted_base
      futile.logger::flog.info("build_offender_violence - Added column to the OV_8 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      OV_7 = base::cbind(OV_7, base::as.data.frame(base::t(OV_6)))
      Row_of_unweighted_bases = base::c(Row_of_unweighted_bases, Unweighted_base)
      futile.logger::flog.info("build_offender_violence - Added column to the ODC_9 dataframe")
      
    }
    
  }
  
  # 15. Bind the row of unweighted bases to the bottom of the table:
  OV_8 = base::rbind(OV_7, Row_of_unweighted_bases)
  futile.logger::flog.info("build_offender_violence - Added the row of unweighted bases")
  
  # 16. Name the rows:
  rownames(OV_8) = lapply(config$PART_TWO$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_offender_violence - Added row labels")
  
  # 18. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  OV_9 = append_significance_column(dataframe = OV_8, column_1 = ncol(OV_8), column_2 = 1,
                                     row_of_unweighted_bases = nrow(OV_8))
  futile.logger::flog.info("build_offender_violence - Appended a significance column, comparing the latest year of data with the previous year")
  OV_10 = append_significance_column(dataframe = OV_9, column_1 = ncol(OV_9) - 1, column_2 = ncol(OV_9) - 2,
                                     row_of_unweighted_bases = nrow(OV_9))
  futile.logger::flog.info("build_offender_violence - Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 19. Name the columns:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(OV_10) = c(names(Years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)
  
  futile.logger::flog.info("build_offender_violence - Added column labels")
  
  # 19. 1. Remove the unneeded rows 
  
  OV_10 <- OV_10[-c(3, 5),]
  
  
  # 20. Return the final dataframe:
  return(OV_10)
  
}

