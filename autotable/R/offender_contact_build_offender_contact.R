#'@title build_offender_contact
#'
#'@description Builds the "Whether they saw the offender" (OC) part/half of one subtable of an "Contact with Offender"
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
#'@examples build_offender_contact(config = "configs/type_of_damage/BURGLARY.yaml", master_config = master_config, subtable = 1)
#'
#'@export

offender_contact_build_offender_contact <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_offender_contact - Reading in source data from %s",
                             List_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, List_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_offender_contact - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    OC_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = Weight_variable)
    futile.logger::flog.info("build_offender_contact - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(OC_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    OC_2 = dplyr::filter(OC_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_offender_contact - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(OC_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    Offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    OC_3 = dplyr::mutate(OC_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.info("build_offender_contact - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    OC_4 = dplyr::filter(OC_3, relevant_incident == 1)
    futile.logger::flog.info("build_offender_contact - Selected %s cases where relevant_incident = 1",
                             nrow(OC_4))
    
    # 9. Filter out those where vftype == 1:
    OC_5 = dplyr::filter(OC_4, vftype == 1)
    futile.logger::flog.info("build_offender_contact - Selected %s cases where vftype == 1",
                             nrow(OC_5))
    
    
    #10. Recode the appropriate variables for the tables based on their response to athome2 and contype1 
    
    OC_5 = dplyr::mutate(OC_5, somehome = dplyr::if_else(athome2 == 1, 1, NULL))
    OC_5 = dplyr::mutate(OC_5, noaware = dplyr::if_else(athome2 == 2, 1, NULL))
    
    OC_5 = dplyr::mutate(OC_5, notaware = dplyr::if_else(contype1 == 2, 1, NULL))
    OC_5 = dplyr::mutate(OC_5, yesaware = dplyr::if_else(contype1 == 3, 1, NULL))
    OC_5 = dplyr::mutate(OC_5, yessawoff = dplyr::if_else(contype1 == 4, 1, NULL))
    
    somehome = filter(OC_5, somehome == 1)
    noaware = filter(OC_5, noaware == 1)
    notaware = filter(OC_5, notaware == 1)
    yesaware = filter(OC_5, yesaware == 1)
    yessawoff = filter(OC_5, yessawoff == 1)
    
    # 10.1 Filter by response to athome2...
    
    OC_5 = filter(OC_5, somehome == 1 | noaware == 1)
    
    # 10. 2 ...and extract the unweighted base:
    Unweighted_base = base::sum(OC_5$relevant_incident)
    futile.logger::flog.info("build_offender_contact - Recoded varaibles and determined the unweighted base as %s",
                             Unweighted_base)
    
    # 11. Weight the data:
    
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    OC_6 = autotable::apply_weight(OC_5, base::c("relevant_incident", Variables_for_rows), weight = Weight_variable)
   
    # 12. Generate the unrounded percentages for this year:
    Weighted_totals = dplyr::summarise_at(OC_6, Variables_for_rows, sum, na.rm = TRUE)
    OC_7        = (Weighted_totals / base::sum(OC_6$relevant_incident)) * 100
    futile.logger::flog.info("build_offender_contact - Generated the unrounded percentages")
    

    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_offender_contact - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      OC_8 = base::as.data.frame(base::t(OC_7))
      Row_of_unweighted_bases = Unweighted_base
      futile.logger::flog.info("build_offender_contact - Added column to the OC_9 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      OC_8 = base::cbind(OC_8, base::as.data.frame(base::t(OC_7)))
      Row_of_unweighted_bases = base::c(Row_of_unweighted_bases, Unweighted_base)
      futile.logger::flog.info("build_offender_contact - Added column to the ODC_9 dataframe")
      
    }
    
  }
  
  # 15. Bind the row of unweighted bases to the bottom of the table:
  OC_9 = base::rbind(OC_8, Row_of_unweighted_bases)
  futile.logger::flog.info("build_offender_contact - Added the row of unweighted bases")
  
  # 16. Name the rows:
  rownames(OC_9) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_offender_contact - Added row labels")
  
  # 17. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  OC_10 = append_significance_column(dataframe = OC_9, column_1 = ncol(OC_9), column_2 = 1,
                                       row_of_unweighted_bases = nrow(OC_9))
  futile.logger::flog.info("build_offender_contact - Appended a significance column, comparing the latest year of data with the previous year")
  OC_11 = append_significance_column(dataframe = OC_10, column_1 = ncol(OC_10) - 1, column_2 = ncol(OC_10) - 2,
                                       row_of_unweighted_bases = nrow(OC_10))
  futile.logger::flog.info("build_offender_contact - Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 18. Name the columns:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(OC_11) = c(names(Years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)
  
  futile.logger::flog.info("build_offender_contact - Added column labels")
  
  
  # 29. Return the final dataframe:
  return(OC_11)
  
}

