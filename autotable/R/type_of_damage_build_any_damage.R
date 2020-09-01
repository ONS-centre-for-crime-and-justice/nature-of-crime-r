#'@title build_any_damage_caused
#'
#'@description Builds the "Some form of damage caused" (DC) part/half of one subtable of an " Type of damage caused" Table for a Nature of Crime dataset.
#'
#'@details This function builds the DC part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the DC part of the specified subtable.
#'
#'@examples build_any_damage_caused(config = "configs/type_of_damage/BURGLARY.yaml", master_config = master_config, subtable = 2)
#'
#'@export

type_of_damage_build_any_damage <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_any_damage_caused - Reading in source data from %s", List_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, List_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_any_damage_caused - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    DC_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = Weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Damage Caused", which describes the
    #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; DC_1 will become DC_2, and then DC_2 will become DC_3, and so on, until we have the dataframe in the form
    #  that we want)
    futile.logger::flog.info("build_any_damage_caused - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(DC_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    DC_2 = dplyr::filter(DC_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_any_damage_caused - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(DC_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    Offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    DC_3 = dplyr::mutate(DC_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.info("build_any_damage_caused - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    DC_4 = dplyr::filter(DC_3, relevant_incident == 1)
    futile.logger::flog.info("build_any_damage_caused - Selected %s cases where relevant_incident = 1",
                             nrow(DC_4))
    
    # 9. Filter out those who responded "Don't know" (i.e. damhomi = 1) and those who refused to answer (damhomj = 1):
    DC_5 = dplyr::filter(DC_4, damhomi != 1 | base::is.na(damhomi))
    DC_5 = dplyr::filter(DC_5, damhomj != 1 | base::is.na(damhomj))
    futile.logger::flog.info("build_any_damage_caused - Selected %s cases where either damhomi =/= 1 or damhomj =/= 1 ",
                             nrow(DC_5))
    
    # 10. The unweighted base to be listed in the table need to be taken out at this point:
    Unweighted_base = base::sum(DC_5$relevant_incident)
    futile.logger::flog.info("Determined the unweighted base as %s",
                             Unweighted_base)
    
    # 11. Weight the data:
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    DC_6 = autotable::apply_weight(DC_5, base::c("relevant_incident", Variables_for_rows), weight = Weight_variable)
    futile.logger::flog.info("build_any_damage_caused - Weighted the data by %s", Weight_variable)
    
    # 12. Generate the unrounded percentages for this year:
    Weighted_totals = dplyr::summarise_at(DC_6, Variables_for_rows, sum, na.rm = TRUE)
    DC_7         = (Weighted_totals / base::sum(DC_6$relevant_incident)) * 100
    futile.logger::flog.info("build_any_damage_caused - Generated the unrounded percentages")
    
    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_any_damage_caused - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      DC_8 = base::as.data.frame(base::t(DC_7))
      Row_of_unweighted_bases = Unweighted_base
      futile.logger::flog.info("build_any_damage_caused - Created DC_8 and added column to the data frame")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      DC_8 = base::cbind(DC_8, base::as.data.frame(base::t(DC_7)))
      Row_of_unweighted_bases = base::c(Row_of_unweighted_bases, Unweighted_base)
      futile.logger::flog.info("build_any_damage_caused - Added column to the DC_8 dataframe")
      
    }
    
  }
  
  
  # 15. Bind the row of unweighted bases to the bottom of the table:
  DC_9 = base::rbind(DC_8, Row_of_unweighted_bases)
  futile.logger::flog.info("build_any_damage_caused - Added the row of unweighted bases")
  
  # 16. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  DC_10 = append_significance_column(dataframe = DC_9, column_1 = ncol(DC_9), column_2 = 1,
                                     row_of_unweighted_bases = nrow(DC_9))
  futile.logger::flog.info("build_any_damage_caused - Appended a significance column, comparing the latest year of data with the previous year")
  DC_11 = append_significance_column(dataframe = DC_10, column_1 = ncol(DC_10) - 1, column_2 = ncol(DC_10) -  2,
                                     row_of_unweighted_bases = nrow(DC_10))
  futile.logger::flog.info("build_any_damage_caused - Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 17. Name the rows:
  rownames(DC_11) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_any_damage_caused - Added row labels")
  
  # 18. Name the columns:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(DC_11) = c(names(Years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  
  futile.logger::flog.info("build_any_damage_caused - Added column labels")
  
  # 19. Return the final data frame:
  return(DC_11)
  
}