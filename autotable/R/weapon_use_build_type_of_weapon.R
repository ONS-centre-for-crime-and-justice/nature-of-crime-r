#'@title build_type_of_weapon
#'
#'@description Builds the "Type of weapon used" (ToW) part/half of one subtable of an "Use of Weapon"
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
#'@param master-config A string corresponding to a folder path whereat the VF datasets for each year are stored as .csv
#'files.
#'
#'@param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#'then the subtable considered will be that corresponding to the \code{n}th item in
#'\code{config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES}.
#'
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the OC part of the specified subtable.
#'
#'@examples build_type_of_weapon(config = "configs/use_of_weapons/BURGLARY.yaml", master config = master config", subtable = 1)
#'
#'@export

weapon_use_build_type_of_weapon <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
List_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(Years_of_data))) {
  
  # 3. Read in the VF dataset for this year:
  futile.logger::flog.info("build_type_of_weapon - Reading in source data from %s",
                           List_of_VF_datasets[i])
  VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, List_of_VF_datasets[i], sep = '/')))
  
  # 4. Fetch the correct weight variable for this year of data:
  Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
  futile.logger::flog.info("build_type_of_weapon - Using %s as the weight variable", Weight_variable)
  
  # 5. Remove zero-weighted cases from the dataset:
  ToW_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = Weight_variable)
  futile.logger::flog.info("build_type_of_weapon - Removed %s cases out of %s because their weights were 0",
                           nrow(VF_dataset) - nrow(ToW_1), nrow(VF_dataset))
  
  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
  ToW_2 = dplyr::filter(ToW_1, victarea == 1 | wherhapp == 1)
  futile.logger::flog.info("build_type_of_weapon - Selected %s cases where victarea = 1 or wherapp = 1",
                           nrow(ToW_2))
  
  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
  Offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
  ToW_3 = dplyr::mutate(ToW_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
  futile.logger::flog.info("build_type_of_weapon - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
  
  # 8. Select only the cases where relevant_incident = 1:
  ToW_4 = dplyr::filter(ToW_3, relevant_incident == 1)
  futile.logger::flog.info("build_type_of_weapon - Selected %s cases where relevant_incident = 1",
                           nrow(ToW_4))
  
  # 9. Filter out those where vftype == 1:
  ToW_5 = dplyr::filter(ToW_4, vftype == 1)
  futile.logger::flog.info("build_type_of_weapon - Selected %s cases where vftype = 1",
                           nrow(ToW_5))
  
  #10. Recode the appropriate variables for the tables based on their response to weap2, then we filter out any DKs or refusals by 
  # only selecting valid responses to these new computed variables 
  
  ToW_5 = dplyr::mutate(ToW_5, weaponused = dplyr::if_else(weap2 == 1, 1, NULL))
  ToW_5 = dplyr::mutate(ToW_5, noweaponused = dplyr::if_else(weap2 == 2, 1, NULL))
  ToW_5 = dplyr::mutate(ToW_5, notknown = dplyr::if_else(weap2 == 3, 1, NULL))
  
  weaponused = filter(ToW_5, weaponused == 1)
  noweaponused = filter(ToW_5, noweaponused == 1)
  notknown = filter(ToW_5, notknown == 1)
  
  ToW_5 = filter(ToW_5, weaponused == 1 | noweaponused == 1 | notknown == 1)
  
  # ToW_5 = filter(ToW_5, weaponused == 1)
  
  # 10. 2 ...and extract the unweighted base:
  
  Unweighted_base = base::sum(ToW_5$relevant_incident)
  futile.logger::flog.info("build_type_of_weapon - Recoded variables and determined the unweighted base as %s",
                           Unweighted_base)
  
  # 11. Weight the data:
  
  Variables_for_rows = unlist(lapply(config$PART_TWO$ROW_LABELS, `[[`, 1))
  ToW_6 = autotable::apply_weight(ToW_5, base::c("relevant_incident", Variables_for_rows), weight = Weight_variable)
  
  # 12. Generate the unrounded percentages for this year:
  Weighted_totals = dplyr::summarise_at(ToW_6, Variables_for_rows, sum, na.rm = TRUE)
  ToW_7        = (Weighted_totals / base::sum(ToW_6$relevant_incident)) * 100
  futile.logger::flog.info("build_type_of_weapon - Generated the unrounded percentages")
  
  
  # 13. Generate a name for this column, which we will later apply in Step 21:
  Start_year              = substr(Years_of_data[[i]], 3, 4)
  End_year                = as.numeric(Start_year) + 1
  names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
  futile.logger::flog.info("build_type_of_weapon - Finished column \"%s\"", names(Years_of_data)[i])
  
  # 14. We now want to add this column into our final dataframe:
  
  if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
    # that case, we must create one:
    
    ToW_8 = base::as.data.frame(base::t(ToW_7))
    Row_of_unweighted_bases = Unweighted_base
    futile.logger::flog.info("build_type_of_weapon - Added column to the ToW_9 dataframe")
    
  } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
    
    ToW_8 = base::cbind(ToW_8, base::as.data.frame(base::t(ToW_7)))
    Row_of_unweighted_bases = base::c(Row_of_unweighted_bases, Unweighted_base)
    futile.logger::flog.info("build_type_of_weapon - Added column to the ToW_8 dataframe")
    
  }
  
}

  # 15. Bind the row of unweighted bases to the bottom of the table:
  ToW_9 = base::rbind(ToW_8, Row_of_unweighted_bases)
  futile.logger::flog.info("build_type_of_weapon - Added the row of unweighted bases")

  # 16. Name the rows:
  rownames(ToW_9) = lapply(config$PART_TWO$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_type_of_weapon - Named the rows")
  
  # 17. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of 
  #     magnitudes, we always want the bottom two rows to be the "Othere" and then the unweighted bases:
  Index_of_Other_row = grep("othweap1", rownames(ToW_8))  
  Other_row          = ToW_9[Index_of_Other_row, ]
  Bases              = ToW_9[nrow(ToW_9), ]
  
  ToW_9_without_Other_row_and_bases = ToW_9[-c(Index_of_Other_row, nrow(ToW_9)),]
  ToW_10_with_Other_row_and_bases   = ToW_9_without_Other_row_and_bases[base::order(-ToW_9_without_Other_row_and_bases[, base::ncol(ToW_9_without_Other_row_and_bases)]), ]  
  ToW_10                            = rbind(ToW_10_with_Other_row_and_bases, Other_row, Bases)
  futile.logger::flog.info("build_type_of_weapon - Ordered rows by magnitude")

  # 18. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  ToW_11 = append_significance_column(dataframe = ToW_10, column_1 = ncol(ToW_10), column_2 = 1,
                                   row_of_unweighted_bases = nrow(ToW_10))
  futile.logger::flog.info("build_type_of_weapon - Appended a significance column, comparing the latest year of data with the previous year")
  ToW_12 = append_significance_column(dataframe = ToW_11, column_1 = ncol(ToW_11) - 1, column_2 = ncol(ToW_11) - 2,
                                   row_of_unweighted_bases = nrow(ToW_10))
  futile.logger::flog.info("build_type_of_weapon - Appended a significance column, comparing the latest year of data with the earliest year")

  # 19. Name the columns:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(ToW_12) = c(names(Years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)
  
  futile.logger::flog.info("build_type_of_weapon - Added column labels")


  # 20. Return the final dataframe:
  return(ToW_12)

}
