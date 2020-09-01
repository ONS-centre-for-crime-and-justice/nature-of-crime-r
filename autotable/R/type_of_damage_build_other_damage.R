#'@title build_other_damage_caused
#'
#'@description Builds the "Type of Damage caused" (ODC) for the criminal damage crime types 
#'Table for a Nature of Crime dataset.
#'
#'@details This function builds the ODC part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the VD part of the specified subtable.
#'
#'@examples build_other_damage_caused(config = "configs/type_of_damage/CRIMINAL_DAMAGE", master_config = master_config, subtable = 1)
#'
#'@export

type_of_damage_build_other_damage <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_other_damage_caused - Reading in source data from %s",
                             List_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, List_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_other_damage_caused - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    ODC_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = Weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Type of Damage Caused", which
    #  describes the subtable component that we want this function to build. Each step in the code will build on the previous
    #  one from now onwards; ODC_1 will become ODC_2, and then ODC_2 will become ODC_3, and so on, until we have the
    #  dataframe in the form that we want)
    futile.logger::flog.info("build_other_damage_caused - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(ODC_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    ODC_2 = dplyr::filter(ODC_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_other_damage_caused - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(ODC_2))
    
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    # subtable <- 1
    Offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]] 
    ODC_3 = dplyr::mutate(ODC_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.info("build_other_damage_caused - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    ODC_3b = dplyr::filter(ODC_3, filthome == 1)
    ODC_4 = dplyr::filter(ODC_3, relevant_incident == 1)
    futile.logger::flog.info("build_other_damage_caused - Selected %s cases where relevant_incident = 1",
                             nrow(ODC_4))
    futile.logger::flog.info("build_other_damage_caused - Selected %s cases where filthome = 1",
                             nrow(ODC_3b))
    
    # 9. Filter for a valid response and extract the unweighted base:
  
    ODC_4 = dplyr::filter(ODC_4, homewind == 1 | homedoor == 1 | homegraf == 1 | homesoil == 1 | homeoth == 1 |
                          wallgraf == 1 | wallbrke == 1 | walloth == 1 | shedwind == 1 | sheddoor == 1 | shedsoil == 1 | 
                          shedgraf == 1 | shedoth == 1 | hmdmoth == 1)
    
    Unweighted_base = base::sum(ODC_4$relevant_incident)
    futile.logger::flog.info("build_other_damage_caused - Determined the unweighted base as %s",
                             Unweighted_base)
    
    # 10. Weight the data:
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    ODC_5 = autotable::apply_weight(ODC_4, base::c("relevant_incident", Variables_for_rows), weight = Weight_variable)
    # futile.logger::flog.info("build_other_damage_caused - Weighted the data by %s", Weight_variable)
    
    # 11. Generate the unrounded percentages for this year:
    Weighted_totals = dplyr::summarise_at(ODC_5, Variables_for_rows, sum, na.rm = TRUE)
    ODC_6         = (Weighted_totals / base::sum(ODC_5$relevant_incident)) * 100
    futile.logger::flog.info("build_other_damage_caused - Generated the unrounded percentages")
    
    # 12. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_other_damage_caused - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 13. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      ODC_7 = base::as.data.frame(base::t(ODC_6))
      Row_of_unweighted_bases = Unweighted_base
      futile.logger::flog.info("build_other_damage_caused - Created ODC_9 and added column to the data frame")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      ODC_7 = base::cbind(ODC_7, base::as.data.frame(base::t(ODC_6)))
      Row_of_unweighted_bases = base::c(Row_of_unweighted_bases, Unweighted_base)
      futile.logger::flog.info("build_other_damage_caused - Added column to the ODC_9 dataframe")
      
    }
    
  }
  
  # 14. Bind the row of unweighted bases to the bottom of the table:
  ODC_8 = base::rbind(ODC_7, Row_of_unweighted_bases)
  futile.logger::flog.info("build_other_damage_caused - Added the row of unweighted bases")
  
  # 15. Name the rows:
  rownames(ODC_8) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_other_damage_caused - Added row labels")
  
  
  # 16. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of 
  #     magnitudes, we always want the bottom two rows to be the "Other emotional response" and then the unweighted bases:
  Index_of_Other_row = grep("hmdmoth", rownames(ODC_7)) 
  Index_of_Home_row = grep("homeoth", rownames(ODC_7))
  Index_of_Wall_row = grep("walloth", rownames(ODC_7))
  Index_of_Shed_row = grep("shedoth", rownames(ODC_7))
  Other_row          = ODC_8[Index_of_Other_row, ]
  Home_row          = ODC_8[Index_of_Home_row, ]
  Wall_row          = ODC_8[Index_of_Wall_row, ]
  Shed_row          = ODC_8[Index_of_Shed_row, ]
  Bases              = ODC_8[nrow(ODC_8), ]
  # ODC_8["House/flat",] <- 0
  # ODC_8["Wall/fence/other garden items",] <- 0
  # ODC_8["Garage/shed",] <- 0
  
  # 16.1. Order home roWs by order of magnitude 
  
  # blank_home_row <- ODC_8[c(16),]
  ODC_8_home_rows <- ODC_8[c(1:5),]
  ODC_8_home_rows_without_other = ODC_8_home_rows[-c(Index_of_Home_row, nrow(ODC_8_home_rows)),]
  ODC_8_home_rows_with_other_row_and_bases = ODC_8_home_rows_without_other[base::order(-ODC_8_home_rows_without_other[, base::ncol(ODC_8_home_rows_without_other)]), ]  
  # ODC_9_home = rbind(blank_home_row, ODC_8_home_rows_with_other_row_and_bases, Home_row)
  ODC_9_home = rbind(ODC_8_home_rows_with_other_row_and_bases, Home_row)
  
  # 16.2. Order wall rows by order of magnitude 
  # 
  # blank_wall_row <- ODC_8[c(17),]
  ODC_8_wall_rows <- ODC_8[c(6:8),]
  ODC_8_wall_rows_without_other = ODC_8_wall_rows[-c(Index_of_Wall_row, nrow(ODC_8_wall_rows)),]
  ODC_8_wall_rows_with_other_row_and_bases = ODC_8_wall_rows_without_other[base::order(-ODC_8_wall_rows_without_other[, base::ncol(ODC_8_wall_rows_without_other)]), ]  
  # ODC_9_wall = rbind(blank_wall_row, ODC_8_wall_rows_with_other_row_and_bases, Wall_row)
  ODC_9_wall = rbind(ODC_8_wall_rows_with_other_row_and_bases, Wall_row)
  
  # 16.3. Order shed rows by order of magnitude 
  
  # blank_shed_row <- ODC_8[c(18),]
  ODC_8_shed_rows <- ODC_8[c(9:13),]
  ODC_8_shed_rows_without_other = ODC_8_shed_rows[-c(Index_of_Shed_row, nrow(ODC_8_shed_rows)),]
  ODC_8_shed_rows_with_other_row_and_bases = ODC_8_shed_rows_without_other[base::order(-ODC_8_shed_rows_without_other[, base::ncol(ODC_8_shed_rows_without_other)]), ]  
  # ODC_9_shed = rbind(blank_shed_row, ODC_8_shed_rows_with_other_row_and_bases, Shed_row)
  ODC_9_shed = rbind(ODC_8_shed_rows_with_other_row_and_bases, Shed_row)
  
  
  # 16.4. Join together   
  ODC_9                             = rbind(ODC_9_home, ODC_9_wall, ODC_9_shed, Other_row, Bases)
  # ODC_9["House/flat",] <- " "
  # ODC_9["Wall/fence/other garden items",] <- " "
  # ODC_9["Garage/shed",] <- " "
  
  futile.logger::flog.info("build_other_damage_caused - Ordered rows by the magnitude of their percentages in the latest year of data")
  
  # 17. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  ODC_10 = append_significance_column(dataframe = ODC_9, column_1 = ncol(ODC_9), column_2 = 1,
                                      row_of_unweighted_bases = nrow(ODC_9))
  futile.logger::flog.info("build_other_damage_caused - Appended a significance column, comparing the latest year of data with the previous year")
  ODC_11 = append_significance_column(dataframe = ODC_10, column_1 = ncol(ODC_10) - 1, column_2 = ncol(ODC_10) - 2,
                                      row_of_unweighted_bases = nrow(ODC_10))
  futile.logger::flog.info("build_other_damage_caused - Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 18. Name the columns:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(ODC_11) = c(names(Years_of_data),
                        name_of_10_year_test_column,
                        name_of_1_year_test_column)
  
  futile.logger::flog.info("build_other_damage_caused - Added column labels")
  
  # 19. Return the final dataframe:
  return(ODC_11)
  
}