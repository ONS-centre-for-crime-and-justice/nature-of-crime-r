#'@title build_method_of_entry
#'
#'@description Builds the "Method of entry" (MOE) table for a Nature of Crime dataset.
#'
#'@details This function builds the MOE table (specified by \code{subtable}) based largely on a configuration
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
#'\code{config$OFFENCE_CODES}.
#'
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the MOE table
#'
#'@examples build_method_of_entry(config = "configs/method_of_entry/BURGLARY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_method_of_entry <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(years_of_data))) {
    
    # 3. Read in the VF datasets:
    futile.logger::flog.info("build_method_of_entry - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variables:
    weight_variable = config$TABLE$WEIGHT_VARIABLES[[i]][[2]]
    
    # 5. Remove zero-weighted cases from the dataset:
    MOE_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                                 weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Method of entry", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; MOE_1 will become MOE_2, and then become MOE_3, and then
    #  MOE_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_method_of_entry - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(MOE_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    MOE_2 = dplyr::filter(MOE_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_method_of_entry - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(MOE_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$TABLE$OFFENCE_CODES[[subtable]][[2]]
    MOE_3 = dplyr::mutate(MOE_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_method_of_entry - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    MOE_4 = dplyr::filter(MOE_3, relevant_incident == 1)
    futile.logger::flog.info("build_method_of_entry - Selected %s cases where relevant_incident = 1",
                             nrow(MOE_4))
    
    # 9. We now want to exclude the missing cases. Because all but one rows of the table are based on values of the door variable,
    #     while one row is based on values of the falseent variable, we will have to handle them separately, since their
    #     numbers of valid cases differ in general:
    
    MOE_5_door = dplyr::filter(MOE_4, !is.na(door))
    MOE_5_falseent   = dplyr::filter(MOE_4, !is.na(falseent))
    
    # 10. The unweighted base to be listed in the table:
    unweighted_base = base::sum(MOE_5_door$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(MOE_4) - nrow(MOE_5_door), unweighted_base)
    
    # 11. Fetch the variables needed for the table:
    MOE_variables = unlist(lapply(config$TABLE$ROW_VARIABLES, `[[`, 1))
    
    # 12. Weight the data:
    MOE_6_door = autotable::apply_weight(MOE_5_door, base::c("relevant_incident", MOE_variables), weight = weight_variable)
    MOE_6_falseent = autotable::apply_weight(MOE_5_falseent, base::c("relevant_incident", "falseent"), weight = weight_variable)
    futile.logger::flog.trace("build_method_of_entry - Weighted the data by %s", weight_variable)

    # 13. Generate the unrounded percentages for this year:
    weighted_totals_door = dplyr::summarise_at(MOE_6_door, MOE_variables, sum, na.rm = TRUE)
    weighted_totals_falseent = dplyr::summarise_at(MOE_6_falseent, "falseent", sum, na.rm = TRUE)
    MOE_7_door       = (weighted_totals_door / base::sum(MOE_6_door$relevant_incident)) * 100
    MOE_7_falseent        = (weighted_totals_falseent / base::sum(MOE_6_falseent$relevant_incident)) * 100
    futile.logger::flog.trace("build_method_of_entry - Generated the unrounded percentages")
    
    # 14. Combine the two parts as necessary and in order to get one column of our dataframe and remove 'falseent' from the door dataframe as this was coming out as O and replace with 'falseent' value from falseent dataframe:
    MOE_8 <- base::as.data.frame(MOE_7_door[1, -7])
    
    MOE_9 = base::c(MOE_8[1, 1:6], MOE_7_falseent, MOE_8[1, 7:15])
    
    # 15. We now want to add these unrounded percentages into our table and transpose rows to a column:
    MOE_10 = base::as.data.frame(MOE_9)
    
    MOE_11 = base::as.data.frame(base::t(MOE_10))
    
    # 16. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_method_of_entry - Finished column \"%s\"", names(years_of_data)[i])
    
    # 17. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      MOE_12 = base::as.data.frame(MOE_11)
      row_of_unweighted_bases = unweighted_base
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      MOE_12 = base::cbind(MOE_12, base::as.data.frame(MOE_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
      
    }
    
  }
  
  # 18. And add the row of unweighted bases to the bottom of the table
  TableMOE <- base::rbind(MOE_12, row_of_unweighted_bases)
  
  # 19. Split the rows into door rows, other door row, window rows and other: 
  
  TableMOE_door   = as.data.frame(TableMOE[grepl("door|false", rownames(TableMOE)), , drop = FALSE])
  TableMOE_window = as.data.frame(TableMOE[grepl("win", rownames(TableMOE)), , drop = FALSE])
  index_of_other_door_row = grep("dooroth2", rownames(TableMOE_door))
  other_door_row          = TableMOE_door[index_of_other_door_row, , drop=FALSE ]
  index_of_other_row = grep("otherent", rownames(TableMOE))
  other_row          = TableMOE[index_of_other_row, , drop=FALSE]
  bases              = TableMOE[nrow(TableMOE), ]
  
  # 20. Drop the other door row already in door rows as need this to be separate at the bottom of door rows
  TableMOE_door = TableMOE_door[-10, ,  drop=FALSE]
  
  # 21. Order the two halves separately ("Door" and "Window" will always be the top rows of their sections by default, as the rest of their rows are sub-categories of them):
  
  TableMOE_door_ordered   = as.data.frame(TableMOE_door[base::order(-TableMOE_door[, base::ncol(TableMOE_door)]), , drop = FALSE])
  TableMOE_window_ordered = as.data.frame(TableMOE_window[base::order(-TableMOE_window[, base::ncol(TableMOE_window)]), , drop = FALSE])
  
  # 22. If the Window row has a greater percentage than the Door row, the window-related half should be on top; else the door-related half should be on top:
  
  if (TableMOE_window_ordered[1, ncol(TableMOE_window_ordered)] > TableMOE_door_ordered[1, ncol(TableMOE_door_ordered)]) {
    
    TableMOE_ordered = rbind(TableMOE_window_ordered, TableMOE_door_ordered, other_door_row, other_row, bases)
    
  } else {
    
    TableMOE_ordered = rbind(TableMOE_door_ordered, other_door_row, TableMOE_window_ordered, other_row, bases)
    
  }  
  
  # 23. Changing the labels of the rownames in the current table so that the row labels in the config file can be read in correctly
  rownames(TableMOE_ordered) [17] = "Unweighted_base"
  
  # 23. Name the rows:
  for (Row in c(1:length(rownames(TableMOE_ordered)))) {
    
    rownames(TableMOE_ordered)[Row] <- unlist(config$TABLE$VARIABLES_AND_THEIR_ROW_LABELS[grepl(rownames(TableMOE_ordered)[Row], config$TABLE$VARIABLES_AND_THEIR_ROW_LABELS)])[2]
    
  }
  futile.logger::flog.info("build_form_of_medical_attention - Added row labels")
  
  # 24. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  MOE_SIG = append_significance_column(dataframe = TableMOE_ordered, column_1 = ncol(TableMOE_ordered), column_2 = 1,
                                       row_of_unweighted_bases = nrow(TableMOE_ordered))
  futile.logger::flog.trace("build_method_of_entry - Appended a significance column, comparing the latest year of data with the previous year")
  MOE_SIG2 = append_significance_column(dataframe = MOE_SIG, column_1 = ncol(MOE_SIG) -1, column_2 = ncol(MOE_SIG) - 2,
                                        row_of_unweighted_bases = nrow(MOE_SIG))
  futile.logger::flog.trace("build_method_of_entry  - Appended a significance column, comparing the latest year of data with the earliest year")  
  
  # 25. Name the columns:
  
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$") 
  
  
  colnames(MOE_SIG2) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  
  return(MOE_SIG2)  
}
