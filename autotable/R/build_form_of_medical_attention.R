#'@title build_form_of_medical_attention
#'
#'@description Builds the "Build_form_of_medical_attention" (FMA) part of one subtable of an "Injuries sustained in
#'incidents" Table for a Nature of Crime dataset.
#'
#'@details This function builds the FMA part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'@examples build_form_of_medical_attention(config = "configs/injuries_sustained/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_form_of_medical_attention <-  function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(years_of_data))) {
    
  # 3. Read in the VF datasets:
    futile.logger::flog.info("build_form_of_medical_attention - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i+3], sep = '/')))
    
  # 4. Fetch the correct weight variables:
    weight_variable = config$ALL_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    
  # (Regarding the choice of variable name here: This just stands for "Form of medical attention", which describes the table component that we want
  #  this function to build. Each step in the code will build on the previous one from now; FMA_1 will become FMA_2, and then become FMA_3, and then
  #  FMA_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    
  # 5. Remove zero-weighted cases from the dataset:
    FMA_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset, 
                                                weightvar = weight_variable)
    futile.logger::flog.info("build_form_of_medical_attention - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(FMA_1), nrow(VF_dataset))
    
  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    FMA_2 = dplyr::filter(FMA_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_form_of_medical_attention - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(FMA_2))
    
  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    FMA_3 = dplyr::mutate(FMA_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_form_of_medical_attention - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built") 
    
  # 8. Select only the cases where relevant_incident = 1:
    FMA_4 = dplyr::filter(FMA_3, relevant_incident == 1)
    futile.logger::flog.info("build_form_of_medical_attention - Selected %s cases where relevant_incident = 1",
                             nrow(FMA_4))
    
  # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$ALL_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      # [NOTE: This "&..." part of the if-condition is TEMPORARY, for reasons outlined in the documentation of apply_additional_violence_filters]
      number_of_cases_pre_filter = nrow(FMA_4)
      FMA_4 = autotable::apply_additional_violence_filters(dataframe = FMA_4,
                                                          subtable_name = config$ALL_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.info("build_form_of_medical_attention - Applied additional violence filters, removing a further %s cases",
                               number_of_cases_pre_filter - nrow(FMA_4))
    }
    
  # 10. Filter on incidents which are 1 
    FMA_5a <- dplyr::filter(FMA_4, (!!as.symbol("relevant_incident")) != 0) 
    
  # 11. Filter on whether force or violence threatened/used is 1 
    FMA_5b <- dplyr::filter(FMA_5a, throrfor == 1) 
    
  # 12. Filter out those who responded "Don't know" to "Did YOU have any medical attention?" (i.e. whemotk = 1) and those who refused to answer (whemotl = 1):
    FMA_6 <- dplyr::filter(FMA_5b, docatt3g != 1) 
    FMA_7 <- dplyr::filter(FMA_6, docatt3h != 1)
    
  # 13. Recode those who said no to 'att' (whether received any medical attention) to 1 for nomeds (as they did not receive any medical attention)
    FMA_8 <- dplyr::mutate(FMA_7,
                          nomeds = dplyr::if_else(att == 0,  1, 0))
    
  # 14. The unweighted base to be listed in the table:
    unweighted_base = base::sum(FMA_8$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(FMA_5a) - nrow(FMA_5b), unweighted_base)
    
  # 15. Fetch the variables needed for this part of the table:
    FMA_variables = unlist(lapply(config$PART_TWO$ROW_VARIABLES, `[[`, 1))
    
  # 16. Weight the data:
    FMA_9 = autotable::apply_weight(FMA_8, base::c("relevant_incident", FMA_variables), weight = weight_variable)
    futile.logger::flog.info("build_form_of_medical_attention - Weighted the data by %s", weight_variable)
    
    FMA_10  <- dplyr::summarise_at(FMA_9, FMA_variables, sum, na.rm = TRUE)
    
  # 17. Generate the unrounded percentages for this year:
    FMA_11 = (FMA_10 / base::sum(FMA_9$relevant_incident)) * 100
    futile.logger::flog.info("build_form_of_medical_attention - Generated the unrounded percentages")
    
  # 18. The table we are creating sums those who have received medical attention by a dentist or a trained first aider into an 'Other form of medical attention' category
    FMA_12 <- dplyr::mutate(FMA_11, othmed = sum(dent, firstaid))
  
  # 19. Removing variables nomeds, dent and firstaid from the dataframe as these values are not used in the final table
    FMA_13 <- FMA_12[, c(-1, -5, -7)]
  
  # 20. We now want to add these unrounded percentages into our table:
    FMA_13a <- base::as.data.frame(base::t(FMA_13))
    
  # 21. Generate a name for this column, which we will later apply in Step 27:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_percentage_emotionally_affected - Finished column \"%s\"", names(years_of_data)[i])
    
  # 22. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      FMA_14 = base::as.data.frame(FMA_13a)
      row_of_unweighted_bases = unweighted_base
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      FMA_14 = base::cbind(FMA_14, base::as.data.frame(FMA_13a))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
      
    }
    
  }
  
  
  # 23. And add the row of unweighted bases to the bottom of the table
    TableFMA <- base::rbind(FMA_14, row_of_unweighted_bases)
    
  # 24. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of 
    #     magnitudes, we always want the bottom two rows to be the "Other medical attention" and then the unweighted bases:
    index_of_some_row = grep("att", rownames(FMA_14))  
    index_of_other_row = grep("othmed", rownames(FMA_14))
    some_row          = TableFMA[index_of_some_row, ]
    other_row          = TableFMA[index_of_other_row, ]
    bases              = TableFMA[nrow(TableFMA), ]
    
    
    TableFMA_without_some_other_row_and_bases = TableFMA[-c(index_of_some_row, index_of_other_row, nrow(TableFMA)),]
    TableFMA_with_some_other_row_and_bases    = TableFMA_without_some_other_row_and_bases[base::order(-TableFMA_without_some_other_row_and_bases[, base::ncol(TableFMA_without_some_other_row_and_bases)]), ]  
    TableFMA_ordered                             = rbind(some_row, TableFMA_with_some_other_row_and_bases, other_row, bases)
    
    # 24. Changing the labels of the rownames in the current table so that the row labels in the config file can be read in correctly
    rownames(TableFMA_ordered) [1] = "Some_form"
    
    rownames(TableFMA_ordered) [6] = "Unweighted_base"
    
    # 25. Name the rows:
    for (Row in c(1:length(rownames(TableFMA_ordered)))) {
      
      rownames(TableFMA_ordered)[Row] <- unlist(config$PART_TWO$VARIABLES_AND_THEIR_ROW_LABELS[grepl(rownames(TableFMA_ordered)[Row], config$PART_TWO$VARIABLES_AND_THEIR_ROW_LABELS)])[2]
      
    }
    futile.logger::flog.info("build_form_of_medical_attention - Added row labels")
    
    # 26. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
    #     the earliest year, for each row:
    FMA_SIG = append_significance_column(dataframe = TableFMA_ordered, column_1 = ncol(TableFMA_ordered), column_2 = 1,
                                         row_of_unweighted_bases = nrow(TableFMA_ordered))
    futile.logger::flog.trace("build_form_of_medical_attention - Appended a significance column, comparing the latest year of data with the previous year")
    FMA_SIG2 = append_significance_column(dataframe = FMA_SIG, column_1 = ncol(FMA_SIG) - 1, column_2 = ncol(FMA_SIG) - 2,
                                         row_of_unweighted_bases = nrow(FMA_SIG))
    futile.logger::flog.trace("build_form_of_medical_attention - Appended a significance column, comparing the latest year of data with the earliest year")  
    
  # 27. Name the columns:
    # For the sig-testing columns, we'll need to attach the correct footnote number:
    name_of_7_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`7_YEAR_TEST`, "$$Note_",
                                         grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
    name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                         grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
    
    colnames(FMA_SIG2) = c(names(years_of_data),
                        name_of_7_year_test_column,
                        name_of_1_year_test_column)
    
return(FMA_SIG2)
}