#'@title build_where_incident_occurred
#'
#'@description Builds the "Where incident occurred" (WIO) table for a Nature of Crime dataset.
#'
#'@details This function builds the WIO table (specified by \code{subtable}) based largely on a configuration
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
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the WIO table
#'
#'@examples build_where_incident_occured(config = "configs/where_incident_occured/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_where_incident_occurred <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(years_of_data))) {
    
    # 3. Read in the VF datasets:
    futile.logger::flog.info("build_where_incident_occurred - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variables:
    weight_variable = config$TABLE$WEIGHT_VARIABLES[[i]][[2]]
    
    # 5. Remove zero-weighted cases from the dataset:
    WIO_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                                 weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage perceived crime", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; WIO_1 will become WIO_2, and then become WIO_3, and then
    #  WIO_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_where_incident_occurred - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(WIO_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    WIO_2 = dplyr::filter(WIO_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_where_incident_occurred - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(WIO_2))
    
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    
    offence_codes = config$TABLE$OFFENCE_CODES[[subtable]][[2]]
    WIO_3 = dplyr::mutate(WIO_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_where_incident_occurred - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    WIO_4 = dplyr::filter(WIO_3, relevant_incident == 1)
    futile.logger::flog.info("build_where_incident_occurred - Selected %s cases where relevant_incident = 1",
                             nrow(WIO_4))
    
    # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(WIO_4)
      WIO_4 = autotable::apply_additional_violence_filters(dataframe = WIO_4,
                                                           subtable_name = config$TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("build_where_incident_occurred - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(WIO_4))
    }
    
    # 10. Filter out missings for 'newloc1a' and any coded as 'NA' (location of incident):
    WIO_5 <- dplyr::filter(WIO_4, (newloc1a <10))
    
    # 11. The unweighted base to be listed in the table:
    unweighted_base = base::sum(WIO_5$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(WIO_4) - nrow(WIO_5), unweighted_base)
    
    # 12. Weight the data:
    WIO_6 = autotable::apply_weight(WIO_5, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("build_where_incident_occurred - Weighted the data by %s", weight_variable)
    
    # 13. Group cases by their values for newloc1a, and then summarise for both parts:
    WIO_6$newloc1a  = factor(WIO_6$newloc1a, levels = c(1, 2, 3, 4, 5, 6))
    
    WIO_7          = dplyr::group_by(WIO_6, newloc1a, .drop = FALSE)
    
    WIO_8          = dplyr::summarise_at(WIO_7, "relevant_incident", sum)
    
    WIO_8$newloc1a = c(1, 2, 3, 4, 5, 6)
    futile.logger::flog.info("build_where_incident_occurred - Grouped cases by their values for drink")
    
    # 14. Generate the unrounded percentages for this year:
    WIO_9 = (WIO_8 / base::sum(WIO_8$relevant_incident)) * 100
    futile.logger::flog.info("build_where_incident_occurred - Generated the unrounded percentages")
    
    WIO_10 <- base::as.data.frame(WIO_9[, -1])
    
    # 15. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_where_incident_occurred - Finished column \"%s\"", names(years_of_data)[i])
    
    # 16. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      WIO_11 = base::as.data.frame(WIO_10)
      row_of_unweighted_bases = unweighted_base
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      WIO_11 = base::cbind(WIO_11, base::as.data.frame(WIO_10))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
      
    }
    
  }
  
  # 17. And add the row of unweighted bases to the bottom of the table
  TableWIO <- base::rbind(WIO_11, row_of_unweighted_bases)
  
  # 18. Name the rows:
  
  rownames(TableWIO) = config$TABLE$ROW_LABELS
  
  # 19. We now wish to order the rows by the magnitude of their percentages in the latest year of data. However, regardless of 
  #     magnitudes, we always want the bottom two rows to be the "Other injury", "No physical injury" and then the unweighted bases:
  index_of_other_row = grep("Other location", rownames(TableWIO))
  other_row          = TableWIO[index_of_other_row, ]
  bases              = TableWIO[nrow(TableWIO), ]
  
  TableWIO_without_other_row_and_bases = TableWIO[-c(index_of_other_row, nrow(TableWIO)),]
  TableWIO_with_other_row_and_bases    = TableWIO_without_other_row_and_bases[base::order(-TableWIO_without_other_row_and_bases[, base::ncol(TableWIO_without_other_row_and_bases)]), ]  
  TableWIO_ordered                             = rbind(TableWIO_with_other_row_and_bases, other_row, bases)
  
  
  # 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  WIO_SIG = append_significance_column(dataframe = TableWIO_ordered, column_1 = ncol(TableWIO_ordered), column_2 = 1,
                                       row_of_unweighted_bases = nrow(TableWIO_ordered))
  futile.logger::flog.trace("build_where_incident_occurred - Appended a significance column, comparing the latest year of data with the previous year")
  WIO_SIG2 = append_significance_column(dataframe = WIO_SIG, column_1 = ncol(WIO_SIG) -1, column_2 = ncol(WIO_SIG) - 2,
                                        row_of_unweighted_bases = nrow(WIO_SIG))
  futile.logger::flog.trace("build_where_incident_occurred  - Appended a significance column, comparing the latest year of data with the earliest year")
  
  
  # 21. Name the columns:
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$") 
  
  colnames(WIO_SIG2) = c(names(years_of_data),
                         name_of_10_year_test_column,
                         name_of_1_year_test_column)
  
  # 22. We are unable to sig test the most recent year with the earliest year due to questionnaire changes. We therefore need to replace the values in this column with a ":"
  WIO_SIG2[ncol(WIO_SIG2) - 1] = ":"
  
  WIO_SIG2[nrow(WIO_SIG2), ncol(WIO_SIG2) -1] = " "
  
  return(WIO_SIG2)
}
