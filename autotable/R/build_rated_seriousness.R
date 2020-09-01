#'@title build_rated_seriousness
#'
#'@description Builds the "Rated seriousness" (RS) part of one subtable of an "Perceived seriousness of incidents" Table for a Nature of Crime dataset.
#'
#'@details This function builds the RS part of one subtable (specified by \code{subtable}) based largely on a configuration
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
#'@examples build_rated_seriousness(config = "configs/perceived_seriousness/ROBBERY.yaml", master_config =
#'master_config", subtable = 1)
#'
#'@export

build_rated_seriousness <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(years_of_data))) {
    
    # 3. Read in the VF datasets:
    futile.logger::flog.info("build_rated_seriousness - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variables:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    
    # 5. Remove zero-weighted cases from the dataset:
    RS_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                                 weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Rated seriousness", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; RS_1 will become RS_2, and then become RS_3, and then
    #  RS_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_rated_seriousness - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(RS_1), nrow(VF_dataset))
    
    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    RS_2 = dplyr::filter(RS_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_rated_seriousness - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(RS_2))
   
    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    RS_3 = dplyr::mutate(RS_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_rated_seriousness - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")
    
    # 8. Select only the cases where relevant_incident = 1:
    RS_4 = dplyr::filter(RS_3, relevant_incident == 1)
    futile.logger::flog.info("build_rated_seriousness - Selected %s cases where relevant_incident = 1",
                             nrow(RS_4))
    
    # 9. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violen" in the Table's title:
    if (grepl("violen", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      # [NOTE: This "&..." part of the if-condition is TEMPORARY, for reasons outlined in the documentation of apply_additional_violence_filters]
      number_of_cases_pre_filter = nrow(RS_4)
      RS_4 = autotable::apply_additional_violence_filters(dataframe = RS_4,
                                                           subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.info("build_rated_seriousness - Applied additional violence filters, removing a further %s cases",
                               number_of_cases_pre_filter - nrow(RS_4))
    }
    
    
    # 10. Filter out missings for 'score2' (scored perceived seriousness):
    RS_5 <- dplyr::filter(RS_4, !is.na(score2))
    
    
    # 11. The unweighted base to be listed in the table:
    unweighted_base = base::sum(RS_5$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(RS_4) - nrow(RS_5), unweighted_base)
    
    # 12. Weight the data:
    RS_6 = autotable::apply_weight(RS_5, "relevant_incident", weight = weight_variable)
    futile.logger::flog.info("build_rated_seriousness - Weighted the data by %s", weight_variable)
    
    # 13. Group cases by their values for score2, and then summarise for both parts:
    RS_6$score2  = factor(RS_6$score2, levels = c(1, 2, 3))
    
    RS_7          = dplyr::group_by(RS_6, score2, .drop = FALSE)
    
    RS_8          = dplyr::summarise_at(RS_7, "relevant_incident", sum)
    
    RS_8$score2 = c(1, 2, 3)
    futile.logger::flog.info("build_rated_seriousness - Grouped cases by their values for score1")
    
    # 14. Generate the unrounded percentages for this year:
    RS_9 = (RS_8 / base::sum(RS_8$relevant_incident)) * 100
    futile.logger::flog.info("build_rated_seriousness - Generated the unrounded percentages")
    
    # 15. Generate the weighted mean for score of perceived seriousness
    RS_mean <- base::as.data.frame(stats::weighted.mean(RS_6$score1, RS_6[[weight_variable]], na.rm=TRUE))
    
    # And we'll need the standard deviation of totdamag for the means sig-testing that will be done later.
    if (i == 1) { 
      standard_deviations_of_rated_seriousness = sqrt(sum(RS_6$relevant_incident * ((RS_6$score1 - as.numeric(RS_mean))^2)) / (sum(RS_6$relevant_incident) - 1))
      
    } else {
      standard_deviations_of_rated_seriousness = c(standard_deviations_of_rated_seriousness,
                                            sqrt(sum(RS_6$relevant_incident * ((RS_6$score1 - as.numeric(RS_mean))^2)) / (sum(RS_6$relevant_incident) - 1)))
    }
    
    # 16. We now want to add these unrounded percentages into our table and remove column:
    RS_10 <- base::as.data.frame(RS_9[ ,-1])
    
    # 17. Making column names in dataframe identifical to allow the row bind in next step
    names(RS_mean) <- names(RS_10)
    
    # 18. Bind the unrounded percentages and weighted mean into the same data frame
    RS_11 <- base::rbind(RS_10, RS_mean)
    
    # 19. Generate a name for this column, which we will later apply in Step 24:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_rated_seriousness - Finished column \"%s\"", names(years_of_data)[i])
    
    # 20. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      RS_12 = base::as.data.frame(RS_11)
      row_of_unweighted_bases = unweighted_base
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      RS_12 = base::cbind(RS_12, base::as.data.frame(RS_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)
      
    }
  
  }
  
  # 21. And add the row of unweighted bases to the bottom of the table
  TableRS <- base::rbind(RS_12, row_of_unweighted_bases)
  
  # 22. Name the rows:
  rownames(TableRS) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 1)
  futile.logger::flog.info("build_rated_seriousness - Added row labels")
  
  # 23. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  RS_SIG = append_significance_column(dataframe = TableRS, column_1 = ncol(TableRS), column_2 = 1,
                                      row_of_unweighted_bases = nrow(TableRS))
  futile.logger::flog.trace("build_rated_seriousness - Appended a significance column, comparing the latest year of data with the previous year")
 RS_SIG2 = append_significance_column(dataframe = RS_SIG, column_1 = ncol(RS_SIG) -1, column_2 = ncol(RS_SIG) - 2,
                                       row_of_unweighted_bases = nrow(RS_SIG))
  futile.logger::flog.trace("build_rated_seriousness  - Appended a significance column, comparing the latest year of data with the earliest year")  
  
  # Means sig-testing is done a bit differently however.
  means_test_for_10_years = autotable::mean_sig_test(estimate_1 = RS_SIG2[nrow(RS_SIG2) - 1, ncol(RS_SIG2) - 2],
                                                     estimate_2 = RS_SIG2[nrow(RS_SIG2) - 1, 1],
                                                     base_1     = RS_SIG2[nrow(RS_SIG2), ncol(RS_SIG2) - 2],
                                                     base_2     = RS_SIG2[nrow(RS_SIG2), 1],
                                                     sd_1       = standard_deviations_of_rated_seriousness[ncol(RS_SIG2) - 2],
                                                     sd_2       = standard_deviations_of_rated_seriousness[1],
                                                     significance_indicator   = "*",
                                                     insignificance_indicator = " ")
  means_test_for_1_year  = autotable::mean_sig_test(estimate_1 = RS_SIG2[nrow(RS_SIG2) - 1, ncol(RS_SIG2) - 2],
                                                    estimate_2 = RS_SIG2[nrow(RS_SIG2) - 1, ncol(RS_SIG2) - 3],
                                                    base_1     = RS_SIG2[nrow(RS_SIG2), ncol(RS_SIG2) - 2],
                                                    base_2     = RS_SIG2[nrow(RS_SIG2), ncol(RS_SIG2) - 3],
                                                    sd_1       = standard_deviations_of_rated_seriousness[ncol(RS_SIG2) - 2],
                                                    sd_2       = standard_deviations_of_rated_seriousness[ncol(RS_SIG2) - 3],
                                                    significance_indicator   = "*",
                                                    insignificance_indicator = " ")
  
  RS_SIG2[] = lapply(RS_SIG2, function(x) if(is.factor(x)) as.character(x) else x)
  RS_SIG2[nrow(RS_SIG2) - 1, ncol(RS_SIG2) - 1] = means_test_for_10_years
  RS_SIG2[nrow(RS_SIG2) - 1, ncol(RS_SIG2)]     = means_test_for_1_year
  
  
  # 24. Name the columns:
  
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(RS_SIG2) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  
  
  return(RS_SIG2)  
  }


    
    

  
    