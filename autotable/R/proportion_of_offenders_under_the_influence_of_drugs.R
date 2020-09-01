#'@title proportion_of_offenders_under_the_influence_of_drugs
#'
#'@description Builds the "Proportion that offender under influence of drugs" (IoD) sub table for a Nature of Crime dataset.
#'
#'@details This function builds the IoD table (specified by \code{subtable}) based largely on a configuration
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
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the POD table
#'
#'@examples proportion_of_offenders_under_the_influence_of_drugs(config = "configs/proportion_of_offenders_being_under_the_influence/ROBBERY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

proportion_of_offenders_under_the_influence_of_drugs <- function(config, master_config, subtable) {

  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

  # Below, we will use a for-loop to process the years of data one-by-one.

  for (i in c(1:length(years_of_data))) {

    # 3. Read in the VF datasets:
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

    # 4. Fetch the correct weight variables:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]

    # 5. Remove zero-weighted cases from the dataset:
    IoD_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Influence of drugs", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; IoD_1 will become IoD_2, and then become IoD_3, and then
    #  IoD_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(IoD_1), nrow(VF_dataset))

    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    IoD_2 = dplyr::filter(IoD_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(IoD_2))

    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    IoD_3 = dplyr::mutate(IoD_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

    # 8. Select only the cases where relevant_incident = 1:
    IoD_4 = dplyr::filter(IoD_3, relevant_incident == 1)
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Selected %s cases where relevant_incident = 1",
                             nrow(IoD_4))

    # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    IoD_5 = dplyr::filter(IoD_4, vftype == 1)
    futile.logger::flog.trace("build_time_of_day - Selected %s cases where vftype = 1", nrow(IoD_5))

    # 10. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(IoD_5)
      IoD_5 = autotable::apply_additional_violence_filters(dataframe = IoD_5,
                                                           subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("proportion_of_offenders_being_under_the_influence_of_drugs - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(IoD_5))
    }

    # 11. Filter out missings on the 'drug' variable:
    IoD_6 = dplyr::filter(IoD_5, !is.na(drug))

    # 12. Grab the unweighted base to be listed in the table:
    unweighted_base = base::sum(IoD_6$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(IoD_5) - nrow(IoD_6), unweighted_base)

    # 13. Weight the data:
    IoD_7 = autotable::apply_weight(IoD_6, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("proportion_of_offenders_being_under_the_influence_of_drugs - Weighted the data by %s", weight_variable)

    # 14. Group cases by their values for drug, and then summarise for both parts:
    IoD_7$drug = factor(IoD_7$drug, levels = c(1, 2, 9))

    IoD_8 = dplyr::group_by(IoD_7, drug, .drop = FALSE)

    IoD_9 = dplyr::summarise_at(IoD_8, "relevant_incident", sum)
    IoD_9$drug = c(1, 2, 9)

    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Grouped cases by their values for drug")

    # 15. Generate the unrounded percentages for this year:
    IoD_10 = (IoD_9 / base::sum(IoD_9$relevant_incident)) * 100
    futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Generated the unrounded percentages")

    IoD_11 <- base::as.data.frame(IoD_10$relevant_incident)

    # 16. Generate a name for this column, which we will later apply in Step :
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("proportion_of_offenders_being_under_the_influence_of_drugs - Finished column \"%s\"", names(years_of_data)[i])

    # 17. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      IoD_12 = base::as.data.frame(IoD_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      IoD_12 = base::cbind(IoD_12, base::as.data.frame(IoD_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

  }

  # 18. And add the row of unweighted bases to the bottom of the table
  IoD_13 = base::rbind(IoD_12, row_of_unweighted_bases)

  # 19. Name the rows:
  rownames(IoD_13) = config$PART_TWO$ROW_LABELS
  futile.logger::flog.info("proportion_of_offenders_being_under_the_influence_of_drugs - Added row labels")

  # 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  IoD_14 = append_significance_column(dataframe = IoD_13, column_1 = ncol(IoD_13), column_2 = 1,
                                      row_of_unweighted_bases = nrow(IoD_13))
  futile.logger::flog.trace("proportion_of_offenders_being_under_the_influence_of_drugs - Appended a significance column, comparing the latest year of data with the previous year")
  IoD_15 = append_significance_column(dataframe = IoD_14, column_1 = ncol(IoD_14) - 1, column_2 = ncol(IoD_14) - 2,
                                      row_of_unweighted_bases = nrow(IoD_14))
  futile.logger::flog.trace("proportion_of_offenders_being_under_the_influence_of_drugs  - Appended a significance column, comparing the latest year of data with the earliest year")

  # 21. Name the columns:
  
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$") 
  
  colnames(IoD_15) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)
  

  return(IoD_15)

}
