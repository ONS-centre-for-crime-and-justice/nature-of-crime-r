#'@title build_point_of_entry
#'
#'@description Builds the "Point of entry" (POE) table for a Nature of Crime dataset.
#'
#'@details This function builds the POE table (specified by \code{subtable}) based largely on a configuration
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
#'@return Returns a dataframe with the requisite percentages and unweighted bases for the POE table
#'
#'@examples build_point_of_entry(config = "configs/point_of_entry/BURGLARY.yaml", master_config =
#'master_config, subtable = 1)
#'
#'@export

build_point_of_entry <- function(config, master_config, subtable) {


  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

  # Below, we will use a for-loop to process the years of data one-by-one.

  for (i in c(1:length(years_of_data))) {

    # 3. Read in the VF datasets:
    futile.logger::flog.info("build_point_of_entry - Reading in source data from %s", list_of_VF_datasets[length(list_of_VF_datasets)])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

    # 4. Fetch the correct weight variables:
    weight_variable = config$TABLE$WEIGHT_VARIABLES[[i]][[2]]

    # 5. Remove zero-weighted cases from the dataset:
    POE_1 <- autotable::remove_zeroweighted_rows(df = VF_dataset,
                                                 weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage perceived crime", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; POE_1 will become POE_2, and then become POE_3, and then
    #  POE_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("build_point_of_entry - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(POE_1), nrow(VF_dataset))

    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    POE_2 = dplyr::filter(POE_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("build_point_of_entry - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(POE_2))

    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$TABLE$OFFENCE_CODES[[subtable]][[2]]
    POE_3 = dplyr::mutate(POE_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("build_point_of_entry - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

    # 8. Select only the cases where relevant_incident = 1:
    POE_4 = dplyr::filter(POE_3, relevant_incident == 1)
    futile.logger::flog.info("build_point_of_entry - Selected %s cases where relevant_incident = 1",
                             nrow(POE_4))

    # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    POE_5 = dplyr::filter(POE_4, vftype == 1)
    futile.logger::flog.trace("build_point_of_entry - Selected %s cases where vftype = 1", nrow(POE_5))

    # 10. Filter out missings for 'frontbac' and any coded as 'NA' (point of entry in incident):
    POE_6 <- dplyr::filter(POE_5, (frontbac <8))

    # 12. The unweighted base to be listed in the table:
    unweighted_base = base::sum(POE_6$relevant_incident)
    futile.logger::flog.info("Removed %s cases where NA, yielding an unweighted base of %s to report",
                             nrow(POE_5) - nrow(POE_6), unweighted_base)

    # 13. Weight the data:
    POE_7 = autotable::apply_weight(POE_6, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("build_point_of_entry - Weighted the data by %s", weight_variable)

    # 14. Group cases by their values for drink, and then summarise for both parts:
    POE_7$frontbac  = factor(POE_7$frontbac, levels = c(1, 2, 3, 4))

    POE_8          = dplyr::group_by(POE_7, frontbac, .drop = FALSE)

    POE_9          = dplyr::summarise_at(POE_8, "relevant_incident", sum)

    POE_9$frontbac = c(1, 2, 3, 4)
    futile.logger::flog.info("build_point_of_entry - Grouped cases by their values for drink")

    # 15. Generate the unrounded percentages for this year:
    POE_10 = (POE_9 / base::sum(POE_9$relevant_incident)) * 100
    futile.logger::flog.info("build_point_of_entry - Generated the unrounded percentages")

    POE_11 <- base::as.data.frame(POE_10[, -1])

    # 16. Generate a name for this column, which we will later apply in Step :
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("build_point_of_entry - Finished column \"%s\"", names(years_of_data)[i])

    # 17. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:

      POE_12 = base::as.data.frame(POE_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      POE_12 = base::cbind(POE_12, base::as.data.frame(POE_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

  }

  # 18. And add the row of unweighted bases to the bottom of the table
  TablePOE <- base::rbind(POE_12, row_of_unweighted_bases)

  # 19. Name the rows:

  rownames(TablePOE) = config$TABLE$ROW_LABELS


  # 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
  #     the earliest year, for each row:
  POE_SIG = append_significance_column(dataframe = TablePOE, column_1 = ncol(TablePOE), column_2 = 1,
                                       row_of_unweighted_bases = nrow(TablePOE))
  futile.logger::flog.trace("build_point_of_entry - Appended a significance column, comparing the latest year of data with the previous year")
  POE_SIG2 = append_significance_column(dataframe = POE_SIG, column_1 = ncol(POE_SIG) -1, column_2 = ncol(POE_SIG) - 2,
                                        row_of_unweighted_bases = nrow(POE_SIG))
  futile.logger::flog.trace("build_point_of_entry  - Appended a significance column, comparing the latest year of data with the earliest year")
  
  # 21. Name the columns:
  
  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$TABLE$FOOTNOTES), "$$") 
  
  colnames(POE_SIG2) = c(names(years_of_data),
                         name_of_10_year_test_column,
                         name_of_1_year_test_column)

  return(POE_SIG2)
}
