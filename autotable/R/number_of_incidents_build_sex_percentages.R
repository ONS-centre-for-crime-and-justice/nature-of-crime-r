#'@title number_of_incidents_build_sex_percentages
#'
#'@description Builds the "Proportions by Sex" (BOT) part/half of one subtable of an "Number of Incidents against men and women"
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
#'@examples number_of_incidents_build_sex_percentages(config = "configs/number_of_incidents_against_men_and_women/BURGLARY.yaml", master config = master config", subtable = 1)
#'
#'@export

number_of_incidents_build_sex_percentages <- function(config, master_config, subtable) {

  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  List_of_NVF_datasets <- base::list.files(path = master_config$NVF_DATA_DIRECTORY)

  # Below, we will use a for-loop to process the years of data one-by-one.

  # 3. Read in the VF dataset for this year:
  futile.logger::flog.info("build_sex_percentages - Reading in source data from %s",
  List_of_NVF_datasets[length(List_of_NVF_datasets)])
  NVF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, List_of_NVF_datasets[length(List_of_NVF_datasets)], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
  Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[length(List_of_NVF_datasets)]][[2]]
  futile.logger::flog.info("build_sex_percentages - Using %s as the weight variable", Weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
  BIS_1 = autotable::remove_zeroweighted_rows(df = NVF_dataset, weightvar = Weight_variable)
  futile.logger::flog.info("build_sex_percentages - Removed %s cases out of %s because their weights were 0",
                           nrow(NVF_dataset) - nrow(BIS_1), nrow(NVF_dataset))

  # 6. Extract the unweighted base:

  Unweighted_base = nrow(BIS_1)
  futile.logger::flog.info("build_sex_percentages - Recoded variables and determined the unweighted base as %s",
                           Unweighted_base)

  # 7. Weight the data

  Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
  BIS_2 = autotable::apply_weight(BIS_1, base::c(Variables_for_rows), weight = Weight_variable)
  futile.logger::flog.info("build_sex_percentages - Weighted the data using %s", Weight_variable)

  # 8. Loop through the variables for the offence type and calculate the male, female and total number of incidents, then bind them together

  for (i in c(1:length(Variables_for_rows))) {

    offence = Variables_for_rows[[i]]
    futile.logger::flog.info("build_sex_percentages - Starting the calculations for %s", offence)

    # Calculate Male Means and Totals
    BIS_3 = dplyr::filter(BIS_2, sex == 1)
    male_unweighted_base = nrow(BIS_3)
    male_incidents   = as.data.frame(sum(BIS_3[, offence]) / 1000000)
    futile.logger::flog.info("build_sex_percentages - Calculated the number of male incidents")

    # Calculate Female Means and Totals
    BIS_4 = dplyr::filter(BIS_2, sex == 2)
    female_unweighted_base = nrow(BIS_4)
    female_incidents   = as.data.frame(sum(BIS_4[, offence]) / 1000000)
    futile.logger::flog.info("build_sex_percentages - Calculated the number of female incidents")

    # Calculate Total Means and Totals
    all_incidents   = as.data.frame(sum(BIS_2[, offence]) / 1000000)
    futile.logger::flog.info("build_sex_percentages - Calculated the number of total incidents")

    # Bind together
    BIS_5 = as.data.frame(cbind(male_incidents, female_incidents, all_incidents))
    futile.logger::flog.info("build_sex_percentages - Binded the columns together")

    # Name the rows and columns
    column_names = c(" Men", " Women", " All")
    rownames(BIS_5) <- offence
    colnames(BIS_5) <- column_names
    futile.logger::flog.info("build_sex_percentages - Named the columns and rows for the variable")

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:

      BIS_6 = BIS_5
      futile.logger::flog.info("build_sex_percentages - Created the BIS_6 dataframe")

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      BIS_6 = base::rbind(BIS_6, BIS_5)
      futile.logger::flog.info("build_sex_percentages - Added BOT_5 to the BOT_6 dataframe")

    }

  }

  # 9.  Calculate percentages by sex and add to main dataframe

  if (grepl("violence", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = FALSE)) {

  Index_row = BIS_6[1, ]
  BIS_6a = as.data.frame(Index_row / Index_row * 100)
  Other_rows = BIS_6[2:4,]
  BIS_6b = as.data.frame(Other_rows / Other_rows[,3] * 100)
  BIS_7 = rbind(BIS_6a, BIS_6b)
  futile.logger::flog.info("build_sex_percentages - Calculated the percentages for sex")

  } else {

  BIS_7 = as.data.frame(BIS_6 / BIS_6[,3] * 100)
  futile.logger::flog.info("build_sex_percentages - Calculated the percentages for sex")
  }


  # 10. Bind the unweighted bases together and add to the dataframe
  # Rows_of_unweighted_bases = data.frame(matrix(ncol=3,nrow=1))
  # Rows_of_unweighted_bases[is.na(Rows_of_unweighted_bases)] <- " "
  Rows_of_unweighted_bases = cbind(male_unweighted_base, female_unweighted_base, Unweighted_base)
  colnames(Rows_of_unweighted_bases) <- column_names
  BIS_8 = rbind(BIS_7, Rows_of_unweighted_bases)

  # 10. Name the rows from the config labels

  rownames(BIS_8) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_sex_percentages - Renamed the rows for the complete data frame")


  # 11. Return the final dataframe:
  return(BIS_8)
  futile.logger::flog.info("build_sex_percentages - Percentages by Sex built")

}
