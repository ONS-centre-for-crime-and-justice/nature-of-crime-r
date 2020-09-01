#' @title emotional_impact_extent_of_impact
#'
#' @description Builds the "Extent of impact" part of one subtable of an "Emotional impact of incidents" table for a
#' Nature of Crime dataset.
#'
#' @details This function builds the "Extent of impact" part of one subtable (specified by \code{subtable}) based
#' predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide
#' for more details about these two config files.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate
#' "Nature of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @param subtable An integer indicating which subtable to consider; if we denote the value of \code{subtable} by \code{n},
#' then the subtable considered will be that corresponding to the \code{n}th item in \code{config$ALL_PARTS$OFFENCE_CODES}.
#'
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Extent of impact" part of the
#' specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/emotional_impact/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' emotional_impact_extent_of_impact(config = config, master_config = master_config, subtable = 2)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

emotional_impact_extent_of_impact = function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$ALL_PARTS$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets = base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$ALL_PARTS$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    EoI_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Percentage Emotionally Affected", which describes the
    #  subtable component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; EoI_1 will become EoI_2, and then EoI_2 will become EoI_3, and so on, until we have the dataframe in the form
    #  that we want)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Removed %s cases out of %s because their weights were 0",
                              nrow(VF_dataset) - nrow(EoI_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    EoI_2 = dplyr::filter(EoI_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Selected %s cases where victarea = 1 or wherapp = 1",
                              nrow(EoI_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    Offence_codes = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[2]]
    EoI_3 = dplyr::mutate(EoI_2, relevant_incident = dplyr::if_else(offence %in% Offence_codes, 1, NULL))
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    EoI_4 = dplyr::filter(EoI_3, relevant_incident == 1)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Selected %s cases where relevant_incident = 1",
                              nrow(EoI_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    EoI_5 = dplyr::filter(EoI_4, vftype == 1)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Selected %s cases where vftype = 1", nrow(EoI_5))

  # 10. Filter out those who responded "Don't know" to "What emotional reactions did you have?" (i.e. whemotk = 1) and those
  #     who refused to answer (whemotl = 1):
    EoI_6 = dplyr::filter(EoI_5, whemotk != 1 | base::is.na(whemotk))
    EoI_6 = dplyr::filter(EoI_6, whemotl != 1 | base::is.na(whemotl))
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Selected %s cases where either whemotk =/= 1 or whemotl =/= 1 ",
                              nrow(EoI_6))

  # 11. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
  #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violence", config$ALL_PARTS$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(EoI_6)
      EoI_6 = autotable::apply_additional_violence_filters(dataframe = EoI_6,
                                                           subtable_name = config$ALL_PARTS$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("emotional_impact_extent_of_impact - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(EoI_6))
    }

  # 12. Exclude the missing cases:
    EoI_7 = dplyr::filter(EoI_6, !is.na(totaff))
    futile.logger::flog.trace("Removed %s cases where totaff was NA", nrow(EoI_6) - nrow(EoI_7))

  # 13. Extract the unweighted base:
    unweighted_base = base::sum(EoI_7$relevant_incident)
    futile.logger::flog.trace("Determined the unweighted base as %s", unweighted_base)

  # 14. Weight the data:
    EoI_8 = autotable::apply_weight(EoI_7, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Weighted the data by %s", weight_variable)

  # 15. Group cases by their values for totaff, and then summarise:
    EoI_8$totaff  = factor(EoI_8$totaff, levels = c(1:4))
    EoI_9         = dplyr::group_by(EoI_8, totaff, .drop = FALSE)
    EoI_10        = dplyr::summarise_at(EoI_9, "relevant_incident", sum)
    EoI_10$totaff = c(1:4)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Grouped cases by their values totaff")

  # 16. Generate the unrounded percentages for this year:
    EoI_11 = (EoI_10$relevant_incident / base::sum(EoI_10$relevant_incident))[2:4] * 100
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Generated the unrounded percentages")

  # 17. Generate a name for this column, which we will later apply in Step 23:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("emotional_impact_extent_of_impact - Finished column \"%s\"", names(years_of_data)[i])

  # 18. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      EoI_12 = base::as.data.frame(EoI_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      EoI_12 = base::cbind(EoI_12, base::as.data.frame(EoI_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}


# 19. Bind the row of unweighted bases to the bottom of the table:
  EoI_13 = base::rbind(EoI_12, row_of_unweighted_bases)
  futile.logger::flog.trace("emotional_impact_extent_of_impact - Added the row of unweighted bases")

# 20. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  EoI_14 = append_significance_column(dataframe = EoI_13, column_1 = ncol(EoI_13), column_2 = 1,
                                      row_of_unweighted_bases = nrow(EoI_13))
  futile.logger::flog.trace("emotional_impact_extent_of_impact - Appended a significance column, comparing the latest year of data with the previous year")
  EoI_15 = append_significance_column(dataframe = EoI_14, column_1 = ncol(EoI_14) - 1, ncol(EoI_14) - 2,
                                      row_of_unweighted_bases = nrow(EoI_14))
  futile.logger::flog.trace("emotional_impact_extent_of_impact - Appended a significance column, comparing the latest year of data with the earliest year")

# 21. Name the rows:
  rownames(EoI_15) = config$PART_TWO$ROW_LABELS
  futile.logger::flog.trace("emotional_impact_extent_of_impact - Added row labels")

# 22. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$ALL_PARTS$FOOTNOTES), "$$")

  colnames(EoI_15) = c(names(years_of_data),
                       name_of_10_year_test_column,
                       name_of_1_year_test_column)

# 23. Return the final data frame:
  return(EoI_15)

}

