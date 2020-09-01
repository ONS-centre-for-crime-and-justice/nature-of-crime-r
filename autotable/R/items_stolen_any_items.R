#' @title items_stolen_any_items
#'
#' @description Builds the "Were any items stolen?" part of one subtable of an "Items stolen" table for a Nature of
#' Crime dataset. At present, this part is only built for the headline crime type Burglary.
#'
#' @details This function builds the "Were any items stolen" part of one subtable (specified by \code{subtable}) based
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Were any items stolen?" part of
#' the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/items_stolen/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' items_stolen_any_items(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

items_stolen_any_items <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("items_stolen_any_items - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("items_stolen_any_items - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    AI_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Items stolen", which describes the subtable
    #  component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; AI_1 will become AI_2, and then AI_2 will become AI_3, and so on, until we have the dataframe in
    #  the form that we want)
    futile.logger::flog.trace("items_stolen_any_items - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(AI_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    AI_2 = dplyr::filter(AI_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("items_stolen_any_items - Selected %s cases where victarea = 1 or wherapp = 1", nrow(AI_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    AI_3 = dplyr::mutate(AI_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("items_stolen_any_items - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 8. Select only the cases where relevant_incident = 1:
    AI_4 = dplyr::filter(AI_3, relevant_incident == 1)
    futile.logger::flog.trace("items_stolen_any_items - Selected %s cases where relevant_incident = 1", nrow(AI_4))

  # 9. Remove missings[Is this needed?]

  # 10. Determine the unweighted base:
    unweighted_base = base::sum(AI_4$relevant_incident)
    futile.logger::flog.trace("items_stolen_any_items - Determined the unweighted base as %s", unweighted_base)

  # 11. Weight the data:
    AI_5 = autotable::apply_weight(AI_4, "relevant_incident", weight = weight_variable)

  # 12. Generate the percentage for this year [CURRENTLY HARDCODED, NEEDS FIXING]:
    AI_6 = as.data.frame((base::sum(dplyr::filter(AI_5, offence %in% c(52, 58))$relevant_incident) / base::sum(AI_5$relevant_incident)) * 100)

  # 13. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("items_stolen_any_items - Finished column \"%s\"", names(years_of_data)[i])

  # 14. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      AI_7 = base::as.data.frame(t(AI_6))
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      AI_7 = base::cbind(AI_7, base::as.data.frame(t(AI_6)))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 15. Bind the row of unweighted bases to the bottom of the table:
  AI_8 = base::rbind(AI_7, row_of_unweighted_bases)
  futile.logger::flog.trace("items_stolen_any_items - Added the row of unweighted bases")

# 16. Name the rows:
  rownames(AI_8) = config$PART_ONE$ROW_LABELS
  futile.logger::flog.trace("items_stolen_any_items - Added row labels")

# 17. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  AI_9 = append_significance_column(dataframe = AI_8, column_1 = ncol(AI_8), column_2 = ncol(AI_8) - 1,
                                     row_of_unweighted_bases = nrow(AI_8))
  futile.logger::flog.trace("items_stolen_any_items - Appended a significance column, comparing the latest year of data with the previous year")
  AI_10 = append_significance_column(dataframe = AI_9, column_1 = ncol(AI_9) - 1, column_2 = 1,
                                     row_of_unweighted_bases = nrow(AI_9))
  futile.logger::flog.trace("items_stolen_any_items - Appended a significance column, comparing the latest year of data with the earliest year")

# 18. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

  colnames(AI_10) = c(names(years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)

  futile.logger::flog.trace("items_stolen_any_items - Added column labels")

# 20. Return the final dataframe:
  return(AI_10)

}

