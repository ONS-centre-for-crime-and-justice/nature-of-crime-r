#' @title items_stolen_any_items
#'
#' @description Builds the "Which items?" part of one subtable of an "Items stolen" table for a Nature of Crime dataset.
#'
#' @details This function builds the "Which items?" part of one subtable (specified by \code{subtable}) based predominantly
#' on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for more details
#' about these two config files.
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
#' items_stolen_which_items(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

items_stolen_which_items <- function(config, master_config, subtable) {

# 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

# 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)

# Below, we will use a for-loop to process the years of data one-by-one.

for (i in c(1:length(years_of_data))) {

  # 3. Read in the VF dataset for this year:
    futile.logger::flog.trace("items_stolen_items_stolen - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

  # 4. Fetch the correct weight variable for this year of data:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.trace("items_stolen_items_stolen - Using %s as the weight variable", weight_variable)

  # 5. Remove zero-weighted cases from the dataset:
    WI_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Items stolen", which describes the subtable
    #  component that we want this function to build. Each step in the code will build on the previous one from now
    #  onwards; WI_1 will become WI_2, and then WI_2 will become WI_3, and so on, until we have the dataframe in
    #  the form that we want)
    futile.logger::flog.trace("items_stolen_items_stolen - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(WI_1), nrow(VF_dataset))

  # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
  #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    WI_2 = dplyr::filter(WI_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.trace("items_stolen_items_stolen - Selected %s cases where victarea = 1 or wherapp = 1", nrow(WI_2))

  # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
  #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    WI_3 = dplyr::mutate(WI_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.trace("items_stolen_items_stolen - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

  # 7.5. Temporary "hacky" code for burglary, until I get time to fix it properly (will prob move offence codes into both parts for at least burglary). We need to select only the cases where items were stolen.
    if (grepl("non-connected", config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]], ignore.case = TRUE)) {
      WI_3 = dplyr::filter(WI_3, offence == 58)
    } else if (grepl("dwelling with entry", config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]], ignore.case = TRUE)) {
      WI_3 = dplyr::filter(WI_3, offence == 52)
    }

  # 8. Select only the cases where relevant_incident = 1:
    WI_4 = dplyr::filter(WI_3, relevant_incident == 1)
    futile.logger::flog.trace("items_stolen_items_stolen - Selected %s cases where relevant_incident = 1", nrow(WI_4))

  # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    WI_5 = dplyr::filter(WI_4, vftype == 1)
    futile.logger::flog.trace("items_stolen_items_stolen - Selected %s cases where vftype = 1", nrow(WI_5))

  # 10. We now want to exclude the missing cases. Because rows of the table are based on values of different variables,
  #     we will have to handle them separately, since their numbers of valid cases differ in general:
  variables_for_rows = unlist(lapply(config$PART_TWO$ROW_LABELS, `[[`, 1))
  WI_5$newvar = rowSums(WI_5[, variables_for_rows], na.rm = TRUE)
  WI_5 = dplyr::filter(WI_5, newvar != 0)

  # 11. Determine the unweighted base:
    unweighted_base = base::sum(WI_5$relevant_incident)
    futile.logger::flog.trace("items_stolen_items_stolen - Determined the unweighted base as %s", unweighted_base)

  # 12. Weight the data:
    variables_for_rows = unlist(lapply(config$PART_TWO$ROW_LABELS, `[[`, 1))
    WI_6 = autotable::apply_weight(WI_5, base::c("relevant_incident", variables_for_rows), weight = weight_variable)

  # 13. Generate the unrounded percentages for this year:
    weighted_totals = dplyr::summarise_at(WI_6, variables_for_rows, sum, na.rm = TRUE)
    WI_7            = (weighted_totals / base::sum(WI_6$relevant_incident)) * 100

  # 14. Generate a name for this column, which we will later apply in Step 21:
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("items_stolen_items_stolen - Finished column \"%s\"", names(years_of_data)[i])

  # 15. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      WI_8 = base::as.data.frame(t(WI_7))
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      WI_8 = base::cbind(WI_8, base::as.data.frame(t(WI_7)))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

}

# 16. Bind the row of unweighted bases to the bottom of the table:
  WI_9 = base::rbind(WI_8, row_of_unweighted_bases)
  futile.logger::flog.trace("items_stolen_items_stolen - Added the row of unweighted bases")

# 17. Name the rows:
  rownames(WI_9) = unlist(lapply(config$PART_TWO$ROW_LABELS, `[[`, 2))
  futile.logger::flog.trace("items_stolen_items_stolen - Added row labels")

# 18. We now wish to order the rows by the magnitude of their percentages in the latest year of data:
  WI_9_ordered = WI_9[base::order(-WI_9[, base::ncol(WI_9)]),]

# However, regardless of magnitudes, we always want the bottom two rows to be the "Other" row(s) and then the unweighted bases.
  index_of_other_row = grep("Other stolen", rownames(WI_9_ordered), ignore.case = TRUE)
  index_of_bases_row = grep("Unweighted", rownames(WI_9_ordered), ignore.case = TRUE)

  WI_9_ordered = rbind(WI_9_ordered[-c(index_of_other_row, index_of_bases_row), ],
                       WI_9_ordered[index_of_other_row, ],
                       WI_9_ordered[index_of_bases_row, ])

  # And if the crime type is not vehicle-related theft, we want to move the jewellery row and the watches row under the "Jewellery/watches" row too:
    if ((!any(grepl("vehicle", config$BOTH_PARTS_OF_THE_TABLE$TITLES_OF_SUBTABLES, ignore.case = TRUE)))) {

     index_of_jewellery_and_watches_row = grep("Jewellery.*Watches", rownames(WI_9_ordered), perl = TRUE, ignore.case = TRUE)
     index_of_jewellery_row             = grep("^(?!.*Watches).*Jewellery", rownames(WI_9_ordered), perl = TRUE, ignore.case = TRUE)
     index_of_watches_row               = grep("^(?!.*Jewellery).*Watches", rownames(WI_9_ordered), perl = TRUE, ignore.case = TRUE)
     jewellery_and_watches_row          = WI_9_ordered[index_of_jewellery_and_watches_row,]
     jewellery_row                      = WI_9_ordered[index_of_jewellery_row,]
     watches_row                        = WI_9_ordered[index_of_watches_row,]

      WI_9_ordered[c(index_of_jewellery_and_watches_row, index_of_jewellery_row, index_of_watches_row),] = NA
      rownames(WI_9_ordered)[c(index_of_jewellery_and_watches_row, index_of_jewellery_row, index_of_watches_row)] = c(1:3)
      WI_9_ordered = rbind(WI_9_ordered[c(1:index_of_jewellery_and_watches_row), ],
                           jewellery_and_watches_row,
                           jewellery_row,
                           watches_row,
                           WI_9_ordered[c(index_of_jewellery_and_watches_row:nrow(WI_9_ordered)), ])

    }

  WI_10 = WI_9_ordered[complete.cases(WI_9_ordered),]

# 19. Add significance columns, comparing (A) the latest year against the previous year; and (B) the latest year against
#     the earliest year, for each row:
  WI_11 = append_significance_column(dataframe = WI_10, column_1 = ncol(WI_10), column_2 = ncol(WI_10) - 1,
                                     row_of_unweighted_bases = nrow(WI_10))
  futile.logger::flog.trace("items_stolen_items_stolen - Appended a significance column, comparing the latest year of data with the previous year")
  WI_12 = append_significance_column(dataframe = WI_11, column_1 = ncol(WI_11) - 1, column_2 = 1,
                                     row_of_unweighted_bases = nrow(WI_11))
  futile.logger::flog.trace("items_stolen_items_stolen - Appended a significance column, comparing the latest year of data with the earliest year")

# 20. Name the columns:

  # For the sig-testing columns, we'll need to attach the correct footnote number:
  name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
  name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                       grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

  colnames(WI_12) = c(names(years_of_data),
                      name_of_10_year_test_column,
                      name_of_1_year_test_column)

  futile.logger::flog.trace("items_stolen_items_stolen - Added column labels")

# 21. Return the final dataframe:
  return(WI_12)

}

