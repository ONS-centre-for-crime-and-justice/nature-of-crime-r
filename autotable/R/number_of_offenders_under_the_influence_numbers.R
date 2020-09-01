#' @title number_of_offenders_under_the_influence_numbers
#'
#' @description Builds the "Number of incidents (thousands)" part of one subtable of a "Number of offenders under the
#' influence" table for a Nature of Crime dataset.
#'
#' @details This function builds the "Number of incidents (thousands)" part of one subtable (specified by \code{subtable})
#' based predominantly on two configurations file specified by \code{config} and \code{master_config}. See the User Guide for
#' more details about these two config files.
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
#' @return Returns a dataframe with the requisite percentages and unweighted bases for the "Number of incidents (thousands)"
#' part of the specified subtable.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/number_of_offenders_under_the_influence/ROBBERY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' number_of_offenders_under_the_influence_numbers(config = config, master_config = master_config, subtable = 1)
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

number_of_offenders_under_the_influence_numbers = function(config, master_config, subtable) {

  # 1. Find out which years we are building tables for:
  years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)

  # 2. Get a list of the VF datasets to process:
  list_of_VF_datasets <- base::list.files(path = master_config$VF_DATA_DIRECTORY)


  # Below, we will use a for-loop to process the years of data one-by-one.

  for (i in c(1:length(years_of_data))) {

    # 3. Read in the VF datasets:
    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Reading in source data from %s", list_of_VF_datasets[i])
    VF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$VF_DATA_DIRECTORY, list_of_VF_datasets[i], sep = '/')))

    # 4. Fetch the correct weight variables:
    weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]

    # 5. Remove zero-weighted cases from the dataset:
    NoI_1 = autotable::remove_zeroweighted_rows(df = VF_dataset, weightvar = weight_variable)
    # (Regarding the choice of variable name here: This just stands for "Number of Incidents", which describes the table component that we want
    #  this function to build. Each step in the code will build on the previous one from now; NoI_1 will become NoI_2, and then become NoI_3, and then
    #  NoI_4, and so on, until we have it in the form that we want. Having a different data frame for each step is very useful for us when debugging!)
    # Fetch the offence type variables from the yaml config file for the loop
    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Removed %s cases out of %s because their weights were 0",
                             nrow(VF_dataset) - nrow(NoI_1), nrow(VF_dataset))

    # 6. Keep only cases where victarea = 1 (i.e. the incident occurred within 15 minutes of the interview location - typically
    #    the respondent's home) or where wherhapp = 1 (i.e. the incident occurred in England & Wales):
    NoI_2 = dplyr::filter(NoI_1, victarea == 1 | wherhapp == 1)
    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Selected %s cases where victarea = 1 or wherapp = 1",
                             nrow(NoI_2))

    # 7. Compute a variable which indicates whether each case is a relevant incident or not, based off its associated offence
    #    code; a value of 1 will indicate that the incident is indeed relevant to the subtable we are building:
    offence_codes = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[2]]
    NoI_3 = dplyr::mutate(NoI_2, relevant_incident = dplyr::if_else(offence %in% offence_codes, 1, NULL))
    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Computed a new variable called relevant_incident; a value of 1 for this variable will indicate that the case in question has an offence code relevant to the subtable being built")

    # 8. Select only the cases where relevant_incident = 1:
    NoI_4 = dplyr::filter(NoI_3, relevant_incident == 1)
    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Selected %s cases where relevant_incident = 1",
                             nrow(NoI_4))

    # 9. Select only the cases where vftype = 1 (i.e. the incident was recorded on the long Victim Form):
    NoI_5 = dplyr::filter(NoI_4, vftype == 1)
    futile.logger::flog.trace("build_time_of_day - Selected %s cases where vftype = 1", nrow(NoI_5))

    # 10. If the headline crime type is violence, we may have to apply some extra filters, depending on which subtable is being
    #     built. We'll find out if the headline crime type is violence by searching for "violence" in the Table's title:
    if (grepl("violen", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
      number_of_cases_pre_filter = nrow(NoI_5)
      NoI_5 = autotable::apply_additional_violence_filters(dataframe = NoI_5,
                                                           subtable_name = config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES[[subtable]][[1]])
      futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Applied additional violence filters, removing a further %s cases",
                                number_of_cases_pre_filter - nrow(NoI_5))
    }

    # 11. Filter out cases which have missing values for the 'drink' variable, and then the same for the 'drug' variable:
    NoI_6_drink = dplyr::filter(NoI_5, !is.na(drink))
    NoI_6_drug  = dplyr::filter(NoI_5, !is.na(drug))

    # 12. Historically, the unweighted base that is listed for this part of the table is that which underlies the 'drink' row:
    unweighted_base = base::sum(NoI_6_drink$relevant_incident)

    # 13. Weight the data:
    NoI_7_drink = autotable::apply_weight(NoI_6_drink, "relevant_incident", weight = weight_variable)
    NoI_7_drug  = autotable::apply_weight(NoI_6_drug, "relevant_incident", weight = weight_variable)
    futile.logger::flog.trace("number_of_offenders_under_the_influence_proportions - Weighted the data by %s", weight_variable)

    # 14. Group cases by their values, and then summarise for both parts:
    NoI_7_drink$drink = factor(NoI_7_drink$drink, levels = c(1, 2, 9))
    NoI_7_drug$drug   = factor(NoI_7_drug$drug, levels = c(1, 2, 9))

    NoI_8_drink = dplyr::group_by(NoI_7_drink, drink, .drop = FALSE)
    NoI_8_drug  = dplyr::group_by(NoI_7_drug, drug, .drop = FALSE)

    NoI_9_drink = dplyr::summarise_at(NoI_8_drink, "relevant_incident", sum)
    NoI_9_drug  = dplyr::summarise_at(NoI_8_drug, "relevant_incident", sum)

    NoI_9_drink$drink = c(1, 2, 9)
    NoI_9_drug$drug   = c(1, 2, 9)

    futile.logger::flog.info("number_of_offenders_under_the_influence_proportions - Grouped cases by their values for 'drink' and 'drug'")

    # 15. Generate the proportions for this year:
    NoI_10_drink = (NoI_9_drink / base::sum(NoI_9_drink$relevant_incident)) * 100
    NoI_10_drug  = (NoI_9_drug  / base::sum(NoI_9_drug$relevant_incident))  * 100
    futile.logger::flog.info("number_of_offenders_under_the_influence_numbers - Generated the unrounded percentages")

    proportion_of_incidents_where_the_offender_was_under_the_influence_of_drink = NoI_10_drink[1, "relevant_incident"]
    proportion_of_incidents_where_the_offender_was_under_the_influence_of_drugs = NoI_10_drug[1, "relevant_incident"]

    # 16. For the number of incidents where the offender was under the influence, we need to multiply the proportions derived above
    #     by the total number of robbery incidents. And to get the total number of robbery incidents, we need to use the NVF:
    if (grepl("robbery", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = TRUE)) {
        list_of_NVF_datasets = base::list.files(path = master_config$NVF_DATA_DIRECTORY)
        NVF_dataset          = base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, list_of_NVF_datasets[i], sep = '/')))
        weighted_NVF_dataset = autotable::apply_weight(NVF_dataset, "robber_i", weight = "C11IndivWgt")

        total_incidents_of_robbery = sum(weighted_NVF_dataset$robber_i, na.rm = TRUE) / 1000

        number_of_incidents_where_the_offender_was_under_the_influence_of_drink = total_incidents_of_robbery *
                                                                                  proportion_of_incidents_where_the_offender_was_under_the_influence_of_drink / 100
        number_of_incidents_where_the_offender_was_under_the_influence_of_drugs = total_incidents_of_robbery *
                                                                                  proportion_of_incidents_where_the_offender_was_under_the_influence_of_drugs / 100

        NoI_11 = as.data.frame(c(round(number_of_incidents_where_the_offender_was_under_the_influence_of_drink / 1000),
                                 round(number_of_incidents_where_the_offender_was_under_the_influence_of_drugs / 1000)))
    } else {
        list_of_NVF_datasets = base::list.files(path = master_config$NVF_DATA_DIRECTORY)
        NVF_dataset          = base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, list_of_NVF_datasets[i], sep = '/')))
        weighted_NVF_dataset = autotable::apply_weight(NVF_dataset, "violnr_i", weight = "C11IndivWgt")

        total_incidents_of_violence = sum(weighted_NVF_dataset$violnr_i, na.rm = TRUE) / 1000

        number_of_incidents_where_the_offender_was_under_the_influence_of_drink = total_incidents_of_violence *
                                                                                  proportion_of_incidents_where_the_offender_was_under_the_influence_of_drink / 100
        number_of_incidents_where_the_offender_was_under_the_influence_of_drugs = total_incidents_of_violence *
                                                                                  proportion_of_incidents_where_the_offender_was_under_the_influence_of_drugs / 100

        NoI_11 = as.data.frame(c(round(number_of_incidents_where_the_offender_was_under_the_influence_of_drink / 1000),
                                 round(number_of_incidents_where_the_offender_was_under_the_influence_of_drugs / 1000)))
    }

    # 17. Generate a name for this column, which we will later apply in Step :
    start_year              = substr(years_of_data[[i]], 3, 4)
    end_year                = as.numeric(start_year) + 1
    names(years_of_data)[i] = paste0("Apr '", start_year, " to Mar '", end_year)
    futile.logger::flog.trace("number_of_offenders_being_under_the_influence_numbers - Finished column \"%s\"", names(years_of_data)[i])

    # 18. We now want to add this column into our final dataframe:

    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
                  # that case, we must create one:

      NoI_12 = base::as.data.frame(NoI_11)
      row_of_unweighted_bases = unweighted_base

    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:

      NoI_12 = base::cbind(NoI_12, base::as.data.frame(NoI_11))
      row_of_unweighted_bases = base::c(row_of_unweighted_bases, unweighted_base)

    }

  }

# 19. And add the row of unweighted bases to the bottom of the table:
NoI_13 = base::rbind(NoI_12, row_of_unweighted_bases)

# 20. Name the rows:
rownames(NoI_13) = config$PART_TWO$ROW_LABELS
futile.logger::flog.info("number_of_offenders_being_under_the_influence_numbers - Added row labels")

# 21. We do not need to do sig-testing for this part, but we need to make it the same size as the other part (which does
#     have sig-testing) in order to bind them in the nature function. We will use ':' to indicate that sig-testing does not
#     apply to these cells:
NoI_14 = cbind(NoI_13, c(rep(":", nrow(NoI_13) - 1), ' '), c(rep(":", nrow(NoI_13) - 1), ' '))

# 22. Name the columns:

# For the sig-testing columns, we'll need to attach the correct footnote number:
name_of_10_year_test_column = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, "$$Note_",
                                     grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")
name_of_1_year_test_column  = paste0(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, "$$Note_",
                                     grep("master_config\\$SIGNIFICANCE_TESTING\\$FOOTNOTE", config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES), "$$")

colnames(NoI_14) = c(names(years_of_data),
                     name_of_10_year_test_column,
                     name_of_1_year_test_column)

futile.logger::flog.trace("number_of_offenders_being_under_the_influence_numbers - Added column labels")

return(NoI_14)

}
