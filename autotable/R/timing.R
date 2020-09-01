#' @title timing
#'
#' @description Builds the "Timing of when incidents occurred" table for a headline crime type, as seen in the "Nature of Crime"
#' dataset for that headline crime type.
#'
#' @details This function builds a "Timing of when incidents occurred" table based on two configuration files. The first config
#' file, \code{config}, corresponds to one of the headline crime types, and chiefly determines the output of the table. The second,
#' \code{master_config}, allows you to tweak things that apply to multiple tables (probably including this one), with the idea being
#' that it's easier to change the master config file once than it is to update each config file for those multiple tables - which
#' could potentially constitute changing 50 different files... bothersome and time-consuming).
#'
#' In this function, each subtable of the table is processed in turn. The processing of the table is handled by lower-order functions,
#' one corresponding to each part of the table. For the "Timing of when incidents occurred" table, there are currently 3 parts:
#' "Time of day", "Time of week", and "Daylight or dark"; these are built by the functions \code{timing_time_of_day},
#' \code{timing_time_of_week}, and \code{timing_daylight_or_dark} respectively, which each return a data frame.
#'
#' The function then binds these 3 parts together into a single data frame to get the full "Timing of when incidents occurred" table
#' before processing it into a \code{GPTable} object (see the User Guide for more details) which is what is returned.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate "Nature
#' of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @return The "Timing of when incidents occured" table for the headline crime type specified by \code{config}, as a \code{GPTable}
#' object.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/timing/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' timing(config = config, master_config = master_config)
#'
#' @seealso See the following URL for an example of a Nature of Crime dataset (where the headline crime type in the example is
#' "Vehicle-related theft"): \url{https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/natureofcrimevehiclerelatedtheft}
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export
#'

timing = function(config, master_config) {

# We would like each of the table's subtables to appear on separate sheets in the final Excel workbook. As we will be doing
# very similar things when building each subtable, we will put the constituent code inside a for-loop to run for each subtable
# in succession; i.e. each iteration of the for-loop will build a different one of the subtables.

# 1. Firstly, however, we need (A) a list of subtables to build; and (B) an empty list into which we can store each completed
#    subtable once it's been built:

subtables_to_build     = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 1)
subtables_built        = vector("list", length(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES))
names(subtables_built) = subtables_to_build


for (i in c(1:length(subtables_to_build))) {

  futile.logger::flog.info("timing - Currently building the subtable '%s'", subtables_to_build[[i]])

  # 2. Call the relevant build_ functions to build both parts of the subtable currently under consideration:

    TotW = autotable::timing_time_of_week(config = config, master_config = master_config, subtable = i)
    ToD  = autotable::timing_time_of_day(config = config, master_config = master_config, subtable = i)
    DoD  = autotable::timing_daylight_or_dark(config = config, master_config = master_config, subtable = i)

  # 3. Shift the rownames so that they are a column of the actual dataframe, and add a second column before that too,
  #    which will represent the second level of indexing; we do this for amenability with gptables' GPTable() function,
  #    which will be used later:

    TotW             = cbind(' ', rownames(TotW), TotW, stringsAsFactors = FALSE)
    ToD              = cbind(' ', rownames(ToD), ToD, stringsAsFactors = FALSE)
    DoD              = cbind(' ', rownames(DoD), DoD, stringsAsFactors = FALSE)
    names(TotW)[1:2] = c("index1", "index2")
    names(ToD)[1:2]  = c("index1", "index2")
    names(DoD)[1:2]  = c("index1", "index2")
    TotW[1, 1]       = config$PART_ONE$NAME
    ToD[1, 1]        = config$PART_TWO$NAME
    DoD[1, 1]        = config$PART_THREE$NAME

  # 4. Bind the three parts of the subtable together, and blank the data frame's rownames so that the gptables indexing will
  #    work properly:

    subtable = rbind(TotW, ToD, DoD)
    rownames(subtable) = c()

  # 5. Check whether any bases are below 50, and - where they are - mark the column heading with a footnote placeholder.

    for (j in c(1:ncol(subtable))) {

      if ((is.numeric(TotW[nrow(TotW), j]) && !is.factor(TotW[nrow(TotW), ]) && TotW[nrow(TotW), j] < master_config$UNWEIGHTED_BASES$CAUTION_THRESHOLD) |
         (is.numeric(ToD[nrow(ToD), j])    && !is.factor(ToD[nrow(ToD), ])   && ToD[nrow(ToD), j]   < master_config$UNWEIGHTED_BASES$CAUTION_THRESHOLD) |
         (is.numeric(DoD[nrow(DoD), j])    && !is.factor(DoD[nrow(DoD), ])   && DoD[nrow(DoD), j]   < master_config$UNWEIGHTED_BASES$CAUTION_THRESHOLD)) {

        names(subtable)[j] = paste0(names(subtable)[j], "$$Note_BASES$$")
      }

    }

  # 6. Store the finished subtable in our list of subtables built:
    subtables_built[[i]] = subtable

    futile.logger::flog.info("timing - Finished building the subtable '%s'", subtables_to_build[[i]])

}


# We now wish to process each of our subtables into GPTable objects, via gptables' GPTable() function:

# 7. As before, let's first set up an empty list, to store the processed subtables in as we go:

  subtables_as_GPTable_objects = vector("list", length(subtables_built))
  futile.logger::flog.info("emotional_impact - Converting the subtables to gptable objects")

for (i in c(1:length(subtables_as_GPTable_objects))) {

  # 8. Set up the necessary parameters for the GPTable() function:
    title         = config$BOTH_PARTS_OF_THE_TABLE$TITLES_OF_SUBTABLES[i]
    subtitles     = config$BOTH_PARTS_OF_THE_TABLE$SUBTITLES
    units         = " "
    scope         = base::paste("Year ending March", config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[1]][[1]] + 1, "to year ending March",
                                config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[length(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES)]][[1]] + 1,
                                "CSEW")
    source        = "Source: Office for National Statistics - Crime Survey for England and Wales"
    index_columns = reticulate::py_dict(c(reticulate::py_eval("1"), reticulate::py_eval("2")), c("index1", "index2"))

    annotations = as.list(config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES)
    annotations = lapply(annotations, function(footnote) if (grepl("master_config", footnote)) {eval(parse(text = footnote))} else {footnote})
    names(annotations) = lapply(c(1:length(annotations)), function(x) paste0("Note_", x))
    if (any(grepl("Note_BASES", names(subtables_built[[i]])))) {
      annotations = c(annotations, "Note_BASES" = master_config$UNWEIGHTED_BASES$FOOTNOTE_FOR_CAUTION)
    }

    additional_formatting = r_to_py(list(
                                  list(column = list(
                                          columns       = list(py_eval("-1"), py_eval("-2")),
                                          format        = list(align = "center"),
                                          include_names = FALSE)),
                                  list(row = list(
                                          rows          = list(py_eval("0")),
                                          format        = list(top = py_eval("1")),
                                          include_names = TRUE)),
                                  list(row = list(
                                          rows          = list(py_eval("-1")),
                                          format        = list(bottom = py_eval("1")),
                                          include_names = TRUE))
                           ))

  # 9. Call the GPTable() function with arguments as above, to convert the subtable to a GPTable object:
    subtable_as_a_GPTable_object = gptables$GPTable(table                 = subtables_built[[i]],
                                                    title                 = title,
                                                    subtitles             = subtitles,
                                                    units                 = units,
                                                    scope                 = scope,
                                                    source                = source,
                                                    index_columns         = index_columns,
                                                    annotations           = annotations,
                                                    additional_formatting = additional_formatting)

  # 10. Add the GPTable object to our list of processed subtables:
    subtables_as_GPTable_objects[[i]] = subtable_as_a_GPTable_object

}

futile.logger::flog.trace("timing - subtables were successfully converted to GPTable objects")

# 11. Return the list of subtables_as_GPTable_objects:
  return(subtables_as_GPTable_objects)

}

