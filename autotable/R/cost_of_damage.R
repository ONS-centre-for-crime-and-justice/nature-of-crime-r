#' @title cost_of_damage
#'
#' @description Builds the "Cost of damage" table for a headline crime type, as seen in the "Nature of Crime" dataset for that
#' headline crime type.
#'
#' @details This function builds a "Cost of damage" table based on two configuration files. The first config file, \code{config}
#' corresponds to one of the headline crime types, and chiefly determines the output of the table. The second, \code{master_config},
#' allows you to tweak things that apply to multiple tables (probably including this one), with the idea being that it's easier to
#' change the master config file once than it is to update each config file for those multiple tables - which could potentially
#' constitute changing 50 different files... bothersome and time-consuming).
#'
#' In this function, each subtable of the table is processed in turn. The processing of the table is handled by lower-order functions,
#' one corresponding to each part of the table. For the "Cost of damage" table, there is currently only 1 part, which is built by the
#' function \code{cost_of_damage_cost}, which returns a data frame.
#'
#' The function then processes this data frame into a \code{GPTable} object (see the User Guide for more details) which is what is
#' returned.
#'
#' @param config A list of lists (and other items) which is the result of applying \code{yaml::read.yaml()} to an appropriate "Nature
#' of Crime" .yaml file, and then \code{autotable::process_config()}.
#'
#' @param master_config A list of lists (and other objects) which is the result of applying \code{yaml::read.yaml()} to the master
#' configuration file.
#'
#' @return The "Cost of damage" table for the headline crime type specified by \code{config}, as a \code{GPTable}
#' object.
#'
#' @examples config = autotable::process_config(yaml::read.yaml("configs/cost_of_damage/BURGLARY.yaml"))
#' master_config = yaml::read_yaml(file = "configs/MASTER.yaml")
#' cost_of_damage(config = config, master_config = master_config)
#'
#' @seealso See the following URL for an example of a Nature of Crime dataset (where the headline crime type in the example is
#' "Vehicle-related theft"): \url{https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/natureofcrimevehiclerelatedtheft}
#'
#' @author Eleanor Scott-Allen, Katie Edser, Matthew Morrison, Nicholas O'Neil; ONS Centre for Crime and Justice, Titchfield, PO15 5RR.
#'
#' @export

cost_of_damage = function(config, master_config, return_as_dataframes = FALSE) {

# 1. Firstly, however, we need (A) a list of subtables to build; and (B) an empty list into which we can store each completed
#    subtable once it's done:

  subtables_to_build = lapply(config$OFFENCE_CODES, `[[`, 1)
  subtables_built    = vector("list", length(config$OFFENCE_CODES))
  names(subtables_built) = subtables_to_build

for (i in c(1:length(subtables_to_build))) {

  futile.logger::flog.info("cost_of_damage - Currently building the subtable '%s'", subtables_to_build[[i]])

  # 2. Call the relevant build_ function to build the subtable currently under consideration:

    CoD = autotable::cost_of_damage_cost(config = config, master_config = master_config, subtable = i)

  # 3. Shift the rownames so that they are a column of the actual dataframe, and add a second column before that too,
  #    which will represent the second level of indexing; we do this for amenability with gptables' GPTable() function,
  #    which will be used later:

    CoD           = cbind(' ', rownames(CoD), CoD, stringsAsFactors = FALSE)
    names(CoD)[1:2] = c("index1", "index2")
    rownames(CoD) = c()

  # 4. Check whether any bases are below 50, and - where they are - mark the column heading with a footnote placeholder.
    
    for (j in c(1:ncol(CoD))) {

      if (is.numeric(CoD[nrow(CoD), j]) && !is.factor(CoD[nrow(CoD), ]) && CoD[nrow(CoD), j] < master_config$UNWEIGHTED_BASES$CAUTION_THRESHOLD) {

        names(CoD)[j] = paste0(names(CoD)[j], "$$Note_BASES$$")

      }

    }

# 5. Store the finished subtable in our list of subtables built:
    subtables_built[[i]] = CoD
    
    futile.logger::flog.info("cost_of_damage - Finished building the subtable '%s'", subtables_to_build[[i]])
    
}

# 6. If return_as_dataframes was set to TRUE, then we will stop here, returning the list of subtables_built...
  if (return_as_dataframes) {
    return(subtables_built)
  }
# ... else we will continue, and proceed to process each of our subtables into GPTable objects, via gptables' GPTable() function:


# 7. As before, let's first set up an empty list, to store the processed subtables in as we go:

  subtables_as_GPTable_objects = vector("list", length(subtables_built))
  futile.logger::flog.trace("cost_of_damage - Converting the subtables to GPTable objects")

for (i in c(1:length(subtables_as_GPTable_objects))) {

  # 8. Set up the necessary parameters for the GPTable() function:
    title         = config$TITLES_OF_SUBTABLES[i]
    subtitles     = config$SUBTITLES
    units         = " "
    scope         = base::paste("Year ending March", config$WEIGHT_VARIABLES[[1]][[1]] + 1, "to year ending March",
                                config$WEIGHT_VARIABLES[[length(config$WEIGHT_VARIABLES)]][[1]] + 1,
                                "CSEW")
    source        = "Source: Office for National Statistics - Crime Survey for England and Wales"
    index_columns = reticulate::py_dict(c(reticulate::py_eval("1"), reticulate::py_eval("2")), c("index1", "index2"))

    annotations = as.list(config$FOOTNOTES)
    annotations = lapply(annotations, function(footnote) if (grepl("master_config", footnote)) {eval(parse(text = footnote))} else {footnote})
    names(annotations) = lapply(c(1:length(annotations)), function(x) paste0("Note_", x))
    if (any(grepl("Note_BASES", names(subtables_built[[i]])))) {
      annotations = c(annotations, "Note_BASES" = master_config$UNWEIGHTED_BASES$FOOTNOTE_FOR_CAUTION)
    }

    notes = as.list(": denotes not applicable.")

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
                                                    notes                 = notes,
                                                    additional_formatting = additional_formatting)

  # 10. Add the GPTable object to our list of processed subtables:
    subtables_as_GPTable_objects[[i]] = subtable_as_a_GPTable_object

}

futile.logger::flog.trace("cost_of_damage - subtables were successfully converted to GPTable objects")

# 11. Return the list of subtables_as_GPTable_objects:
  return(subtables_as_GPTable_objects)

}

