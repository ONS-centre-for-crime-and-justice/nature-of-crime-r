#'@title method_of_entry
#'
#'@description Builds the "Method of entry of incidents" Table for a headline crime type, as seen in the "Nature of Crime"
#'dataset for that headline crime type. See the following URL for an example of a Nature of Crime dataset, for the headline
#'crime type "Burglary":
#'\url{https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/datasets/natureofburglary}.
#'
#'@details This function builds an "Method of entry in incidents" Table based on a configuration file, where this
#'configuration file corresponds to one of the headline crime types. Each subtable of the Table is processed in turn,
#'where the bulk of this processing is handled by \code{build_method_of_entry} which builds the subtable respectively. By
#'default, each subtable is converted to a \code{gptable} object and stored in a list as the final step; if, however,
#'\code{return_as_dataframes} is set to \code{TRUE}, a list of R dataframes will be returned instead.
#'
#'@param config A string corresponding to the path of a YAML file, where this YAML serves as the configuration file for
#'the function. This YAML must exhibit a particular structure/format, since the function accesses certain levels and fields by
#'name. For more information on the format required, please see the User Guide (found in the "resources" folder of the
#'project) or inspect a valid YAML file from previous successful runs.
#'
#'@param master_config A string corresponding to a folder path, whereat the VF datasets for each year are stored as .csv
#'files.
#'
#'@param return_as_dataframes A Boolean; \code{TRUE} means that the subtables are returned as a list of dataframes, while
#'\code{FALSE} (the default) means that they are returned as a list of gptable objects.
#'
#'@return Returns a list of the Table's subtables as either gptable objects (by default) or as R dataframes (should
#'\code{return_as_dataframes} be set to \code{TRUE}.
#'
#'@examples method_of_entry(config = "configs/method_of_entry/BURGLARY.yaml", master_config = master_config)
#'
#'@export
#'

method_of_entry = function(config, master_config, return_as_dataframes = FALSE) {
   
   # We would like each of the Table's subtables to appear on separate sheets in the final Excel workbook. As we will be doing
   # very similar things when building each subtable, we will put the constituent code inside a for-loop to run for each subtable
   # in succession; i.e. each iteration of the for-loop will build a different one of the subtables.
   
   # 1. Firstly, however, we need (A) a list of subtables to build; and (B) an empty list into which we can store each completed
   #    subtable once it's done:
   
   subtables_to_build = lapply(config$TABLE$OFFENCE_CODES, `[[`, 1)
   subtables_built    = vector("list", length(config$TABLE$OFFENCE_CODES))
   names(subtables_built) = subtables_to_build
   
   
   for (i in c(1:length(subtables_to_build))) {
      
      futile.logger::flog.info("method_of_entry - Currently building the subtable '%s'", subtables_to_build[[i]])
      
      # 2. Call the relevant build_ functions to build the subtable currently under consideration:
      
      MOE   = autotable::build_method_of_entry(config = config, master_config = master_config, subtable = i)
      
      # 3. Shift the rownames so that they are a column of the actual dataframe, and add a second column before that too,
      #    which will represent the second level of indexing; we do this for amenability with gptables' GPTable() function,
      #    which will be used later:
      
      MOE               = cbind(' ', rownames(MOE), MOE, stringsAsFactors = FALSE)
      names(MOE)[1:2]   = c("index1", "index2")
      
      # 4. Blank the dataframe's rownames so that the gptables indexing will
      #    work properly:
      subtable = MOE
      rownames(subtable) = c()
      
      # 5. Store the finished subtable in our list of subtables built:
      subtables_built[[i]] = subtable
      
      futile.logger::flog.info("method_of_entry - Finished building the subtable '%s'", subtables_to_build[[i]])
      
   }
   
   # 6. If return_as_dataframes was set to TRUE, then we will stop here, returning the list of subtables_built...
   if (return_as_dataframes) {
      return(subtables_built)
   }
   # ... else we will continue, and proceed to process each of our subtables into GPTable objects, via gptables' GPTable() function:
   
   
   # 7. As before, let's first set up an empty list, to store the processed subtables in as we go:
   
   subtables_as_GPTable_objects = vector("list", length(subtables_built))
   futile.logger::flog.trace("method_of_entry - Converting the subtables to GPTable objects")
   
   for (i in c(1:length(subtables_as_GPTable_objects))) {
      
      # 8. Set up the necessary parameters for the GPTable() function:
      title         = config$TABLE$TITLES_OF_SUBTABLES[i]
      subtitles     = config$TABLE$SUBTITLES
      units         = " "
      scope         = base::paste("Year ending March", config$TABLE$WEIGHT_VARIABLES[[1]][[1]] + 1, "to year ending March",
                                  config$TABLE$WEIGHT_VARIABLES[[length(config$TABLE$WEIGHT_VARIABLES)]][[1]] + 1,
                                  "CSEW")
      source        = "Source: Office for National Statistics - Crime Survey for England and Wales"
      Index_columns = reticulate::py_dict(c(reticulate::py_eval("1"), reticulate::py_eval("2")), c("index1", "index2"))
      
      annotations = config$TABLE$FOOTNOTES
      annotations = lapply(annotations, function(footnote) if (grepl("master_config", footnote)) {eval(parse(text = footnote))} else {footnote})
      names(annotations) = lapply(c(1:length(annotations)), function(x) paste0("Note_", x))
      
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
      subtable_as_a_GPTable_object = gptables$GPTable(table         = subtables_built[[i]],
                                                      title         = title,
                                                      subtitles     = subtitles,
                                                      units         = units,
                                                      scope         = scope,
                                                      source        = source,
                                                      index_columns = Index_columns,
                                                      annotations   = annotations,
                                                      additional_formatting = additional_formatting)
      
      # 10. Add the GPTable object to our list of processed subtables:
      subtables_as_GPTable_objects[[i]] = subtable_as_a_GPTable_object
      
   }
   
   futile.logger::flog.trace("method_of_entry - subtables were successfully converted to GPTable objects")
   
   # 11. Return the list of subtables_as_GPTable_objects:
   return(subtables_as_GPTable_objects)
   
}


