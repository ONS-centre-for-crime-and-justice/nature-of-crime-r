#'@title number_of_incidents_against_men_and_women
#'
#'@description Builds the "Number of Incidents against men and women" Table for a headline crime type, as seen in the "Nature of Crime"
#'dataset for that headline crime type. 
#'
#'@details This function builds an "Number of Incidents against men and women" Table based on a configuration file, where this
#'configuration file corresponds to one of the headline crime types. Each subtable of the Table is processed in turn,
#'where the bulk of this processing is handled by \code{number_of_incidents_build_incidents} and
#'\code{number_of_incidents_build_offence_percentages} and \code{number_of_incidents_build_sex_percentages}, which build the two halves/parts of the subtable respectively. By
#'default, each subtable is converted to a \code{gptable} object and stored in a list as the final step; if, however,
#'\code{return_as_dataframes} is set to \code{TRUE}, a list of R dataframes will be returned instead.
#'
#'@param config A string corresponding to the path of a YAML file, where this YAML serves as the configuration file for
#'the function. This YAML must exhibit a particular structure/format, since the function accesses certain levels and fields by
#'name. For more information on the format required, please see the User Guide (found in the "resources" folder of the
#'project) or inspect a valid YAML file from previous successful runs.
#'
#'@param master-config A string corresponding to a folder path, whereat the VF datasets for each year are stored as .csv
#'files.
#'
#'@param return_as_dataframes A Boolean; \code{TRUE} means that the subtables are returned as a list of dataframes, while
#'\code{FALSE} (the default) means that they are returned as a list of gptable objects.
#'
#'@return Returns a list of the Table's subtables as either gptable objects (by default) or as R dataframes (should
#'\code{return_as_dataframes} be set to \code{TRUE}.
#'
#'@examples number_of_incidents_against_men_and_women(config = "configs/number_of_incidents_against_men_and_women/VIOLENCE.yaml", master config = master config")
#'
#'@export

number_of_incidents_against_men_and_women = function(config, master_config, return_as_dataframes = FALSE) {
   
   
   # 1. Firstly, however, we need (A) a list of subtables to build; and (B) an empty list into which we can store each completed
   #    subtable once it's done:
   
   Subtables_to_build = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 1)
   Subtables_built    = vector("list", length(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES))
   names(Subtables_built) = Subtables_to_build
   
   for (i in c(1:length(Subtables_to_build))) {
      
      futile.logger::flog.info("numbers_of_incidents_against_men_and_women - Currently building the subtable '%s'", Subtables_to_build[[i]])
      
      # 2. Call the relevant build_ functions to build both parts of the subtable currently under consideration, using a filter to determine whether the second
      # part of the table needs to be built or not. The grepl uses the "non-connected" part of the subtable title to determine this. 
      
      #  code for burglary tables 
      
      if (grepl("robbery", config$BOTH_PARTS_OF_THE_TABLE$TITLE_OF_TABLE, ignore.case = FALSE)) {
            BI   = number_of_incidents_build_incidents(config = config, master_config = master_config, subtable = i)
            print("Skipping part two")
            print("Starting part three")
            BIS              = number_of_incidents_build_sex_percentages(config = config, master_config = master_config, subtable = i)
            BI               = cbind(' ', rownames(BI), BI, stringsAsFactors = FALSE)
            # BIS            = cbind(' ', ' ', BIS, stringsAsFactors = FALSE)
            names(BI)[1:2]   = c("index1", "index2")
            Subtable = cbind(BI, BIS)
            rownames(Subtable) = c()
            Subtables_built[[i]] = Subtable      
            
         } else {
            BI   = number_of_incidents_build_incidents(config = config, master_config = master_config, subtable = i)
            print("Starting part two")
            BOT = number_of_incidents_build_offence_percentages(config = config, master_config = master_config, subtable = i)
            print("Starting part three")
            BIS              = number_of_incidents_build_sex_percentages(config = config, master_config = master_config, subtable = i)
            BI               = cbind(' ', rownames(BI), BI, stringsAsFactors = FALSE)
            # BOT              = cbind(' ', ' ', BOT, stringsAsFactors = FALSE)
            # BIS              = cbind(' ', ' ', BIS, stringsAsFactors = FALSE)
            names(BI)[1:2]   = c("index1", "index2")
            Subtable = cbind(BI, BOT, BIS)
            rownames(Subtable) = c()
            # Subtable = rbind(Subtable[1,], Subtable[2:5,])
            Subtables_built[[i]] = Subtable
         }
      
   }
   
   # # 3. If return_as_dataframes was set to TRUE, then we will stop here, returning the list of Subtables_built...
   if (return_as_dataframes) {
      return(Subtables_built)
   }
   
   # ... else we will continue, and proceed to process each of our subtables into gptable objects, via gptables' GPTable() function:
   
   
   # 4. As before, let's first set up an empty list, to store the processed subtables in as we go:
   
   Subtables_as_gptables_objects = vector("list", length(Subtables_built))
   futile.logger::flog.info("numbers_of_incidents_against_men_and_women - Converting the subtables to gptable objects")
   
   for (i in c(1:length(Subtables_as_gptables_objects))) {
      
      # 5. Set up the necessary parameters for the GPTable() function:
      Title         = config$BOTH_PARTS_OF_THE_TABLE$TITLES_OF_SUBTABLES[i]
      Subtitles     = config$BOTH_PARTS_OF_THE_TABLE$SUBTITLES
      Units         = " "
      Scope         = base::paste("Year ending March", config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[length(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES)]][[1]] + 1,
                                  "CSEW")
      Source        = "Source: Office for National Statistics - Crime Survey for England and Wales"
      Index_columns = reticulate::py_dict(c(reticulate::py_eval("1"), reticulate::py_eval("2")), c("index1", "index2"))
      Annotations = as.list(config$BOTH_PARTS_OF_THE_TABLE$FOOTNOTES)
      names(Annotations) = lapply(c(1:length(Annotations)), function(x) paste0("Note_", x))
      
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
      
      futile.logger::flog.info("numbers_of_incidents_against_men_and_women - neccessary parameters set up")
      
      # 6. Call the GPTable() function with arguments as above, to convert the subtable to a gptable object :
      Subtable_as_a_gptable_object = gptables$GPTable(table                 = Subtables_built[[i]],
                                                      title                 = Title,
                                                      subtitles             = Subtitles,
                                                      units                 = Units,
                                                      scope                 = Scope,
                                                      source                = Source,
                                                      index_columns         = Index_columns,
                                                      annotations           = Annotations,
                                                      additional_formatting = additional_formatting)
      
      futile.logger::flog.info("numbers_of_incidents_against_men_and_women - called the gp tables")
      
      # 7. Add the gptable object to our list of processed subtables:
      Subtables_as_gptables_objects[[i]] = Subtable_as_a_gptable_object
      
   }
   
   futile.logger::flog.info("numbers_of_incidents_against_men_and_women - Subtables were successfully converted to gptable objects")
   
   # 8. Return the list of Subtables_as_gptables_objects:
   return(Subtables_as_gptables_objects)
}


