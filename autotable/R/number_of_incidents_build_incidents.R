#'@title number_of_incidents_build_incidents
#'
#'@description Builds the "Number of Incidents" (BI) part/half of one subtable of an "Number of Incidents against men and women"
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
#'@examples number_of_incidents_build_incidents(config = "configs/number_of_incidents_against_men_and_women/BURGLARY.yaml", master config = master config", subtable = 1)
#'
#'@export

number_of_incidents_build_incidents <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_NVF_datasets <- base::list.files(path = master_config$NVF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  # 3. Read in the VF dataset for this year:
  futile.logger::flog.info("build_incidents - Reading in source data from %s",
  List_of_NVF_datasets[length(List_of_NVF_datasets)])
  NVF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, List_of_NVF_datasets[length(List_of_NVF_datasets)], sep = '/')))
  
  # 4. Fetch the correct weight variable for this year of data:
  Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[length(List_of_NVF_datasets)]][[2]]
  futile.logger::flog.info("build_incidents - Using %s as the weight variable", Weight_variable)
  
  # 5. Remove zero-weighted cases from the dataset:
  BI_1 = autotable::remove_zeroweighted_rows(df = NVF_dataset, weightvar = Weight_variable)
  futile.logger::flog.info("build_incidents - Removed %s cases out of %s because their weights were 0",
                           nrow(NVF_dataset) - nrow(BI_1), nrow(NVF_dataset))
  
  # 6. Extract the unweighted base: 
  
  Unweighted_base = nrow(BI_1)
  futile.logger::flog.info("build_incidents - determined the unweighted base as %s",
                           Unweighted_base)
  
  # ABOVE HERE IS FINE 
  
  # 7. Weight the data 
  
  Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
  BI_2 = autotable::apply_weight(BI_1, base::c(Variables_for_rows), weight = Weight_variable)
  futile.logger::flog.info("build_incidents - Weighted the data for the relevant variables")
  
  # 8. Loop through the variables for the offence type and calculate the male, female and total number of incidents, then bind them together 
  
  for (i in c(1:length(Variables_for_rows))) {
    
    offence = Variables_for_rows[[i]]
    futile.logger::flog.info("build_incidents - Starting the calculations for %s", offence)
    
    # Calculate Male Means and Totals
    BI_3 = dplyr::filter(BI_2, sex == 1)
    male_unweighted_base = nrow(BI_3)
    male_incidents   = as.data.frame(sum(BI_3[, offence]) / 1000000)
    futile.logger::flog.info("build_incidents - Calculated the number of male incidents")
    
    # Calculate Female Means and Totals
    BI_4 = dplyr::filter(BI_2, sex == 2)
    female_unweighted_base = nrow(BI_4)
    female_incidents   = as.data.frame(sum(BI_4[, offence]) / 1000000)
    futile.logger::flog.info("build_incidents - Calculated the number of female incidents")
    
    # Calculate Total Means and Totals
    # MEAN CALCULATION HERE
    all_incidents   = as.data.frame(sum(BI_2[, offence]) / 1000000)
    futile.logger::flog.info("build_incidents - Calculated the number of total incidents")
    
    # Bind together
    BI_5 = as.data.frame(cbind(male_incidents, female_incidents, all_incidents))
    futile.logger::flog.info("build_incidents - Binded the columns together")
    
    # Name the rows and columns
    column_names = config$BOTH_PARTS_OF_THE_TABLE$COL_NAMES
    rownames(BI_5) <- offence
    colnames(BI_5) <- column_names
    futile.logger::flog.info("build_incidents - Named the columns and rows for the variable")
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      BI_6 = BI_5
      futile.logger::flog.info("build_incidents - Created the BI_6 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      BI_6 = base::rbind(BI_6, BI_5)
      futile.logger::flog.info("build_incidents - Added BI_5 to the BI_6 dataframe")
      
    }
    
  } 
  
  # 9. Bind the unweighted bases together and add to the dataframe
  Rows_of_unweighted_bases = cbind(male_unweighted_base, female_unweighted_base, Unweighted_base)
  colnames(Rows_of_unweighted_bases) <- column_names
  BI_7 = rbind(BI_6, Rows_of_unweighted_bases)
  
  # 10. Name the rows from the config labels  
  
  rownames(BI_7) = lapply(config$PART_ONE$ROW_LABELS, `[[`, 2)
  futile.logger::flog.info("build_incidents - Renamed the rows for the complete data frame")
  
  # 11. Bind the unweighted bases together and add to the dataframe
  
  
  # 12. Return the final dataframe:
  return(BI_7)
  futile.logger::flog.info("build_incidents - Number of Incidents built")
  
}