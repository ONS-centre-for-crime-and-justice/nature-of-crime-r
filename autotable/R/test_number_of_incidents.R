config = yaml::read_yaml("D://Repos/27th August - Full Pipeline/crime-tables-r/configs/number_of_incidents_against_men_and_women/VIOLENCE.yaml")
master_config 
subtable = 3 
i = 1

Subtables_to_build = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 1)
Subtables_built    = vector("list", length(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES))
names(Subtables_built) = Subtables_to_build

number_of_incidents_build_incidents <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_NVF_datasets <- base::list.files(path = master_config$NVF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_offender_contact - Reading in source data from %s",
                             List_of_NVF_datasets[i])
    NVF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, List_of_NVF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_offender_contact - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    BI_1 = autotable::remove_zeroweighted_rows(df = NVF_dataset, weightvar = Weight_variable)
    futile.logger::flog.info("build_offender_contact - Removed %s cases out of %s because their weights were 0",
                             nrow(NVF_dataset) - nrow(BI_1), nrow(NVF_dataset))
    
    # 6. Extract the unweighted base: 
    
    Unweighted_base = nrow(BI_1)
    futile.logger::flog.info("build_incidents - determined the unweighted base as %s",
                             Unweighted_base)
    
    # 7. Weight the data 
    
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    BI_2 = autotable::apply_weight(BI_1, base::c(Variables_for_rows), weight = Weight_variable)
    futile.logger::flog.info("build_incidents - Weighted the data for the relevant variables")
    
    offence_codes = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 2)
    offence <- offence_codes[[subtable]]
    futile.logger::flog.info("build_incidents - Starting the calculations for %s", offence)
    
    # Calculate Male Means and Totals
    BI_3 = dplyr::filter(BI_2, sex == 1)
    male_unweighted_base = nrow(BI_3)
    male_incidents   = as.data.frame(sum(BI_3[, offence]) / 1000000)
    colnames(male_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of male incidents")
    
    # Calculate Female Means and Totals
    BI_4 = dplyr::filter(BI_2, sex == 2)
    female_unweighted_base = nrow(BI_4)
    female_incidents   = as.data.frame(sum(BI_4[, offence]) / 1000000)
    colnames(female_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of female incidents")
    
    # Calculate Total Means and Totals
    # MEAN CALCULATION HERE
    all_incidents   = as.data.frame(sum(BI_2[, offence]) / 1000000)
    colnames(all_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of total incidents")
    
    # Bind together
    BI_5 = as.data.frame(rbind(male_incidents, female_incidents, all_incidents))
    column_names = config$BOTH_PARTS_OF_THE_TABLE$COL_NAMES
    rownames(BI_5) = column_names
    futile.logger::flog.info("build_incidents - Binded the columns together")
                            
    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_offender_contact - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      BI_6 = BI_5
      futile.logger::flog.info("build_offender_contact - created BI_6 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      BI_6 = base::cbind(BI_6, BI_5)
      futile.logger::flog.info("build_offender_contact - Added column to the BI_6 dataframe")
    }
    
  }
  
  # 29. Return the final dataframe:
  return(BI_6)
  
}


number_of_incidents_build_offence_percentages <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_NVF_datasets <- base::list.files(path = master_config$NVF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_offender_contact - Reading in source data from %s",
                             List_of_NVF_datasets[i])
    NVF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, List_of_NVF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_offender_contact - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    BOT_1 = autotable::remove_zeroweighted_rows(df = NVF_dataset, weightvar = Weight_variable)
    futile.logger::flog.info("build_offender_contact - Removed %s cases out of %s because their weights were 0",
                             nrow(NVF_dataset) - nrow(BI_1), nrow(NVF_dataset))
    
    # 6. Extract the unweighted base: 
    
    Unweighted_base = nrow(BOT_1)
    futile.logger::flog.info("build_incidents - determined the unweighted base as %s",
                             Unweighted_base)
    
    # 7. Weight the data 
    
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    BOT_2 = autotable::apply_weight(BI_1, base::c(Variables_for_rows), weight = Weight_variable)
    futile.logger::flog.info("build_incidents - Weighted the data for the relevant variables")
    
    offence_codes = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 2)
    offence <- offence_codes[[subtable]]
    futile.logger::flog.info("build_incidents - Starting the calculations for %s", offence)
    
    # Calculate Male Means and Totals
    BOT_3 = dplyr::filter(BOT_2, sex == 1)
    male_unweighted_base = nrow(BOT_3)
    male_incidents   = as.data.frame(sum(BOT_3[, offence]) / 1000000)
    colnames(male_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of male incidents")
    
    # Calculate Female Means and Totals
    BOT_4 = dplyr::filter(BOT_2, sex == 2)
    female_unweighted_base = nrow(BOT_4)
    female_incidents   = as.data.frame(sum(BOT_4[, offence]) / 1000000)
    colnames(female_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of female incidents")
    
    # Calculate Total Means and Totals
    # MEAN CALCULATION HERE
    all_incidents   = as.data.frame(sum(BOT_2[, offence]) / 1000000)
    colnames(all_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of total incidents")
    
    # Bind together
    BOT_5 = as.data.frame(rbind(male_incidents, female_incidents, all_incidents))
    column_names = config$BOTH_PARTS_OF_THE_TABLE$COL_NAMES
    rownames(BOT_5) = column_names
    futile.logger::flog.info("build_incidents - Binded the columns together")
    
    # Create All violence totals 
    
    violence = "violnr_i"
    BOT_3a = dplyr::filter(BOT_2, sex == 1)
    male_unweighted_base = nrow(BOT_3a)
    male_incidents   = as.data.frame(sum(BOT_3a[, violence]) / 1000000)
    colnames(male_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of male incidents")
    
    # Calculate Female Means and Totals
    BOT_4a = dplyr::filter(BOT_2, sex == 2)
    female_unweighted_base = nrow(BOT_4a)
    female_incidents   = as.data.frame(sum(BOT_4a[, violence]) / 1000000)
    colnames(female_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of female incidents")
    
    # Calculate Total Means and Totals
    # MEAN CALCULATION HERE
    all_incidents   = as.data.frame(sum(BOT_2[, violence]) / 1000000)
    colnames(all_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of total incidents")
    
    # Bind together
    BOT_5a = as.data.frame(rbind(male_incidents, female_incidents, all_incidents))
    column_names = config$BOTH_PARTS_OF_THE_TABLE$COL_NAMES
    rownames(BOT_5) = column_names
    futile.logger::flog.info("build_incidents - Created All Violence for comparison")
    
    # Divide Current offence by total 
    
    BOT_6 = as.data.frame(BOT_5 / BOT_5a * 100)
    
    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_offender_contact - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      BOT_7 = BOT_6
      futile.logger::flog.info("build_offender_contact - created BOT_7 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      BOT_7 = base::cbind(BOT_7, BOT_6)
      futile.logger::flog.info("build_offender_contact - Added column to the BOT_7 dataframe")
    }
    
  }
  
  # 29. Return the final dataframe:
  return(BOT_7)
  
}


i = 5 

number_of_incidents_build_sex_percentages <- function(config, master_config, subtable) {
  
  # 1. Find out which years we are building tables for:
  Years_of_data = lapply(config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES, `[[`, 1)
  
  # 2. Get a list of the VF datasets to process:
  List_of_NVF_datasets <- base::list.files(path = master_config$NVF_DATA_DIRECTORY)
  
  # Below, we will use a for-loop to process the years of data one-by-one.
  
  for (i in c(1:length(Years_of_data))) {
    
    # 3. Read in the VF dataset for this year:
    futile.logger::flog.info("build_offender_contact - Reading in source data from %s",
                             List_of_NVF_datasets[i])
    NVF_dataset <- base::as.data.frame(data.table::fread(base::paste(master_config$NVF_DATA_DIRECTORY, List_of_NVF_datasets[i], sep = '/')))
    
    # 4. Fetch the correct weight variable for this year of data:
    Weight_variable = config$BOTH_PARTS_OF_THE_TABLE$WEIGHT_VARIABLES[[i]][[2]]
    futile.logger::flog.info("build_offender_contact - Using %s as the weight variable", Weight_variable)
    
    # 5. Remove zero-weighted cases from the dataset:
    BIS_1 = autotable::remove_zeroweighted_rows(df = NVF_dataset, weightvar = Weight_variable)
    futile.logger::flog.info("build_offender_contact - Removed %s cases out of %s because their weights were 0",
                             nrow(NVF_dataset) - nrow(BI_1), nrow(NVF_dataset))
    
    # 6. Extract the unweighted base: 
    
    Unweighted_base = nrow(BIS_1)
    futile.logger::flog.info("build_incidents - determined the unweighted base as %s",
                             Unweighted_base)
    
    # 7. Weight the data 
    
    Variables_for_rows = unlist(lapply(config$PART_ONE$ROW_LABELS, `[[`, 1))
    BIS_2 = autotable::apply_weight(BI_1, base::c(Variables_for_rows), weight = Weight_variable)
    futile.logger::flog.info("build_incidents - Weighted the data for the relevant variables")
    
    offence_codes = lapply(config$BOTH_PARTS_OF_THE_TABLE$OFFENCE_CODES, `[[`, 2)
    offence <- offence_codes[[subtable]]
    futile.logger::flog.info("build_incidents - Starting the calculations for %s", offence)
    
    # Calculate Male Means and Totals
    BIS_3 = dplyr::filter(BIS_2, sex == 1)
    male_unweighted_base = nrow(BIS_3)
    male_incidents   = as.data.frame(sum(BIS_3[, offence]) / 1000000)
    colnames(male_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of male incidents")
    
    # Calculate Female Means and Totals
    BIS_4 = dplyr::filter(BIS_2, sex == 2)
    female_unweighted_base = nrow(BIS_4)
    female_incidents   = as.data.frame(sum(BIS_4[, offence]) / 1000000)
    colnames(female_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of female incidents")
    
    # Calculate Total Means and Totals
    # MEAN CALCULATION HERE
    all_incidents   = as.data.frame(sum(BIS_2[, offence]) / 1000000)
    colnames(all_incidents) = " "
    futile.logger::flog.info("build_incidents - Calculated the number of total incidents")
    
    # Bind together
    BIS_5 = as.data.frame(rbind(male_incidents, female_incidents, all_incidents))
    column_names = config$BOTH_PARTS_OF_THE_TABLE$COL_NAMES
    rownames(BIS_5) = column_names
    futile.logger::flog.info("build_incidents - Binded the columns together")
    
    # Divide Current offence by total 
    
    All_row = BIS_5[3, ]
    BIS_6 = as.data.frame(BIS_5 / All_row * 100)
    
    # 13. Generate a name for this column, which we will later apply in Step 21:
    Start_year              = substr(Years_of_data[[i]], 3, 4)
    End_year                = as.numeric(Start_year) + 1
    names(Years_of_data)[i] = paste0("Apr '", Start_year, " to Mar '", End_year)
    futile.logger::flog.info("build_offender_contact - Finished column \"%s\"", names(Years_of_data)[i])
    
    # 14. We now want to add this column into our final dataframe:
    
    if (i == 1) { # If this is out first iteration through the loop, then we don't have a dataframe to add to, however! So in
      # that case, we must create one:
      
      BIS_7 = BIS_6
      futile.logger::flog.info("build_offender_contact - created BIS_7 dataframe")
      
    } else { # But if it's not our first iteration, then we will just add the column to the dataframe that already exists:
      
      BIS_7 = base::cbind(BIS_7, BIS_6)
      futile.logger::flog.info("build_offender_contact - Added column to the BIS_7 dataframe")
    }
    
  }
  
  # 29. Return the final dataframe:
  return(BIS_7)
  
}