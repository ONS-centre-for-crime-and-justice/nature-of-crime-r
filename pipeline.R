# 1. Install (if necessary) and load the required R packages...
required_R_packages = base::c("autotable", "data.table", "dplyr", "futile.logger", "reticulate", "yaml")
for (package in required_R_packages) {
  if (!require(package, character.only = TRUE)) {
    utils::install.packages(package)
    library(package, character.only = TRUE)
  }
}

futile.logger::flog.info("PIPELINE - Packages loaded and logger initialised")

# ... and we'll also need the gptables package from Python: 
reticulate::use_condaenv("gptables")
gptables = reticulate::import("gptables")

# 2. Set up the logger:
autotable::set_up_logger(path = "logfiles/", write_to_file = FALSE)
start_time = proc.time()
futile.logger::flog.trace("PIPELINE - Packages loaded and logger initialised")

# 3. Load the matrix of Tables-to-build into R, and read in the master config:
source("Tables_to_build.R")
futile.logger::flog.trace("PIPELINE - Fetched the matrix of tables-to-build")

master_config = yaml::read_yaml(file = "configs/MASTER.yaml", fileEncoding = "UTF-8")
futile.logger::flog.trace("PIPELINE - Read in the master config")

# 4. Loop through every cell of the matrix, left-to-right, top-to-bottom, building the tables:

for (i in c(1:base::nrow(tables_to_build))) {
  
  for (j in c(1:base::ncol(tables_to_build))) {
    
    if (tables_to_build[i,j]) { # If the cell indicates that we should build a table, then call the appropriate function
                                # with the appropriate config file as an argument, storing the returned value in its allotted
                                # place:
      
      futile.logger::flog.info("PIPELINE - Now building the %s table for %s", base::colnames(tables_to_build)[j],
                               base::rownames(tables_to_build)[i])
      
      config_file_to_use     = autotable::process_config(yaml::read_yaml(file = base::paste0("configs/", base::colnames(tables_to_build)[j], '/',
                                                                         base::rownames(tables_to_build)[i], ".yaml")))
      function_to_use        = base::match.fun(base::colnames(tables_to_build)[j])
      
      tables_built[[i]][[j]] = function_to_use(config = config_file_to_use, master_config = master_config)
      
      futile.logger::flog.info("PIPELINE - SUCCESS: the %s table for %s has been built", base::colnames(tables_to_build)[j],
                               base::rownames(tables_to_build)[i])
      
    } else {
      
      futile.logger::flog.trace("PIPELINE - Skipping the %s table for %s, as instructed by the matrix",
                               base::colnames(tables_to_build)[j], base::rownames(tables_to_build)[i])
    }
    
  }
  
}

# 5. Process the Tables_built into Excel workbooks:

workbook_names = lapply(master_config$EXCEL_WORKBOOK_NAMES, `[[`, 2)

for (i in c(1:length(workbook_names))) {
  
  if (length(unlist(tables_built[[i]])) == 0) {
    next
  }
  
  file = paste0("output/", workbook_names[[i]])
  
  sheet_names    = autotable::generate_sheet_names(lengths(tables_built[[i]])[lengths(tables_built[[i]]) > 0])
  sheets         = unlist(tables_built[[i]])
  names(sheets)  = sheet_names
  
  # Add a cover page to the front of the workbook:
  cover = gptables$Cover(
  cover_label = "Notes",
  title       = master_config$COVER_SHEET_TITLES[i],
  intro       = as.list("Data tables shown in this workbook relate to the Crime Survey for England and Wales (CSEW)."),
  about       = c("The data contained in these tables are from the Crime Survey for England and Wales (CSEW).",
                   "For more information on these data, see the User guide to crime statistics for England and Wales",
                 "These tables have been built using R and the code is available here – https://github.com/ONS-centre-for-crime-and-justice",
                  "Following a methodological change to the handling of repeat victimisation in the CSEW, these data are not comparable with data published before January 2019. For more information see ‘Improving victimisation estimates derived from the Crime Survey for England and Wales’",
                 "For further information about the Crime Survey for England and Wales (CSEW) and police recorded crime statistics, please email crimestatistics@ons.gov.uk",
                 "or write to: ONS Centre for Crime and Justice, Office for National Statistics, Room 2200, Segensworth Road, Titchfield, PO15 5RR"),
  contact     = c("Statistical contact: Nick Stripe",
                  "Tel: 01329 444651",
                  "Email: crimestatistics@ons.gov.uk"))
  
  
  Excel_workbook = gptables$produce_workbook(filename = file,
                                             sheets   = sheets,
                                             theme    = gptables$Theme("configs/FORMATTING.yaml"),
                                             cover    = cover)

  # Adjust row heights:
  for (j in c(1:length(sheets))) {

    table     = sheets[[j]]$table
    base_rows = grep("Unweighted", table[, grep("Unweighted", table, ignore.case = TRUE)], ignore.case = TRUE) + 4
    
    if (length(base_rows) == 0) {
      next 
    } else if (length(base_rows) == 1) {
      rows_to_adjust = as.character(base_rows)
    } else {
      rows_to_adjust = as.character(c(base_rows, base_rows[base_rows != max(base_rows)] + 1))
    }
    
    worksheet = Excel_workbook$worksheets()[[j + 1]]
    for (row in rows_to_adjust) {
      worksheet$set_row(py_eval(row), py_eval("30"))
    }
        
  }
  
  # Adjust index column widths:
  for (j in c(1:length(sheets))) {
    
    table = sheets[[j]]$table
    worksheet = Excel_workbook$worksheets()[[j + 1]]

    index1_column = grep("index1", colnames(table)) - 1
    index2_column = grep("index2", colnames(table)) - 1
    index3_column = grep("index3", colnames(table)) - 1
    
    if (length(index1_column) > 0) {
      width = max(nchar(gsub("\\$", "", gsub("Note_", "()", table[, index1_column + 1])))) * 0.8155 + 1
      worksheet$set_column(py_eval(as.character(index1_column)), py_eval(as.character(index1_column)), py_eval(as.character(width)))
    }
    if (length(index2_column) > 0) {
      width = max(nchar(gsub("\\$", "", gsub("Note_", "()", table[, index2_column + 1])))) * 0.7455 + 1
      worksheet$set_column(py_eval(as.character(index2_column)), py_eval(as.character(index2_column)), py_eval(as.character(width)))
    }
    if (length(index3_column) > 0) {
      width = max(nchar(gsub("\\$", "", gsub("Note_", "()", table[, index3_column + 1])))) * 0.7455 + 1
      worksheet$set_column(py_eval(as.character(index3_column)), py_eval(as.character(index3_column)), py_eval(as.character(width)))
    }
    
  }
  
  # Adjust significance column widths:
  for (j in c(1:length(sheets))) {
    
    table = sheets[[j]]$table

    worksheet = Excel_workbook$worksheets()[[j + 1]]

    significance_column_1 = grep(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`1_YEAR_TEST`, colnames(table))  - 1
    significance_column_2 = grep(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`7_YEAR_TEST`, colnames(table))  - 1
    significance_column_3 = grep(master_config$SIGNIFICANCE_TESTING$COLUMN_LABELS$`10_YEAR_TEST`, colnames(table)) - 1
    
    if (length(significance_column_1) > 0) {
      width = min(nchar(colnames(table)[significance_column_1])*0.8155+1, 12.5)
      worksheet$set_column(py_eval(as.character(significance_column_1)), py_eval(as.character(significance_column_1)), py_eval(as.character(width)))
    }
    if (length(significance_column_2) > 0) {
      width = min(nchar(colnames(table)[significance_column_2])*0.8155+1, 12.5)
      worksheet$set_column(py_eval(as.character(significance_column_2)), py_eval(as.character(significance_column_2)), py_eval(as.character(width)))
    }
    if (length(significance_column_3) > 0) {
      width = min(nchar(colnames(table)[significance_column_3])*0.8155+1, 12.5)
      worksheet$set_column(py_eval(as.character(significance_column_3)), py_eval(as.character(significance_column_3)), py_eval(as.character(width)))
    }
    
  }

  # Close the workbook:
  Excel_workbook = Excel_workbook$close()
  futile.logger::flog.info("PIPELINE - Workbook created: %s", file)
  
}

futile.logger::flog.info("PIPELINE - Finished running successfully. Elapsed time: %s minutes",
                         round((proc.time() - start_time)[3] / 60, 1))
