#'@title process_config
#'
#'@description Processes a Nature of Crime config file, parsing and evaluating any strings that begin with
#'"master_config".
#'
#'@details This function allows config files to make reference to the master config file. This means that
#'updating the master config file will effectively update all config files which have references to the master
#'config file, potentially saving a lot of time and hassle when changing something that affects every table. That is,
#'instead of having to go into the config file for every table and making 100 of the same update, you can update it just
#'once in the master_config, and all the fields referencing the master_config will auto-update.
#'
#'@param config A list of lists (and other items) which is the result of applying yaml::read.yaml() to a Nature of Crime
#'.yaml config file.
#'
#'@return The original list of lists given by \code{config} except where any strings beginning with "master_config" have
#'been replaced with the corresponding object from the master config file.
#'
#'@examples process_config(config = read.yaml("configs/age_of_vehicle_stolen/VEHICLE_RELATED_THEFT.yaml"))
#'
#'@export

process_config = function(config) {

  config = lapply(config, function(x) if(is.list(x)) process_config(x) else sapply(x, function(y) if (grepl("master_config", y)) {eval(parse(text = y))} else {y}, USE.NAMES = FALSE))
  return (config)

}
