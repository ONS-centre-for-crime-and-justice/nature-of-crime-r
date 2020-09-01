#'@title remove_zeroweighted_rows
#'
#'@description Strips out rows where the record has a weight of zero
#'
#'@details Given a specified weight column, this function deletes records from the input file that have a weight of 0 and returns a data frame with these records removed.
#'
#'@param df - a file to remove the zero weighted rows from
#'
#'@param weightvar - the variable to use as the criterion for whether a row should be deleted
#'
#'@return Data Frame
#'
#'@examples remove_zeroweighted_rows(mtcars, "vs")
#'
#'@export


remove_zeroweighted_rows <- function(df, weightvar){
  
  output <- dplyr::filter(df, base::get(weightvar) > 0)
  
  return(output)

}

