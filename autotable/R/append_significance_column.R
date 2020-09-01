#'@title append_significance_column
#'
#'@description Performs z-tests across two columns of a dataframe, testing each cell of the first column against the
#'corresponding cell of the second; the results of these tests are appended as a column to the right of the original
#'dataframe.
#'
#'@details This function repeatedly calls \code{autotable::z_test_crime} to test each value of \code{column_1} against
#'its counterpart in \code{column_2}. If a result is significanct, it will be indicated as such by
#'\code{significance_indicator} in the appended column; if it is not, then it will be indicated as such by
#'\code{insignificance_indicator}.
#'
#'@param dataframe A numeric \code{dataframe} containing 1 row of unweighted bases and at least 2 columns.
#'
#'@param column_1 An integer specifying a column of \code{dataframe}, different to \code{column_2}.
#'
#'@param column_2 An integer specifying a column of \code{dataframe}, different to \code{column_1}.
#'
#'@param design_1 TO BE ADDED LATER.
#'
#'@param design_2 TO BE ADDED LATER.
#'
#'@param row_of_unweighted_bases An integer indicating which row of \code{dataframe} holds the unweighted bases.
#'
#'@param threshold TO BE ADDED LATER.
#'
#'@param significance_indicator A string specifying how significance should be indicated in the appended column.
#'
#'@param insignificance_indicator A string specifying how insignificance should be indicated in the appended column.
#'
#'@return The original \code{dataframe}, except with the new significance column appended to its right.
#'
#'@export
#'

append_significance_column = function(dataframe,
                                      column_1,
                                      column_2,
                                      design_1 = 1.2,
                                      design_2 = 1.2,
                                      row_of_unweighted_bases,
                                      threshold = 1.96,
                                      significance_indicator = '*',
                                      insignificance_indicator = ' ') {

  Column_to_append = c()

  for (i in c(1:(nrow(dataframe)))) {
    Column_to_append = c(Column_to_append,
                         z_test_crime(estimate_1               = dataframe[i, column_1],
                                      estimate_2               = dataframe[i, column_2],
                                      base_1                   = dataframe[row_of_unweighted_bases, column_1],
                                      base_2                   = dataframe[row_of_unweighted_bases, column_2],
                                      design_1                 = design_1,
                                      design_2                 = design_2,
                                      threshold                = threshold,
                                      significance_indicator   = significance_indicator,
                                      insignificance_indicator = insignificance_indicator))
  }

  Column_to_append[row_of_unweighted_bases] = ' '
  Column_to_append[is.na(Column_to_append)] = ' '

  return(cbind(dataframe, Column_to_append))

}

