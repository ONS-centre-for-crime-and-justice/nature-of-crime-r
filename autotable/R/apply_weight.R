#'@title apply_weight
#'
#'@description applies a weight: mutiplies selected columns by another column (the weight)
#'
#'@details The function takes a list of columns and mutiplies each value (or row) by the value in another column
#'which is the weight column
#'
#'@param df The dataframe to manuplicate
#'
#'@param columns The columns to apply the weight too
#'
#'@param weight The column by which to mutiple the other columns by, supplied as a character string
#'
#'@return The original df but witht the selected columns altered
#'
#'@examples apply_weight(mtcars, columns= c("hp", "drat"), weight= "carb")
#'
#'@export

apply_weight <- function(df, columns, weight){


  df[, columns] <- df[, columns] * df[, weight]

  return(df)

}

