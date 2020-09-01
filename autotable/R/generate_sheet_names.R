#'@title generate_sheet_names
#'
#'@description Appropriately generates sheet names for an Excel workbook-to-be, where each sheet name is of the form "Table XY",
#'for X a number, and Y a lowercase letter of the alphabet or an empty string.
#'
#'@details This function takes in a numeric vector \code{Workbook_structure} from which it generates the sheet names, which are
#'returned as a character vector. The function supports a maximum of 25 subtables per table.
#'
#'@param Workbook_structure A numeric vector, where each slot in the vector respresents a different main table, and the number
#'in that slot represents how many subtables that table has. For instance, the vector \code{c(3,4,6,8)} indicates that the first
#'table has 8 subtables, the second table has 4, the third has 6, and the fourth has 8. Each element of the vector should be
#'an integer between 1 and 25 inclusive.
#'
#'@return A character vector of the sheet names, in order.
#'
#'@examples \code{generate_sheet_names(Workbook_structure = c(1, 3, 2))} # Returns c("Table 1", "Table 2a", "Table 2b",
#'"Table 2c", "Table 3a", "Table 3b")
#'
#'@export
#'

generate_sheet_names = function(Workbook_structure) {

  # Initialising:
  Sheet_names = NULL
  X           = 1
  Y_values    = c('', letters)

  for (Table in Workbook_structure) {

    Y_index = ifelse(Table == 1, 1, 2)

    repeat {
      Y = Y_values[Y_index]
      Sheet_names = c(Sheet_names, paste0("Table ", X, Y))
      Y_index = Y_index + 1

      if (Table == 1 | Y_index - Table > 1) {
        break
      }
    }

    # Increment X and reset the Y_index:
    X = X + 1
    Y_index = 1

  }

  return(Sheet_names)

}
