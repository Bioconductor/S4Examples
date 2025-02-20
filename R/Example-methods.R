
#' Create a long form method for Example class
#'
#' @importFrom BiocGenerics longForm
#'
#' @param object An object of class `Example`
#'
#' @param ... Additional arguments
#'
#' @returns A long form of the object
#'
#' @exportMethod longForm
setMethod(
    "longForm", "Example", function(object, ...) {
        print("longForm method for Example class")
    }
)
