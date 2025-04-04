#' @rdname Example-methods
#'
#' @title Create a long form method for Example class
#'
#' @importFrom BiocGenerics longForm
#'
#' @param object An object of class `Example`
#'
#' @param ... Additional arguments
#'
#' @returns A long form of the object
#'
#' @examples
#' ex <- Example(list(a = 1, b = 2, c = 3))
#' longForm(ex)
#' @exportMethod longForm
setMethod(
    "longForm", "Example", function(object, ...) {
        ## lengths in object must be constant
        all_lengths <- unique(lengths(object))
        if (identical(length(all_lengths), 1L))
            as.data.frame(object)[, c("group_name", "value")]
        else
            stop("All elements in object must have the same length")
    }
)
