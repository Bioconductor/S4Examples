#' @docType class
#'
#' @title A class to represent S4 examples
#'
#' @description This class is a simple example of an S4 class.
#' It inherits from the [SimpleList][S4Vectors::SimpleList] class.
#'
#' @importClassesFrom S4Vectors SimpleList
#' @importFrom methods setClass new
#'
#' @examples
#' showClass("Example")
#'
#' @exportClass Example
.Example <- setClass(
    Class = "Example", contains = "SimpleList"
)

#' @rdname Example-class
#'
#' @param x Either a 'list' or a 'SimpleList' object (defaults to an empty
#'   `list`)
#'
#' @importFrom methods is
#'
#' @examples
#' library(S4Vectors)
#'
#' Example()
#' Example(list(a = 1, b = 2))
#' Example(SimpleList(a = 1, b = 2))
#' @export
Example <- function(x = list()) {
    if (!(is.list(x) || is(x, "SimpleList")))
        stop("x must be a 'list' or a 'SimpleList'")

    .Example(x)
}

.checkElementLengths <- function(object) {
    errors <- NULL
    all_names <- names(object)
    ## all element names must be in the alphabet (lower case)
    if (!all(all_names %in% letters)) {
        msg <- "All element names must be in the alphabet"
        errors <- c(msg, errors)
    }

    if (!identical(all_names, tolower(all_names))) {
        msg <- "All element names must be lower case"
        errors <- c(msg, errors)
    }

    errors
}

.checkElementNames <- function(object) {
    errors <- NULL
    all_lengths <- unique(lengths(object))

    ## create condition where each element must have equal length
    if (!identical(1L, length(all_lengths))) {
        msg <- "All elements in Example must have the same length"
        errors <- c(msg, errors)
    }

    errors
}

.validExample <- function(object) {
    if (length(object)) {
        c(
            .checkElementLengths(object),
            .checkElementNames(object)
        )
    }
}

S4Vectors::setValidity2("Example", .validExample)

#' @name Example-class
#'
#' @aliases coerce,Example,DataFrame-method
#'
#' @section Coercion:
#' In the code example below, 'from' is an object of class `Example`.
#'
#' `as(from, "DataFrame")`: Creates a `DataFrame` object from an `Example`
#' class. Instances of the `Example` class have identical lengths for all
#' elements.
#'
#' @importFrom methods coerce
#'
#' @examples
#' as(
#'     Example(x = list()), "DataFrame"
#' )
#' @exportMethod coerce
setAs("Example", "DataFrame", function(from) {
    if (isEmpty(from))
        cols <- TRUE
    else
        cols <- c("group_name", "value")
    S4Vectors::DataFrame(from)[, cols]
})
