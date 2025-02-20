#' @docType class
#'
#' @title A class to represent S4 examples
#'
#' @description This class is a simple example of an S4 class.
#' It inherits from the [SimpleList][S4Vectors::SimpleList] class.
#'
#' @importClassesFrom S4Vectors SimpleList
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
