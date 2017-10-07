#' valid
#' @param x an object
#' @param class a 'character'
#' @description Whereas 'is()' returns TRUE if an
#' object already has a class, 'valid()' returns TRUE if
#' an object passes all the validity tests required by
#' the class. When a valid() method provided, if this 
#' function returns TRUE, then the class constructor
#' should not throw an error if called with the same
#' data, since the constructor uses this code for 
#' validation.
#' @export
valid <- function(x, class1) {
    class(x) <- c(class1, class(x))
    UseMethod("valid", x)
}
