##' shuffle vectors
##'
##' create a vector of double the length of the input vectors (which have the
##' same length) where every element with an odd index is from x and the others
##' are from y
##' @param x vector
##' @param y vector of same length as x
##' @return vector of
##' @export
shuffle <- function (x, y){
    n <- length(x)
    if (length(y) != n)
        stop("no shuffle for you! (arguments have different lengths)")
    N <- 2 * n
    r <- rep(NA, N)
    r[seq(1, N, 2)] <- x
    r[seq(2, N, 2)] <- y
    r
}
