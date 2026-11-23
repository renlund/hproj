##' @title elements of uniformal 'nchar' length
##' @description make nchar have the same value for all elements of x
##' @title fixed length
##' @param x vector to fix
##' @param n interger; uniformal nchar length for elements of x
##' @param fill character to fill the void
##' @param right logical; fill to the right?
##' @param trunc.right logical; if n is less than the maximum character length in x, do
##'     you want truncation from the right?
##' @return character string
##' @export
##' @examples
##' equal_nchar(8:12, fill = "0")
##' equal_nchar(x = c("A", "ABC"))
##' equal_nchar(x = c("A", "ABC"), n = 4, fill = "_")
##' equal_nchar(x = c("A", "ABC"), fill = ".", right = TRUE)
##' equal_nchar(x = c("A", "ABC"), n = 2)
##' equal_nchar(x = c("A", "ABC"), n = 2, trunc.right = FALSE)
equal_nchar <- function(x, n = NULL, fill = " ", right = FALSE,
                        trunc.right = TRUE){
    properties(fill, class = "character", length = 1, na.ok = FALSE)
    properties(right, class = "logical", length = 1, na.ok = FALSE)
    properties(trunc.right, class = "logical", length = 1, na.ok = FALSE)
    N <- length(x)
    R <- rep(NA_character_, N)
    notna <- which(!is.na(x))
    y <- as.character(x[!is.na(x)])
    M <- max(nchar(y))
    if(is.null(n)) n <- M else n <- type.convert(n, as.is = TRUE)
    properties(n, class = "integer", length = 1, na.ok = FALSE)
    z <- y
    for(k in seq_along(y)){
        m <- nchar(y[k])
        if(m < n){
            z[k] <- if(right){
                        paste0(y[k], paste(rep(fill, n-m), collapse = ""))
                    } else {
                        paste0(paste(rep(fill, n-m), collapse = ""), y[k])
                    }
        } else if(m == n){
            next
        } else if(m > n){
            if(trunc.right){
                z[k] <- substr(y[k], 1, n)
            } else {
                z[k] <- substr(y[k], m - n + 1, m)
            }
        }
    }
    R[notna] <- z
    R
}
