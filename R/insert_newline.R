##' insert linebreaks to a character vector
##'
##' Given a maximum number of character 'n', insert a linebreak before this number
##'     is reached. N.B. Words longer than 'n' will however NOT be abbreviated.
##' @title add linebreaks
##' @param s a character string
##' @param n a maximum number of characters
##' @param linebreak defaults to newline \code{"\n"}
##' @param splitby defaults to space \code{" "}
##' @param max.it maximum number of iterations of a while loop
##' @return character vector
##' @examples
##'     s <- paste0("A very long and perhaps supercalifragilisticexpialidocious",
##'                 " sentence in desparate need of linebreaks")
##'     cat(insert_linebreak(s, n = 20))
##'     cat(insert_linebreak(s, n = 30))
##'     cat(insert_linebreak(s, n = 7))
##'     cat(insert_linebreak(s, n = 100))
##' @export
insert_linebreak <- function(s, n, linebreak = "\n", splitby = " ", max.it = 10000){
    properties(x = s, class = c("character", "factor"))
    properties(x = n, class = "numeric", length = 1, na.ok = FALSE)
    properties(x = linebreak, class = "character", length = 1, na.ok = FALSE)
    properties(x = splitby, class = "character", length = 1, na.ok = FALSE)
    properties(x = max.it, class = c("integer", "numeric"), length = 1, na.ok = FALSE)
    if(n < 1) stop("need n to be at least 1")
    if("factor" %in% class(s)){
        r <- factor(x = as.numeric(s),
                    labels = insert_linebreak(
                        s = levels(s), n = n, linebreak = linebreak,
                        splitby = splitby
                    ))
        return(r)
    }
    ORIGINAL <- s
    R <- rep(NA_character_, length(ORIGINAL))
    for(index in seq_along(s)){ ## index = 1
        s <- ORIGINAL[index]
        x <- unlist(strsplit(s, splitby))
        xn <- unlist(lapply(x, nchar))
        cxn <- cumsum(xn)
        is <- NULL
        dummy <- 0
        while(length(cxn) > 0 & dummy < max.it){
            dummy <- dummy + 1
            if(cxn[1] <= n){
                m <- max(cxn[cxn <= n])
                i <- which(cxn == m)
            } else {
                m <- cxn[1]
                i <- 1
            }
            cxn <- (cxn - m)[cxn-m > 0]
            is <- c(is, i)
        }
        R[index] <- if(length(is) > 1){
            id <- 1
            S <- NULL
            for(i in is){ ## i = is[1]
                S <- c(S, paste0(x[id:(id+i-1)], collapse = splitby))
                id <- id + i
            }
            paste0(S, collapse = linebreak)
        } else {
            s
        }
    }
    R
}

if(FALSE){
    s = c("Suslf nk fsdnjk fnsdnj  sdjkfnk sjdnf",
          "asjkdb  abhasdjb jas asbh adsbh jad",
          "sd IUAHFIASHF IAH ASFK ndf ksd")
    insert_linebreak(s, n = 12)
}
