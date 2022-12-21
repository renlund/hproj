##' some palettes
##'
##' collection of some commonly used palettes
##' @param p character, one of the available palettes
##' @param show logical; if TRUE plot the palette
##' @return character vector of color codes; possible a plot as a side effect
##' @export
farben <- function(p, show = FALSE){
    ps <- c("ucr", "nejm")
    if( !(p %in% ps) ){
        s <- paste0("p should be one of {", paste(ps, collapse = ", "), "}")
        stop(s)
    }
    if(p=="ucr"){
        r <- c("#61A60E", "#3A96B4", "#DC6511", "#6E3784", "#005E85")
    }
    if(p == "nejm"){
        r <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF",
               "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF")
    }
    if (show) {
        grDevices::dev.new()
        graphics::barplot(table(factor(r, levels = r)),
                          col = r, yaxt = "n")
    }
    r
}

##' format CI
##'
##' Format CI as text "estimate (lower bound, upper bound)". Note to self: this
##' is mainly a function since you (the author) always forget.
##' @param est estimate
##' @param low lower bound
##' @param high upper bound
##' @param d digits
##' @param space1 logical; space between 'estimate' and parenthesis
##' @param space2 logical; space between comma and 'upper bound'
##' @examples
##' e  = c(1,     1.501, 1.499)
##' lo  = c(0.899, 1.250, 1.000)
##' hi = c(1.455, 1.950, 2)
##' citxt(e, lo, hi)
##' @export
citxt <- function(est, low, high, d = 2, space1 = TRUE, space2 = TRUE){
    s <- paste0("%.", d, "f", if(space1) " " else "",
                "(%.", d, "f,", if(space2) " " else "",
                "%.", d, "f)")
    sprintf(s, est, low, high)
}
