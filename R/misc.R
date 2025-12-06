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


shuffle <- function (x, y){
    n <- length(x)
    if (length(y) != n) stop("no shuffle for you!")
    N <- 2 * n
    r <- rep(NA, N)
    r[seq(1, N, 2)] <- x
    r[seq(2, N, 2)] <- y
    r
}

##' group data.frame
##'
##' split a data frame by inserting lines according to grouping specified by a
##' variable 'group'.
##' @param object a data.frame
##' @param group character, name of grouping variable
##' @param onto character/logical, the name of varible that grouping values are
##'     entered onto, or, if TRUE, enter grouping values onto all
##' @param ontoprefix if not null, paste this as a prefix on the onto variable
##'     (only applicable if 'onto' is character)
##' @examples
##' d <- data.frame(hey = sample(c("John", "Jane", "Anaximandros"), 10, TRUE),
##'                 numy = round(runif(10, 15,25), 1),
##'                 foo = rep(LETTERS[1:3], c(2, 5, 3)),
##'                 bar = letters[1:10],
##'                 measx = round(rnorm(10,10,1), 1))
##' group_frame(object = d, group = "foo", onto = "bar")
##' group_frame(object = d, group = "foo", onto = TRUE)
##' @export
group_frame <- function(object, group, onto, ontoprefix = "    "){
    properties(object, class = "data.frame")
    properties(group, class = "character", length = 1, na.ok = FALSE)
    properties(onto, class = c("character", "logical"),
               length = 1, na.ok = FALSE)
    onto_term <- is.character(onto)
    if(!is.null(ontoprefix)){
        properties(ontoprefix, class = "character", length = 1, na.ok = FALSE)
    }
    rl <- rle(object[[group]])
    if(any(duplicated(rl$values))){
        warning("grouping variable not contiguous")
    }
    n <- length(rl$values)
    index <- shuffle(rep(0, n), rl$lengths)
    x <- shuffle(rep(1, n), rl$lengths)
    cx <- cumsum(x)
    N <- max(cx)
    D <- NULL
    group_i <- which(names(object) == group)
    R <- object[rep(NA, N), -group_i]
    rownames(R) <- NULL
    i1 <- cx[index == 0]
    i2 <- setdiff(1:N, i1)
    if(onto_term){
        char_i <- which(unlist(lapply(R, FUN = is.character)))
        R[char_i] <- lapply(R[char_i], FUN = \(x) {x[i1] <- ""; x})
        R[[onto]][i1] <- rl$values
    } else if(isTRUE(onto)){
        R[] <- lapply(R, FUN = \(x){x[i1] <- rl$values; x})
    }
    for(nm in names(R)){
        if(!is.null(ontoprefix) && onto_term && nm == onto){
            R[[nm]][i2] <- paste0(ontoprefix, object[[nm]])
            next
        }
        R[[nm]][i2] <- object[[nm]]
    }
    R
}
