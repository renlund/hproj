##' @title compile main rnw file
##' @description Compile source or dm-source. Leave arguments empty to let
##'     \code{cmp} figure out which one you want.
##' @param dm logical; if TRUE choose dm-source. If NULL 'dm' is set to the
##'     hproj option 'dm_active'.
##' @param ... arguments passed to \code{\link[knitr]{knit2pdf}}
##' @export
cmp <- function(dm = NULL, ...){
    opts_hproj$check()
    cmp_rnw(input = holy_get("source_file", dm = dm),
            output = holy_get("output_file", dm = dm), ...)
}

##' @describeIn cmp shorthand for \code{cmp(dm=TRUE)}
cmp_dm <- function() cmp(dm = TRUE)

##' @describeIn cmp the function that compiles; a wrapper for
##'     \code{knitr::knit2pdf}
##' @param input file to compile
##' @param output output tex file
##' @export
##' @importFrom knitr knit2pdf
cmp_rnw <- function(input, output, ...){
    if(!grepl("\\.(R|r)nw$", input)){
        stop("methods for non-Rnw files not implemented")
    }
    ut <- sub("\\.pdf$", "\\.tex", output)
    knitr::knit2pdf(
        input  = input,
        output = ut,
        envir  = .GlobalEnv,
        ...
    )
    invisible(NULL)
}
