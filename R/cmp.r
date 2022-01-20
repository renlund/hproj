##' @title compile main rnw file
##' @description compile source or dm-source
##' @param dm logical; if TRUE choose dm-source. If NULL the hproj option
##'     'dm_active' is chosen
##' @param ... arguments passed to \code{\link[knitr]{knit2pdf}}
##' @export
cmp <- function(dm = NULL, ...){
    if(is.null(dm)){
        s <- paste0("DM status is unclear.")
        dm <- force_dm_status(s)
    }
    opts_hproj$check()
    if(dm){
        input <- opts_hproj$get("dm_source_file")
        output <- opts_hproj$get("dm_output_file")
    } else {
        input <- opts_hproj$get("source_file")
        output <- opts_hproj$get("output_file")
    }
    cmp_rnw(input = input, output = output, ...)
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
