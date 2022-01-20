#' @title view pdf
#' @description Leave arguments empty to open the current pdf version of the
#'     report/DM file, or point to any pdf, as this is just a wrapper for
#'     \code{shell.exec}
#' @param file file of interest
#' @param dm logical; if TRUE divert to DM-file if 'file' is NULL
#' @export
look <- function(file = NULL, dm = NULL){
    if(is.null(file)) file <- holy_get("output_file", dm = dm)
    shell.exec(file)
    invisible(NULL)
}

#' @describeIn look shorthand for \code{look(dm = TRUE)}
#' @export
look_dm <- function() look(file = NULL, dm = TRUE)
