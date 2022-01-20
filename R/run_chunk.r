#' @title run chunks
#' @description Run a chunk in a given document
#' @param chunk name or number of chunk, if \code{NULL} (default) then the first
#'     chunk will be executed
#' @param file the file which has the chunks, default 'source_file'
#' @param dm if TRUE when file is NULL, then file will be gotten from hproj options
#' @param envir is \code{.GlobalEnv} by default, the environemnt in which to
#'     evaluate chunks
#' @param verbose if TRUE, may print some slightly unneccessary messages
#' @export
run_chunk <- function(chunk = NULL, file = NULL, dm = NULL, envir = .GlobalEnv, verbose = TRUE){
    if(is.null(file)) file = holy_get("source_file", dm = dm)
    cinfo <- chunks_info(file, all = TRUE)
    if(is.null(chunk)) chunk <- 1
    if(is.numeric(chunk)){
        N  <- nrow(cinfo)
        if((mer <- max(chunk)) > N){
            stop(paste0("[cess] there are ", N,
                        " chunks and you've supplied number up to ", mer, "."))
        }
    }
    if(is.character(chunk)){
        if(!all(chunk %in% cinfo$name)){
            cat("Available chunks in ", file, " are:\n",
                paste0(paste0(" * ", cinfo$name), collapse="\n"), sep = "")
            no <- which(!chunk %in% cinfo$name)
            cat("\n\nThe following chunks do not exists:\n",
                paste(paste(" * ", chunk[no]), collapse="\n"), sep = "")
            stop("[run_chunk] specified chunks does not completely match")
        }
        copy <- chunk
        for(k in seq_along(chunk)){
            copy[k] <- which(cinfo$name %in% chunk[k])
        }
        chunk  <- as.numeric(copy)
    }
    for(indx in chunk){ # indx = chunk[1]
        if(verbose) cat("Evaluating chunk ", indx, " ('", cinfo$name[indx], "'):\n", sep = "")
        eval(expr = parse(text = cinfo$code[indx]), envir = envir)
        if(verbose) cat("\n")
    }
    invisible(NULL)
}

##' @describeIn run_chunk \code{rblock} is an alias for \code{run_chunk}
##' @export
rblock <- run_chunk

##' @describeIn run_chunk \code{rblock_dm(chunk, ...)} is shorthand for
##'     \code{run_chunk(chunk, file = NULL, dm = TRUE, ...)}
##' @param ... arguments passed from shorthand functions to \code{run_chunk}
##' @export
rblock_dm <- function(chunk = NULL, ...){
    run_chunk(chunk = chunk, file = NULL, dm = TRUE, ...)
}

##' @describeIn run_chunk \code{cess} is an obsolete alias for \code{run_chunk}
##' @export
cess <- run_chunk

##' @describeIn run_chunk \code{cess_dm} is an obsolete alias for
##'     \code{rblock_dm}
##' @export
cess_dm <- rblock_dm
