#' @title Run chunks
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
    ## if file is null then it is determined by dm:
    if(is.null(file) & is.null(dm)){
        ## dm <- opts_hproj$get("dm_active")
        ## if(is.null(dm)){
        ##     p <- paste0("'file' and 'dm' arguments are NULL and hproj options ",
        ##                 "'dm_active' is not set.\n",
        ##                 "If 'Y' (or 'y') this option will be set to TRUE.\n",
        ##                 "Are you working on the DM file?\n",
        ##                 "                                  ")
        ##     if(readline(prompt = p) %in% c("Y", "y")){
        ##         opts_hproj$set(dm_active = TRUE)
        ##     } else {
        ##         opts_hproj$set(dm_active = FALSE)
        ##     }
        ##     dm <- opts_hproj$get("dm_active")
        ## }
        s <- "DM status is unclear"
        dm <- force_dm_status(s)
    }
    if(is.null(file)){
        what_file <- paste0(if(dm) "dm_" else NULL, "source_file")
        file = opts_hproj$get(what_file)
    }
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

if(FALSE){
   file <- "C:/R/P_package/hproj/ignorera_detta/chunk_tester.rnw"
   cess(file=file)
   chunk = c("auto", "B", "2")
   cess(chunk, file)
   chunk = c("auto", "B", "2", "ERROR", "do not compute")
   cess(chunk, file)
}
