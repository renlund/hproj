#' @title document structure
#' @description Get information about the structure in a rnw file. Leave empty
#'     all arguments empty and hproj will try to figure out what you want.
#' @param file file of interest
#' @param dm logical; if TRUE divert to DM-file if 'file' is NULL
#' @return A print out
#' @export
doc_struc <- function(file = NULL, dm = NULL){
    ## if(is.null(file)) file = opts_hproj$get("source_file")
    if(is.null(file)) file = holy_get("source_file", dm = dm)
    sec <- sections_info(file = file)
    sec$type = factor("sec", levels = c("sec", "chu"))
    chu <- chunks_info(file = file)
    chu$type = factor("chu", levels = c("sec", "chu"))
    n_sec <- nrow(sec)
    n_chu <- nrow(chu)
    n <- n_sec + n_chu
    if(n == 0){
        message("[doc_struc] no sections of chunks found")
        return(invisible(NULL))
    }
    tmp <- merge(sec, chu, by = c("name", "row", "type"), all = T)
    both <- tmp[order(tmp$row), c("type", "name", "row", "sub")]
    indent <- rep(NA_integer_, n)
    dummy <- 0
    for(k in 1:n){
        dummy <- if(!is.na(both$sub[k])) both$sub[k] else dummy
        indent[k] <- dummy
    }
    width <- max(options("width")$width, 44) - 4
    set_ind <- "   "
    chu_pre <- " * "
    sec_mark  <- paste(rep("=", width), collapse = "")
    sub_mark <- paste0(set_ind,
                       paste(rep("-", width - nchar(set_ind)), collapse = ""))
    sub2_ext <- "- "
    short <- function(s, tol = width-10, extend = NULL,
                      discount = 2*nchar(set_ind)){
        s <- as.character(s)
        s <- if(nchar(s)>tol){
                 paste(substr(x = s, start = 1, stop = tol-3), "...")
             } else s
        n <- nchar(s)
        if(!is.null(extend)){
            extra <- width - n - 1
            mer <- substr(paste(rep(extend, extra) , collapse = ""),
                          start = 1, stop = extra - discount)
            s <- paste(s, mer)
        }
        s
    }
    r <- c("## document:", paste0("##     ", file),
           "## structure (* chunks; section ===; subsection ---; subsubsection - -):",
           "")
    for(k in 1:n){ # k = 6
        if(both$type[k] == "chu"){
            r <- c(r, paste0(paste(rep(set_ind, indent[k]), collapse=""),
                             chu_pre, both$name[k]))
        } else if(both$type[k] == "sec"){
            if(indent[k] == 2){
                r <- c(r, paste0(paste(rep(set_ind, 2), collapse=""),
                                 short(both$name[k], extend = sub2_ext)))
            } else if(indent[k] == 1){
                r <- c(r, paste0(paste(rep(set_ind, 1), collapse=""),
                                 short(both$name[k])), sub_mark)
            } else if(indent[k] == 0){
                r <- c(r, short(both$name[k]), sec_mark)
            }
        }
    }
    cat(r, sep = "\n")
}

#' @describeIn doc_struc shorthand for \code{doc_struc(dm = TRUE)}
#' @export
doc_struc_dm <- function() doc_struc(file = NULL, dm = TRUE)

#' @describeIn doc_struc get information about chunks in a rnw file
#' @param all if TRUE some extra chunk info will be given
#' @import knitr
#' @return A data frame with variables
#' \itemize{
#'    \item name: name of chunk (if any)
#'    \item row: start row
#'    \item n.row: number of rows
#'    \item stop: end line of chunk
#'    \item eval.arg: (if \code{all = TRUE}) if there is an argument specified for \code{eval}
#'    \item code:  (if \code{all = TRUE}) the code in the chunk
#' }
#' @export
chunks_info <- function(file = NULL, all = FALSE){
    ## if(is.null(file)) file = opts_hproj$get("source_file")
    if(is.null(file)) file = holy_get("source_file")
    if(file_name(file)$extension != ".rnw") warning("[chunks_info] this is not an rnw-file")
    X <- readLines(con = file)
    starts <- grep(pattern = "^ *<<.*>>=.*$", x = X)
    stopps <- grep(pattern = "^@ *$", x = X)
    n <- length(starts)
    if(length(stopps) != n) {
        stop(paste("[chunks_info] there seems to be", n, "chunk start(s) and",
                   length(stopps), "stop(s)."))
    }
    each_n <- if(n==1) stopps - starts else stopps[2:n] - starts[1:(n-1)]
    if(any(each_n<0)){
        stop(paste("[chunks_info] chunk starts and stops ",
                   "in wrong order somewhere"))
    }
    inits_raw <- X[starts]
    inits0 <- unlist(
        lapply(X = strsplit(x = inits_raw, split = "#", fixed = TRUE),
               FUN = function(x) x[1])
    )
    inits1 <- gsub(pattern = " |<<|>>=", replacement = "", x = inits0)
    inits2 <- gsub(pattern = "\"", replacement="'", x=inits1, fixed=TRUE)
    chunk_val <- strsplit(x = inits2, split = ",", fixed = TRUE)
    inits3 <- unlist(lapply(X = chunk_val, FUN = function(x) x[1]))
    inits4 <- ifelse(grepl(pattern = "=", x = inits3),
                     sprintf("<chunk %d: unnamed>", 1:n),
                     gsub(pattern = "'", replacement = "", x = inits3))
    eval_val <- unlist(
        lapply(X=chunk_val,
               FUN = function(x) x[grepl(pattern = "^eval=.*$", x = x)][1])
    )
    eval_arg <- gsub(pattern = "eval=", replacement = "", x = eval_val)
    gEt <- function(x){
        if(!is.na(x) & !x %in% c("FALSE", "TRUE")) {
            tryCatch(get(x, envir = .GlobalEnv), error = function(e) NA)
        } else {
            NA
        }
    }
    eval = ifelse(
        is.na(eval_arg),
        knitr::opts_chunk$get("eval"),
           ifelse(
               eval_arg == "TRUE",
               TRUE,
           ifelse(
               eval_arg == "FALSE",
               FALSE,
               unlist(lapply(X = eval_arg, FUN = gEt))
           )
           )
    )
    code_spann <- as.list(NULL)
    for(i in 1:n){
        code_spann[[i]] <- if(stopps[i] > starts[i] + 1) (starts[i]+1):(stopps[i]-1) else NA
    }
    R <- data.frame(
        "name" = inits4,
        "row" = starts,
        "n.row" = stopps-starts-1,
        "eval.arg" = eval_arg,
        "eval" = eval,
        "code" = unlist(
            lapply(X = 1:n,
                   FUN = function(i) paste(X[code_spann[[i]]], collapse = "\n"))
        ),
        stringsAsFactors = FALSE
    )
    if(!all){
        R$eval <- NULL
        R$code <- NULL
    }
    R
}

#' @describeIn doc_struc get information about sections in a rnw file
#' @return A data frame with variables
#' \itemize{
#'    \item name: name of section
#'    \item row: row number where section starts
#'    \item sub: number of 'sub', i.e. 0 for section, 1 for subsection,
#'    and 2 for subsubsection
#' }
#' @export
sections_info <- function(file = NULL){
    ## if(is.null(file)) file = opts_hproj$get("source_file")
    if(is.null(file)) file = holy_get("source_file")
    if(file_name(file)$extension != ".rnw"){
        warning("[sections_info] this is not an rnw-file")
    }
    X <- readLines(con = file)
    sec_hit   <- grep(pattern = "\\\\(sub){0,2}section\\{", x = X)
    n_hit <- length(sec_hit)
    sec_title_raw <- strsplit(x = X[sec_hit], split = "section\\{")
    if(max(unlist(lapply(X = sec_title_raw, FUN = length))) > 2){
        warning(paste0("[sections_info]\nThere should only be one ",
                       "\\((sub)^k)section (k=0,1,2) per line."))
    }
    sec_title1 <- lapply(X = sec_title_raw, FUN = function(x) x[[2]])
    sec_title2 <- unlist(
        lapply(X = sec_title1,
               FUN = function(x) unlist(strsplit(x = x[[1]], split = "}"))[1])
    )
    subs <- rep(NA_integer_, n_hit)
    look <- lapply(X = sec_title_raw,
                   FUN = function(x) unlist(strsplit(x = x[1], split = "\\\\")))
    for(k in seq_along(sec_hit)){ # k = 2
        if(any(grepl(pattern = "^sub$", x = look[[k]]))){
            subs[k] <- 1
            next
        }
        if(any(grepl(pattern = "^subsub$", x = look[[k]]))){
            subs[k] <- 2
            next
        }
        subs[k] <- 0
    }
    data.frame(
        "name" = sec_title2,
        "row" = sec_hit,
        "sub" = subs,
        stringsAsFactors = FALSE
    )
}
