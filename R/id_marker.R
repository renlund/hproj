##' create 'id mark'
##'
##' Create a string that can capture user, time of creation, file, working
##' directory, etc.
##'
##' Currently the following marks are replaced
##'  + `<user>` by \code{Sys.getenv("USERNAME")}
##'  + `<wd>` by \code{getwd()}
##'  + `<time>` by \code{format(Sys.time(), format = "%Y-%m-%d %H:%M")}
##'  + `<file>` by 'file' (has to be supplied by the arguments)
##'  + `<path>` by \code{file.path(wd, file)}
##'  + `<chunk>` by 'chunk' (or, the actual chunk label if function called at
##' knitr runtime)
##'
##' @param x character; string where cartain marks of the form '<text>' will be
##'     replaced
##' @param ... things passed to \code{sub_key}
##' @param file character; if '<file>' is used in x, this is the replacement
##' @param chunk character; if '<chunk>' is used, this is the replacement. Note:
##'     if \code{id_marker} is being called at runtime (by knitr), then the
##'     actual label of the current chunk will be used.
##' @return A string of length 1
##' @export
id_marker <- function(x = "Created from <path> by <user> at <time>.",
                      ..., file = NULL, chunk = NULL){
    dots <- list(...) ## dots <- as.list(NULL)
    ## set default values
    if(is.null(file)) file <- "filename-placeholder.R"
    if(is.null(chunk)) chunk <- "chunk-placeholder"
    ## get run time values
    chunk.label <- knitr::opts_current$get("label")
    if(is.null(chunk.label)) chunk.label <- chunk
    wd <- getwd()
    user <- Sys.getenv("USERNAME")
    time <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
    path <- file.path(wd, file)
    key <- c("<user>" = user,
             "<file>" = file,
             "<wd>" = wd,
             "<path>" = path,
             "<chunk>" = chunk.label,
             "<time>" = time)
    do.call(what = "sub_key",
            args = c(list('x' = x, 'key' = key), dots))
}

##' @describeIn id_marker substitute by key; the names of key will be replaced
##'     by the values of key in x
##' @param g logical; use gsub instead of sub
##' @param fixed logical; passed to sub/gsub
##' @export
sub_key <- function(x, key, g = FALSE, fixed = TRUE){
    r <- x
    for(i in seq_along(key)){
        Args <- list(pattern = names(key)[i],
                     replacement = key[i],
                     x = r,
                     fixed = fixed)
        r <- do.call(what = if(g) gsub else sub,
                     args = Args)
    }
    r
}
