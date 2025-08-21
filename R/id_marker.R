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
##' @param chunk character; replaces '<chunk>'. Note:
##'     if \code{id_marker} is being called at runtime (by knitr), then the
##'     actual label of the current chunk will be used.
##' @param wd character (working directory), replaces '<wd>'
##' @param path character (file path), replaces '<path>'
##' @param user character (username), replaces '<user>'
##' @param time character (date+time), replacez '<time>'
##' @return A string of length 1
##' @export
id_marker <- function(
                x = "Created by <user> at <time> from chunk '<chunk>' in <path>.",
                ...,
                file = "PLACEHOLDER.Rnw",
                chunk = knitr::opts_current$get("label"),
                wd = getwd(),
                path = file.path(wd, file),
                user = Sys.getenv("USERNAME"),
                time = format(Sys.time(), format = "%Y-%m-%d %H:%M")
                ){
    dots <- list(...) ## dots <- as.list(NULL)
    ## ## set default values
    ## if(is.null(file)) file <- "filename-placeholder.R"
    if(is.null(chunk)) chunk <- "PLACEHOLDER"
    ## if(is.null(wd)) wd <- getwd()
    ## if(null(path)) path <- file.path(wd, file)
    ## if(is.null(user)) user <- Sys.getenv("USERNAME")
    ## if(is.null(time)) time <- format(Sys.time(), format = "%Y-%m-%d %H:%M")
    ## get run time values
    key <- c(
        "<file>" = file,
        "<chunk>" = chunk,
        "<wd>" = wd,
        "<path>" = path,
        "<user>" = user,
        "<time>" = time
    )
    do.call(what = "sub_key",
            args = c(list('x' = x, 'key' = key), dots))
}

##' @describeIn id_marker a standard marker
##' @param n1 integer; max length for first part of mark text
##' @param n2 integer; max length for second part of mark text
##' @param newline logical; linebreak between first and second part of mark text
##' @param linebreak character; code for linebreak
##' @param file character; name of file
##' @param chunk logical; want reference to a chunk? Derived from filename if NULL
##' @export
standard_mark <- function(n1 = 80, n2 = n1,
                          newline = TRUE,
                          linebreak = "\n",
                          file = "PLACEHOLDER.Rnw",
                          chunk = NULL){
    if(is.null(chunk)){
        ext <- file_name(file)$extension
        chunk <- if(ext %in% c(".Rnw", ".rnw")) TRUE else FALSE
    }
    r1 <- if(chunk){
              "Created by <user> at <time> from chunk '<chunk>' in"
          } else {
              "Created by <user> at <time> from"
          }
    t1 <- insert_linebreak(s = id_marker(x = r1, file = file),
                           n = n1,
                           linebreak = linebreak)
    r2 <- file.path(getwd(), file)
    t2 <- insert_linebreak(s = r2,
                           n = n2,
                           linebreak = linebreak,
                           splitby = .Platform$file.sep,
                           rm.split = FALSE)
    paste0(t1, if(newline) linebreak else " ", t2)
}

if(FALSE){
    standard_mark() |> cat("\n")
    standard_mark(file = "Foo.R") |> cat("\n")
    standard_mark(chunk = FALSE) |> cat("\n")
    standard_mark(50) |> cat("\n")
    standard_mark(50, 40, newline =TRUE) |> cat("\n")
    standard_mark(50, 40, newline =FALSE) |> cat("\n")
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
