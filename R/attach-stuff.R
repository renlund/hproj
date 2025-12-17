##' @title chunk options at runtime
##' @description A wrapper for \code{knitr::opts_current$get()}. Sometimes it is
##'     useful to have access to current chunk options at runtime, e.g. 'label'.
##' @param s the option you want
##' @export
knit_proc <- function(s){
    knitr::opts_current$get(s)
}

##' @describeIn knit_proc check if knitr is running
##' @export
knit_runtime <- function(){
    isTRUE(getOption('knitr.in.progress'))
}

##' @title attach csv
##' @description A function that (1) writes a csv to file, and (2) returns the
##'     caption with the appropriate LaTeX code to attach that file at the end.
##'     Currently this function will use \code{write.csv2}, since I find that it
##'     readable for everyone (so far).
##' @param cap the caption
##' @param tab the table to be written as csv (using \code{utils::write.cvs2})
##' @param file name of file; path and extension will be added automatically.
##' @param dir subfolder; default 'table'
##' @export
csv_attach <- function(cap, tab, file = NULL, dir = "table"){
    if(is.null(file) & knit_runtime()){
        file = file.path(dir, paste0(knit_proc("label"), ".csv"))
    } else if(is.null(file)){
        file = file.path(dir, "__PLACEHOLDER__.csv")
        s <- paste0("  |  if 'file' is NULL the knitr label will be used, but you\n",
                    "  |  are not knitting so '", file,"' is used")
        message(s)
    } else {
        if(!grepl("\\.csv$", file)){
            file = paste0(file, ".csv")
        }
        file = file.path(dir, file)
    }
    message("file written:", file)
    utils::write.csv2(x = tab, file = file, row.names = FALSE)
    if(opts_hproj$get("tab_attach")){
        paste0(cap, " \\attachfile{", file, "}")
    } else cap
}

##' @describeIn csv_attach xlsx_attach ... (add text here)
##' @param ... args passed to openxlsx::write.xlsx
##' @export
xlsx_attach <- function(cap, tab, file = NULL, dir = "table", ...){
    if(is.null(file) & knit_runtime()){
        file = file.path(dir, paste0(knit_proc("label"), ".xlsx"))
    } else if(is.null(file)){
        file = file.path(dir, "__PLACEHOLDER__.xlsx")
        s <- paste0("  |  if 'file' is NULL the knitr label will be used, but you\n",
                    "  |  are not knitting so '", file,"' is used")
        message(s)
    } else {
        if(!grepl("\\.xlsx$", file)){
            file = paste0(file, ".xlsx")
        }
        file = file.path(dir, file)
    }
    message("file written:", file)
    ## openxlsx::write.xlsx(tab, file = file, asTable = TRUE,
    ##                      colNames = TRUE)
    openxlsx::write.xlsx(tab, file = file, ...)
    if(opts_hproj$get("tab_attach")){
        paste0(cap, " \\attachfile{", file, "}")
    } else cap
}
## XK there should be a tab_attach ...

##' @title attach figure
##' @description A function that will return the caption with the appropriate
##'     LaTeX code for attaching, meant for use with knitr:s system for creating
##'     figures.
##' @param cap the caption
##' @param ext type of figure you want (default: 'pdf'). Note: in general, this
##'     output must be created by knitr, so should be one of the devices given
##'     as chunk option 'dev'.
##' @export
fig_attach <- function(cap, ext = NULL){
    if(is.null(ext)){
        ext <- opts_hproj$get("fig_dev")
    }
    if(knit_runtime()){
        lab <- knit_proc('label')
        path <- knit_proc('fig.path')
    } else {
        lab = "__PLACEHOLDER__"
        path = "figure"
        s <- paste0("  |  the file (path, name, extension) is determined at runtime\n",
                    "  |  and you are not knitting so '", lab, "' is used")
        message(s)
    }
    attach <- opts_hproj$get("fig_attach")
    if(!ext %in% knit_proc('dev')){
        if(attach & knit_runtime()) warning("the file extension may not exist")
    }
    n <- length(cap)
    CAP <- gsub("%", "%%", cap)
    if(attach){
        sprintf(paste0(CAP, " \\attachfile{", path, "/", lab,"-%s.", ext, "}"), 1:n)
    } else cap
}
