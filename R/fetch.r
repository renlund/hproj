#' @title load
#' @description load an object from 'calc' directory
#' @param ... (unquoted) names of objects
#' @export
fetch <- function(..., overwrite=TRUE, message=FALSE,
                 formats=c("rdata", "rdat"), env = .GlobalEnv){
    name <- as.character(eval(substitute(alist(...))))
    if(length(name) == 0) name <- NULL
    fetch_object(name, overwrite=overwrite, message=message,
          formats=formats, env = env)
}

#' @describeIn fetch the function that does the work
#' @param name character; the name of a variable. If missing, available objects will be listed
#' @param overwrite if variable already exists in global workspace, should it be overwritten?
#' @param message do you want an explanatory message?
#' @param formats formats to look for. Default \code{c('.rdata', '.rdat')}.
#' @param env the environment to load into. Defaults to the global enviroment.
#' @export
fetch_object <- function(name = NULL, overwrite=FALSE, message=FALSE,
                  formats=c("rdata", "rdat", "Rdata", "Rdat"), env = .GlobalEnv){
    types <- paste0("(\\.", paste0(formats,collapse=")|(\\."),")" )
    if(is.null(name)){
        get_meta_info(saved_only = TRUE)
        saved_info()
        return(invisible(NULL))
    }
    if(!is.character(name)){
        stop("'name' should be the names (as a character vector) of variables saved")
    }
    Lext <- list.files('calc', pattern=types, all.files=TRUE, ignore.case = TRUE)
    L <- gsub(types, "", Lext)
    if(length(Lext)==0) stop("there are no saves whatsoever")
    for(K in name){
        if(K %in% L){
            dummy <- if(exists(K, envir=env)) 1 else 0
            place <- which(L==K)
            if(dummy==1){
                if(overwrite) {
                    load(file=file.path('calc', Lext[place]), envir=env)
                    if(message) message(paste0("'", K, "' was overwritten."))
                } else {
                    if(message) message(paste0("'", K, "' exists and was not overwritten."))
                }
            } else {
                load(file=file.path('calc', Lext[place]), envir=env)
                if(message) message(paste0("'",K,"' dit not exist and was loaded."))
            }
        } else {
         warning(paste0("'", K, "' does not exists in directory 'calc'."))
        }
    }
}

##' @describeIn fetch a shorthand for loading all variables with variable names
##'     beginning with '.'
##' @export
fetch_dots <- function(){
    s <- hproj:::whatsaved()
    d <- s[grepl("^\\.", s)]
    fetch_object(name = d)
}
