## These function creates and handles overall project options
## and will hopefully be elaborated. The options are stored in
## an environment 'la_milieu'

## @title la_milieu
## @description an environment
la_milieu <- new.env(parent = getNamespace("hproj"))

## @title hproj_get
## @description this function retrieves the hproj settings
## @param name name of hproj setting variable
hproj_get <- function(name){
   if(length(ls(envir=la_milieu))==0) hproj_restore(check = FALSE)
   defaults <- get("defaults", envir=la_milieu)
   if (missing(name))
      defaults
   else {
      L <- as.list(NULL)
      for(k in name){
         L[[k]] <- defaults[[k]]
      }
      if(length(L) == 1) L[[1]] else if(length(L) == 0) NULL else L
   }
}

## @title hproj_set
## @description this function sets the hproj settings
## @param ... the names and values you want set
hproj_set <- function(..., check = TRUE){
   if(length(ls(envir=la_milieu))==0) hproj_restore()
   dots <- list(...)
   value <- get("value", la_milieu)
   for(k in names(dots)) if(!(k %in% value)) dots[[k]] <- NULL
   current <- hproj_get()
   for(k in names(dots)) current[[k]] <- dots[[k]]
   assign(x="defaults", value=current, envir=la_milieu)
   if(check) hproj_check()
   invisible(NULL)
}

## @title hproj_restore
## @description this function restores the default hproj settings
hproj_restore <- function(check = FALSE){
   assign(x="defaults", value=list(
      source_file = NULL,
      output_file = NULL,
      version = NULL,
      version_latex = NULL,
      dm_source_file = NULL,
      dm_output_file = NULL,
      dm_version = NULL,
      dm_version_latex = NULL,
      dm_active = NULL
   ), envir=la_milieu)
   assign(x="value", value = names(get(x="defaults", envir=la_milieu)), envir=la_milieu)
   if(check) hproj_check()
   invisible(NULL)
}

## @title hproj_check
## @description some checks of the hproj options
hproj_check <- function(){
    main_doc <- hproj_get("source_file")
    if(!is.null(main_doc)){
        if(!file.exists(main_doc)){
            foo <- paste0(rep("~", options("width")$width), collapse = "")
            warning(paste0("\n", foo,
                           "\nSource document set to ", main_doc,
                           " which I can't find in the current directory.\n",
                           "Perhaps use an .Rprofile with:\n",
                           "opts_hproj$set('source_file' = <correct-file>)\n",
                           foo))
        }
    } else {
        warning("no main document/source file set")
    }
    dm_doc <- hproj_get("dm_source_file")
    if(!is.null(dm_doc)){
        if(!file.exists(dm_doc)){
            foo <- paste0(rep("~", options("width")$width), collapse = "")
            warning(paste0("\n", foo,
                           "\nDM document set to ", dm_doc,
                           " which I can't find in the current directory.\n",
                           "Perhaps use an .Rprofile with:\n",
                           "opts_hproj$set('dm_source_file' = <correct-file>)\n",
                           foo))
        }
    }
    out_file <- hproj_get("output_file")
    dm_out_file <- hproj_get("dm_output_file")
    name <- file_name(main_doc)$name
    ext <- file_name(main_doc)$extension
    dm_name <- file_name(dm_doc)$name
    dm_ext <- file_name(dm_doc)$extension
    if(!all(c(ext, dm_ext) %in% c(".rnw", ".Rnw"))){
        stop("source document should be an rnw file")
    }
    if(is.null(out_file)) {
        hproj_set("output_file" = paste0(name, ".pdf"), check = FALSE)
    }
    if(is.null(dm_out_file) & !is.null(dm_doc)) {
        hproj_set("dm_output_file" = paste0(dm_name, ".pdf"), check = FALSE)
    }
    version <- hproj_get("version")
    version_latex <- hproj_get("latex_version")
    if(!is.null(version_latex)) if(version_latex == "") version_latex <- NULL
    if(is.null(version_latex)){
        vl <- if(is.null(version)) "" else  paste0("\\\\ ", version)
        hproj_set("version_latex" = vl, check = FALSE)
    }
    dm_version <- hproj_get("dm_version")
    dm_version_latex <- hproj_get("dm_latex_version")
    if(!is.null(dm_version_latex)) if(dm_version_latex == "") dm_version_latex <- NULL
    if(is.null(dm_version_latex) & !is.null(dm_doc)){
        vl <- if(is.null(dm_version)) "" else  paste0("\\\\ ", dm_version)
        hproj_set("dm_version_latex" = vl, check = FALSE)
    }
    dm_active <- hproj_get("dm_active")
    if(!is.null(dm_active) & !is.logical(dm_active)){
        warning("'dm_active' should be logical or NULL, reset to NULL")
        hproj_set("dm_active" = NULL, check = FALSE)
    }
    invisible(NULL)
}

#' @title hproj options
#' @description This list tries to mimic the behaviour of opts_chunk from knitr.
#'     Currently these values are maintained with the functions in (the list)
#'     \code{opts_hproj}: \itemize{
#'
#' \item{source_file - name derived from project name}
#'
#' \item{output_file - will be like source_file but appropriate file extension
#' (unless set manually)}
#'
#' \item{version a version number as character, e.g. "Version 1". This will
#'     appear on the LaTeX version of the report}
#'
#' \item{version_latex this is the string that will determine how the version
#'     number appears in the title (in LaTeX produced pdf:s) and will be added
#'     automatically}
#'
#' \item{all above options also exists in a version with prefix \code{dm_}}
#'
#' \item{dm_active, this will indicate what file to point to (when a
#' dm-file is present) for functions \code{cmp}, \code{doc_struc}, \code{look},
#' \code{send}. This can also be toggled with \code{dm_on()} and \code{dm_off()}}
#'
#' }
#' @export
opts_hproj <- list(
   "get" = hproj_get,
   "set" = hproj_set,
   "restore" = hproj_restore,
   "check" = hproj_check
)

##' @title activate DM
##' @description set hproj option 'dm_activate' to TRUE
##' @export
dm_on <- function(){
    dm_(value = TRUE)
}

##' @describeIn dm_on inactivate DM
##' @export
dm_off <- function(){
    dm_(value = FALSE)
}

##' @describeIn dm_on set activation status for DM
##' @param value value for 'dm_active'
##' @export
dm_ <- function(value = NULL){
    if(!is.null(value)){
        if(!is.logical(value)){
            stop("'value' should be logical (or NULL)")
        } else {
            if(is.na(value)) stop("'value' should not be NA")
        }
    }
    if(!is.null(value)){
        hproj_set("dm_active" = value, check = FALSE)
    } else {
        reg_val <- hproj_get("dm_active")
        cat("Current value for 'dm_active': ", reg_val, "\n")
        invisible(NULL)
    }
}

force_dm_status <- function(mess = NULL){
    dm <- hproj_get("dm_active")
    if(is.null(dm)){
        txt <- paste0(
            "   +----------------------------------------------+\n",
            "   | DM status has not been set for this session. |\n",
            "   | There are different ways to do this:         |\n",
            "   |  * opts_hproj$set(dm_active = <T or F>)      |\n",
            "   |  * dm_on() or dm_off()                       |\n",
            "   | Enter 'y' to set to TRUE (else FALSE)        |\n",
            "   +----------------------------------------------+\n"
        )
        s <- paste0(mess, "\n", txt)
        if(readline(prompt = s) == "y"){
            hproj_set("dm_active" = TRUE, check = FALSE)
        } else {
            hproj_set("dm_active" = FALSE, check = FALSE)
        }
    }
    hproj_get("dm_active")
}

holy_get <- function(s, dm = NULL){
    val <- c("source_file", "output_file", "version", "version_latex")
    if(!s %in% val){
        t <- paste0("s should be one of: ", paste(val, collapse = ", "), ".")
        stop(t)
    }
    if(is.null(dm)) dm <- force_dm_status()
    wanted <- paste0(if(dm) "dm_", s)
    hproj_get(wanted)
}
