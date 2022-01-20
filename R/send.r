##' @title mark file for delivery
##' @description date and (possibly) version stamp a report and place in 'sent' folder
##' @param git logical; commit to git?
##' @param dm logical; send the dm? If NULL DM status will be checked
##' @export
send <- function(git = TRUE, dm = NULL){
    output_file <- holy_get("output_file", dm = dm)
    source_file <- holy_get("source_file", dm = dm)
    version <- holy_get("version", dm = dm)
    if(is.null(output_file)) stop("hproj options does not specify an output file")
    version <- if(is.null(version)){
                   ""
               } else {
                   paste0("--", gsub(" ", "-", version, fixed = TRUE))
               }
    send_function(output_file = output_file, version = version,
                  source_file = source_file, git = git)
}

##' @describeIn send shorthand for \code{send(dm = TRUE)}
##' @export
send_dm <- function(git = TRUE) send(git = git, dm = TRUE)

##' @describeIn send the function that does the work
##' @param output_file the report pdf
##' @param version version number
##' @param source_file the report rnw
##' @param git commit? (default \code{TRUE})
##' @export
send_function <- function(output_file, version, source_file, git){
    fn <- file_name(output_file)
    ext <- fn$extension
    name <- fn$name
    today <- gsub('-', '', Sys.Date())
    nameD <- paste0(name, version, '--', today)
    ## this part makes sure we do not overwrite files
    FnameDE <- function(nameD) paste0(nameD, ext)
    nameDE <- FnameDE(nameD)
    while(file.exists(file.path('sent', nameDE))){
        if(readline(paste0("A pdf with name '", nameDE,"'",
                          " already exists in 'sent'.\n",
                          " Overwrite it? (If so 'y') ")) == 'y') break
        suffix <- readline(paste0("\nOk - I will not overwrite it.",
                                  " Please provide a suffix. "))
        nameD <- paste0(nameD, suffix)
        nameDE <- FnameDE(nameD)
    }
    ## now commit files with default or given commit message
    if(git){
        if(is.null(source_file)) stop("hproj options does not specify a source file")
        default_message <- paste("Source for:", nameDE)
        invite_response <- paste0("The current commit message is '",
                                  default_message, "'.\n",
                                  "Want to change it? ('y' for yes, anything for 'no')")
        response <- readline(invite_response)
        if(response == 'y'){
            invite_response <- "Provide new commit message"
            commit_message <- readline(invite_response)
            if(commit_message == ""){
                warning(paste("We do not like empty commit messages.",
                              "Default message will be used"))
                commit_message <- default_message
            }
        } else {
            commit_message <- default_message
        }
        system(paste("git add", source_file))
        system(paste0("git commit -m \"", commit_message, "\""))
    }
    file.copy(from=output_file, to=file.path('sent', nameDE))
    if(!git) cat(paste0(" *-----------------------------------------*",
                        "  This is a good time to commit your files!",
                        " *-----------------------------------------*\n"))
    cat("\n\n                      'Some people update\n",
        "                     the version number\n",
        "                     already at this point\n",
        "                     in time.'\n",
        "                             ~ Unknown Sage\n")
    invisible(NULL)
}
