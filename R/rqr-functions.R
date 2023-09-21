#-# required properties
#-#
#-# requirements (class, length, etc) of the object
#-# @param x the object
#-# @param nm name of object
#-# @param class required class
#-# @param length required length
#-# @param na.ok are missing values ok?
properties <- function(x, nm = NULL, class = NULL, length = NULL, na.ok = NULL){
    if(is.null(nm)) nm <- paste0(as.character(substitute(x)), collapse = " ")
    if(!is.null(class)){
        s <- sprintf("\n%s fails to be in class {%s}", nm,
                     paste0(class, collapse = ", "))
        if(!any(class(x) %in% class)) stop(s)
    }
    if(!is.null(length)){
        s <- sprintf("\n%s fails to have length in {%s}", nm,
                     paste0(length, collapse = ", "))
        if(!length(x) %in% length) stop(s)
    }
    if(!is.null(na.ok)){
        if(!na.ok){
            s <- sprintf("\n%s contains missing values", nm)
            if(any(is.na(x))) stop(s)
        }
    }
    invisible(TRUE)
}

#-# required inclusion
#-#
#-# elements required to be part of the object
#-# @param x the object
#-# @param nm name of object
#-# @param include what must be included
inclusion <- function(x, nm = NULL, include){
    if(is.null(nm)) nm <- paste0(as.character(substitute(x)), collapse = " ")
    not_incl_index <- which(!include %in% x)
    if(length(not_incl_index) > 0){
        s <- sprintf("\nRequired elements {%s} does not exist in %s",
                     paste(include[not_incl_index], collapse = ", "), nm)
        stop(s)
    }
    invisible(TRUE)
}

#-# required avoidance
#-#
#-# elements required not to be part of the object
#-# @param x the object
#-# @param nm name of object
#-# @param avoid what must be avoided
avoidance <- function(x, nm = NULL, avoid){
    if(is.null(nm)) nm <- paste0(as.character(substitute(x)), collapse = " ")
    incl_index <- which(x %in% avoid)
    if(length(incl_index) > 0){
        s <- sprintf("\n%s contains {%s}; avoid {%s}.", nm,
                     paste0(x[incl_index], collapse = ", "),
                     paste0(avoid, collapse = ", "))
        stop(s)
    }
    invisible(TRUE)
}

#-# required belonging
#-#
#-# object required to be within a set of elements
#-# @param x the object
#-# @param nm name of object
#-# @param set the set that must contain x
one_of <- function(x, nm, set){
    if(is.null(nm)) nm <- paste0(as.character(substitute(x)), collapse = " ")
    if(length(x) != 1 | x %in% set){
        s <- sprintf("\n%s should be exactly one of {%s}",
                     nm, paste0(set, collapse= ", "))
    }
}

#-# required renaming
#-#
#-# suggested renaming in case of conflict
#-# @param x the object
#-# @param nm name of object
#-# @param avoid what to avoid
#-# @param prefix prefix to add incrementaly to element until conflict is
#-#     resolved
#-# @param suffix suffix to add incrementaly to element until conflict is
#-#     resolved
#-# @param limit how many increments to try before admitting defeat
#-# @param verbose supply helpful messages?
renaming_key <- function(x, nm = NULL, avoid, prefix = '.', suffix = NULL,
                         limit = 10, verbose = TRUE){
    if(is.null(nm)) nm <- paste0(as.character(substitute(x)), collapse = " ")
    x.org <- x
    rename <- which(x %in% avoid)
    if(length(rename) == 0){
        invisible(TRUE)
    } else {
        if(verbose){
            m <- sprintf("Elements {%s} are prohibited in %s",
                         paste0(x[rename], collapse = ", "), nm)
            message(m)
        }
        dummy <- 0
        while(length(rename) > 0 & dummy < limit){
            x[rename] <- paste0(prefix, x[rename], suffix)
            rename <- which(x %in% avoid)
            dummy <- dummy + 1
        }
        if(length(rename) > 0) {
            stop("\nSuggested renaming failed.")
        }
        if(verbose){
            i <- which(x.org != x)
            z <- paste(paste0(x.org[i], " -> ", x[i]), collapse = ", ")
            message("Suggested changes: ", z)
        }
        setNames(x, nm = x.org)
    }
}

#-# @describeIn renaming_key change one name at the time
rename <- function(x, avoid, prefix = ".", suffix = NULL, limit = 10){
    dummy <- 1
    while(x %in% avoid & dummy <= limit){
        x <- paste0(prefix, x, suffix)
        dummy <- dummy + 1
    }
    if(dummy > limit) stop("fail") else x
}
