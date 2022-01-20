##' remove all but
##'
##' like \code{rm} but specify what you want to keep
##' @title remove2
##' @param ... variables you want to keep
##' @param rmlist alternatively, the variables you want to keep as a character vector
##' @param envir environment in which to find the variables
##' @param verbose give helful messages?
##' @export
rm2 <- function(..., rmlist = NULL, envir = .GlobalEnv, verbose = TRUE){
    L <- if(is.null(rmlist)){
        as.character(unlist(eval(substitute(alist(...)))))
    } else rmlist
    lsenv <- ls(envir = envir)
    for(K in L){
        if(!K %in% lsenv){
            if(verbose){
                s <- paste0("No object '", K, "' in specified environment\n",
                            "You want to continue anyway? (y for yes,",
                            " anything else for no)   ")
                if(readline(s) != 'y'){
                    message("function aborted! nothing removed")
                    return(invisible(NULL))
                }
            }
            L <- setdiff(L, K)
        }
    }
    rm <- setdiff(lsenv, L)
    remove(list = rm, envir = envir)
    if(verbose){
        rm_t <- paste0(rm, collapse = ", ")
        kp_t <- paste0(L, collapse = ", ")
        if(kp_t == "") kp_t <- "(nothing)"
        message(paste0("Removing:\n  ", rm_t,
                       "\nand keeping:\n  ", kp_t))
    }
    invisible(NULL)
}


if(FALSE){
    foo <- function(...) as.character(unlist(eval(substitute(alist(...)))))
    foo(u)
    foo(x,y)
    x <- 1
    y <- "a"
    z <- 3:1
    rm2(u)
    rm2(rmlist='u')
    rm2(x,u)
    rm2(x,z)
    rm2(z, verbose = FALSE)
    rm2(rmlist = c('x', 'z'))
}
