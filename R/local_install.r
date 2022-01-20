#' @title install a local package
#' @description Because I can NEVER remember what parameter choice I need
#' to make to make \code{install.packages} work.
#' @param path path to local package
#' @export
local_install <- function(path){
   utils::install.packages(
      pkgs=path,
      repos=NULL,
      type="source"
   )
}
