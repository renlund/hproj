#' @title Decompose filename
#' @description Decompose a filename into name and extension
#' @param filename the vector of names you want decomposed
#' @return A dataframe with variables 'name' and 'extension'
#' @examples
#' some_names <- c("foo.txt", "foo.bar.r", ".emacs", "DESCRIPTION")
#' fileName(some_names)
#' @export
file_name <- function(filename){
   ext <- rep(NA_character_, length(filename))
   main <- ext
   for(k in seq_along(filename)){
      K <- filename[k]
      test <- regmatches(K, regexpr("\\.[A-Za-z0-9]*$", K))
      ext[k] <- if(length(test)>0) test else ""
      main[k] <- sub( paste0(ext[k], "$"), "", K)
   }
   R <- data.frame(
      name = main,
      extension = ext,
      stringsAsFactors = FALSE
   )
   rownames(R) <- filename
   R
}
