#' @title Clean LaTeX files
#' @description Remove unnecessary LaTeX files
#' @details Sometimes LaTeX leaves a mess...
#' @export
clean <- function(){
   removables <- c(
      #".tex",
      "\\.toc",
      "-concordance\\.tex",
      "\\.log",
      "\\.brf",
      "\\.bbl",
      "\\.blg",
      "\\.lof",
      "\\.out",
      "\\.aux",
      "\\.lot",
      "\\.synctex.gz",
      "\\.nav",
      "\\.snm",
      "\\.vrb",
      "\\.fls",
      "\\.fdb_latexmk"
   )
   monster <- paste0("(", paste(paste0("(", removables, ")"), collapse="|"), ")$")
   files <- list.files(pattern = monster)
   cat("The following files will be removed\n")
   cat(NULL, paste0(paste0("   ", files), sep = "\n"))
   if(readline(prompt = "'y' to proceed? ") == "y"){
      if(all(file.remove(files))) cat("\n\nFiles erased\n")
   } else {
      cat("\nNo files erased\n")
   }
   invisible(NULL)
}
