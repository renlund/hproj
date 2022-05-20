##' @title code to include graphics
##' @description Generate standard LaTeX code for including graphics
##' @param file path to file to include
##' @param caption caption
##' @param label label
##' @param caption.lot caption for list of tables
##' @param placement placement code, defaults to 'htb'
##' @param cat if TRUE (default) code will be cat:ed
##' @export
latex_incl_graph <- function(file, caption, label, caption.lot = caption,
                                   placement = "htb", cat = TRUE){
    code <- paste0("\n\\begin{figure}[", placement, "]\\begin{center}\n",
                   "\\includegraphics{", file, "}\n",
                   "\\caption[", caption.lot, "]{", caption, "}\n",
                   "\\label{", label, "}\n",
                   "\\end{center}\\end{figure}\n")
    if(cat) cat(code) else code
}

#' @title clean LaTeX files
#' @description Remove unnecessary LaTeX files. Prompt will be given before
#'     erasing.
#' @details Sometimes LaTeX leaves a mess. View source code to see what file
#'     endings are matched for removal.
#' @export
latex_clean <- function(){
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
