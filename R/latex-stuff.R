##' @title latex code to include graphics
##' @description Generate standard LaTeX code for including graphics
##' @param file path to file to include
##' @param caption caption
##' @param label label
##' @param caption.lot caption for list of tables
##' @param placement placement code, defaults to 'htb'
##' @param cat if TRUE (default) code will be cat:ed
##' @export
latex_include_graphics <- function(file, caption, label, caption.lot = caption,
                                   placement = "htb", cat = TRUE){
    code <- paste0("\n\\begin{center}\\begin{figure}[", placement, "]\n",
                   "\\includegraphics{", file, "}\n",
                   "\\caption[", caption.lot, "]{", caption, "}\n",
                   "\\label{", label, "}\n",
                   "\\end{figure}\\end{center}\n")
    if(cat) cat(code) else code
}
