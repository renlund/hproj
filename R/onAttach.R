.onAttach  = function(libname, pkgname){
    s <- paste0(
        " +---------------------------------------------------------------+\n",
        " | The hproj package is meant for use within a directory created |\n",
        " | by hproj::new_project and in conjunction with knitr. Some     |\n",
        " | function relies on knitr-created folders (cache, figure) and  |\n",
        " | others on hproj-created folders (calc, sent, table). Some     |\n",
        " | parts are mainly controlled by options set in the hproj-      |\n",
        " | created .Rprofile and/or the first chunk of the .rnw-template.|\n",
        " +---------------------------------------------------------------+"
    )
    packageStartupMessage(s)
}
