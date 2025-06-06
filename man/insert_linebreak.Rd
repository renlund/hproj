% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert_newline.R
\name{insert_linebreak}
\alias{insert_linebreak}
\title{add linebreaks}
\usage{
insert_linebreak(s, n, linebreak = "\\n", splitby = " ", max.it = 10000)
}
\arguments{
\item{s}{a character string}

\item{n}{a maximum number of characters}

\item{linebreak}{defaults to newline \code{"\n"}}

\item{splitby}{defaults to space \code{" "}}

\item{max.it}{maximum number of iterations of a while loop}
}
\value{
character vector
}
\description{
insert linebreaks to a character vector
}
\details{
Given a maximum number of character 'n', insert a linebreak before this number
is reached. N.B. Words longer than 'n' will however NOT be abbreviated.
}
\examples{
    s <- paste0("A very long and perhaps supercalifragilisticexpialidocious",
                " sentence in desparate need of linebreaks")
    cat(insert_linebreak(s, n = 20))
    cat(insert_linebreak(s, n = 30))
    cat(insert_linebreak(s, n = 7))
    cat(insert_linebreak(s, n = 100))
}
