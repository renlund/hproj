% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latex-stuff.R
\name{latex_incl_graph}
\alias{latex_incl_graph}
\title{code to include graphics}
\usage{
latex_incl_graph(
  file,
  caption,
  label,
  caption.lot = caption,
  placement = "htb",
  cat = TRUE
)
}
\arguments{
\item{file}{path to file to include}

\item{caption}{caption}

\item{label}{label}

\item{caption.lot}{caption for list of tables}

\item{placement}{placement code, defaults to 'htb'}

\item{cat}{if TRUE (default) code will be cat:ed}
}
\description{
Generate standard LaTeX code for including graphics
}
