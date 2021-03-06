% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMallet.R
\name{import_dir}
\alias{import_dir}
\title{Run MALLET import-dir function.}
\usage{
import_dir(
  MALLET_PATH,
  import_dir,
  output_dir,
  keep_sequences = TRUE,
  remove_stops = FALSE,
  stoplist_file = NULL
)
}
\arguments{
\item{MALLET_PATH}{path to local mallet directory}

\item{import_dir}{path to directory of files for importing}

\item{output_dir}{path to output directory}

\item{keep_sequences}{TRUE|FALSE}

\item{remove_stops}{TRUE|FALSE}

\item{stoplist_file}{path to file with newline-separated words default = NULL}
}
\description{
load the contents of a directory into mallet instances (one per file)
}
