% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rMallet.R
\name{import_file}
\alias{import_file}
\title{Run MALLET import-file function.}
\usage{
import_file(
  MALLET_PATH,
  MODEL_PATH,
  input_file,
  output_file,
  keep_sequences = TRUE,
  remove_stops = FALSE,
  extra_stopwords = NULL
)
}
\arguments{
\item{MALLET_PATH}{path to local mallet directory}

\item{MODEL_PATH}{path to an existing Topic Model}

\item{input_file}{path to a file for import}

\item{output_file}{path to output file}

\item{keep_sequences}{TRUE|FALSE}

\item{remove_stops}{TRUE|FALSE}

\item{extra_stopwords}{path to file with whitespace-separated words default = NULL}
}
\description{
load a single file into mallet instances (one per line)
}
