% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_preparation.R
\name{harmonize_column_types}
\alias{harmonize_column_types}
\title{Harmonize Column Types}
\usage{
harmonize_column_types(
  df,
  types_map = NULL,
  dictionary_path = system.file("extdata", "star_dictionary.csv", package = "startbox")
)
}
\arguments{
\item{df}{The dataframe to harmonize.}

\item{types_map}{(optional) predefined type mapping (otherwise it will load from a CSV).}

\item{dictionary_path}{(optional) path to the CSV dictionary file if types_map is not provided.}
}
\value{
A dataframe with harmonized column types.
}
\description{
Harmonizes the types of columns in a dataframe according to a type mapping.
}
