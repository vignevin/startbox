% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inOut.R
\name{prepare_excel_model}
\alias{prepare_excel_model}
\title{Prepare Excel Trial File from Template}
\usage{
prepare_excel_model(self, directory = NULL, filename = NULL)
}
\arguments{
\item{self}{An instance of the \code{UserData} R6 class.}

\item{directory}{Character. Directory where the new file should be saved. Defaults to the current working directory if NULL.}

\item{filename}{Character. Name of the new Excel file (must end with \code{.xlsx}). If NULL, a default name based on the template is used.}
}
\value{
None. The method updates \code{self$excel_data_trial} with the path to the copied file.
}
\description{
Prepares the Excel trial file by copying the blank template if \code{self$excel_data_trial} is missing or invalid.
This ensures a valid Excel structure is available for inserting observation and metadata sheets.
}
\details{
\itemize{
\item If \code{self$excel_data_trial} is already set and the file exists, nothing is done.
\item Otherwise, the method copies \code{self$excel_model} to the specified location and updates the reference in the object.
\item A message is printed to indicate success or failure.
}
}
