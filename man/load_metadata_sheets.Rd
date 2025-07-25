% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inOut.R
\name{load_metadata_sheets}
\alias{load_metadata_sheets}
\title{Load Metadata Sheets from Excel Trial File}
\usage{
load_metadata_sheets(self)
}
\arguments{
\item{self}{An instance of the \code{UserData} R6 class containing the path to the Excel trial file.}
}
\value{
None. Updates \code{self$metadata} by assigning:
\itemize{
\item \code{"plot_desc"} from the "placette" sheet if present and non-empty.
\item \code{"moda_desc"} from the "modalite" sheet if present and non-empty.
}
}
\description{
Reads the sheets named \code{"placette"} and \code{"modalite"} from the Excel trial file stored in \code{self$excel_data_trial},
and stores the non-empty content into the metadata list of the \code{UserData} object.
}
\details{
\itemize{
\item Empty rows (fully NA or empty strings) are removed from each sheet before storage.
\item Messages are printed to confirm loading or alert if sheets are missing or empty.
}
}
