% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{block_tnt}
\alias{block_tnt}
\title{Tnt by block}
\usage{
block_tnt(self, code_tnt = "TNT")
}
\arguments{
\item{self}{an instance of the \code{UserData} R6 class containing metadata with plot_desc and moda_desc}

\item{code_tnt}{string to identify in TNT in the row of the dataframe, by default "TNT"#'}
}
\value{
a dataframe with plot_id associated with one (or more) tnt_id in the same block
}
\description{
This function associate each TNT (Non Treated Treatment) to one block.
}
\details{
This function use the plot description, that must have plot_id and block_code given for each plot.
Plot description is merged with modality description, assuming that each row that contains the string given by code_TNT ("TNT")
is identified as a plot with TNT treatment.
The results is stored in self$plot_tnt_association$block_association
}
