% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMutationsByLineage.R
\name{getMutationsByLineage}
\alias{getMutationsByLineage}
\title{Retrieve mutations by lineage}
\usage{
getMutationsByLineage(pangolin_lineage, frequency = 0.75, logInfo = TRUE)
}
\arguments{
\item{pangolin_lineage}{PANGO lineage name or vector}

\item{frequency}{a number between 0 and 1 specifying the frequency threshold above which to return mutations (default=0.8)}

\item{logInfo}{(optional) `Boolean` (T/F), T logs helper messages during API calls.}
}
\value{
dataframe
}
\description{
Retrieve all mutations in a specified lineage above a threshold
}
\examples{
p1 = getMutationsByLineage(pangolin_lineage="P.1", frequency=0.5)

}
