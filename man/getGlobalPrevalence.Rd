% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGlobalPrevalence.R
\name{getGlobalPrevalence}
\alias{getGlobalPrevalence}
\title{Retrieve global daily prevalence of a lineage}
\usage{
getGlobalPrevalence(pangolin_lineage, mutations = NULL, cumulative = NULL)
}
\arguments{
\item{pangolin_lineage}{PANGO lineage name}

\item{mutations}{(optional) a `vector` of mutation(s)}

\item{cumulative}{(optional) `Boolean` (T/F), T returns cumulative global prevalence since first day of detection}
}
\value{
dataframe
}
\description{
Retrieves the global daily prevalence of a PANGO lineage
}
\examples{
# B.1.1.7 lineage
b117 = getGlobalPrevalence(pangolin_lineage = "B.1.1.7", mutations = "S:E484K")
head(b117)

}
