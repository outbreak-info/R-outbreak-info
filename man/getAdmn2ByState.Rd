% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAdmn2ByState.R
\name{getAdmn2ByState}
\alias{getAdmn2ByState}
\title{Retrieve COVID-19 data for counties in a state(s)}
\usage{
getAdmn2ByState(states, ...)
}
\arguments{
\item{states}{vector or list of state name(s)}
}
\value{
dataframe
}
\description{
Retrieve up-to-date COVID-19 data from outbreak.info for all counties in one or more states/provinces.
}
\examples{
getAdmn2ByState("California")

}
