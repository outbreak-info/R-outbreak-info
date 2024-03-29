% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCuratedLineages.R
\name{getCuratedLineages}
\alias{getCuratedLineages}
\title{Lookup curated lineages from outbreak.info}
\usage{
getCuratedLineages()
}
\value{
dataframe containing VOCs, VOIs, VUMs, and associated metadata.
}
\description{
Retrieve a curated list of Variants of Concern, Variants of Interest,
Variants Under Monitoring, and de-escalated variants maintained by the [outbreak.info team](https://outbreak.info/situation-reports)
}
\examples{
curated = getCuratedLineages()
# Pull out the curated lineages which are WHO-desginated
curated[!is.na(curated$who_name),]
# Pull out the Variants of Concern
curated[curated$variantType == "Variant of Concern",]
}
