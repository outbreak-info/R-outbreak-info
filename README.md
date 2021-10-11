
<!-- README.md is generated from README.Rmd. Please edit that file -->

# outbreakinfo <img src="man/figures/logo.png" align="right" />

#### R package for [outbreak.info](https://outbreak.info)

[outbreak.info](https://outbreak.info) is a platform to discover and
explore COVID-19 data and variants. Our Variant Reports allow
researchers to understand and track any emerging or known variant using
customizable visualizations, enabling near real-time genomic
surveillance, and our Epidemiology tools allow users to explore how
COVID-19 cases and deaths are changing across locations.

The **outbreakinfo** R package provides access to the underlying genomic
and epidemiology data on outbreak.info. This includes **SARS-CoV-2
variant prevalence** data calculated using the
[Bjorn](https://github.com/andersen-lab/bjorn/) package using data
provided by GISAID. We also standardize **COVID-19 case and death data**
from Johns Hopkins University and the New York Times and calculate
derived statistics.

## Installation

``` r
# Install development version from GitHub
# devtools::install_github("outbreak-info/R-outbreak-info/outbreakinfo")
```

## Getting Started

If you’re getting started using **outbreakinfo**, we recommend starting
with the tutorial
[vignettes](https://outbreak-info.github.io/R-outbreak-info/docs/articles/index.html).

Note that to access the genomics data (SARS-CoV-2 variant prevalences),
you will need to **create an account on
[GISAID](https://www.gisaid.org/registration/register/)** before being
able to access the data. It may take a few days for the registration to
become active. Before calling the genomics functions, you’ll need to
register your GISAID credentials:

``` r
outbreakinfo::authenticateUser()
```

## Related Projects

API access for outbreak.info’s [Research
Library](https://outbreak.info/resources), which provides metadata on
COVID-19 publications, pre-prints, clinical trials, datasets, protocols,
and more is available on our
[API](https://api.outbreak.info/try/resources). API access for the cases
and deaths data is also available on our
[API](https://api.outbreak.info/try/covid19). These API endpoints can be
accessed through the [httr](https://httr.r-lib.org/) R package or the
the Python [requests](https://docs.python-requests.org/en/latest/)
package.

-----

## Examples

### Genomics data

#### Lineage | Mutation Tracker

Provides access to the prevalence of a lineage, mutation(s), or lineage
with additional mutations, to access the data underlying the
[outbreak.info Variant
Tracker](https://outbreak.info/situation-reports?muts=S%3AP681R).

``` r
library(outbreakinfo)
#  Provide GISAID credentials
authenticateUser()
#> Please open this url in a browser and authenticate with your GISAID credentials.
#>
#> https://gpsapi-test.epicov.org/epi3/gps_authenticate/DMEVVOZKNPUJPXMQYRUNSJUTWCUCTSKBRCPGIRQLKHLGKUPMGYVQQIJJCZQIHWBCXFUQJYZVHPWFONZHHQUUNNGPPELHRZMVHDFUXSFWTCNRYUZPWMUTZROQMYGCCFFO
#>
#> Waiting for authentication... [press CTRL-C to abort]
#> Authenticated successfully!
# Get the prevalence of mutation P681R in the Spike protein in the United States over time.
P681R = getPrevalence(pangolin_lineage="B.1.617.2", mutations = c("S:P681R"), location = "Brazil")
#> Retrieving data...
#> Retrieving data...
knitr::kable(head(P681R))
```

| date       | total\_count | lineage\_count | total\_count\_rolling | lineage\_count\_rolling | proportion | proportion\_ci\_lower | proportion\_ci\_upper | query\_key                |
| :--------- | -----------: | -------------: | --------------------: | ----------------------: | ---------: | --------------------: | --------------------: | :------------------------ |
| 2021-04-26 |          324 |              2 |              180.8571 |               0.2857143 |  0.0015798 |               2.7e-06 |             0.0137634 | (B.1.617.2) AND (S:P681R) |
| 2021-04-27 |          258 |              0 |              185.8571 |               0.2857143 |  0.0015373 |               2.6e-06 |             0.0133964 | (B.1.617.2) AND (S:P681R) |
| 2021-04-28 |          115 |              0 |              183.2857 |               0.2857143 |  0.0015588 |               2.7e-06 |             0.0136142 | (B.1.617.2) AND (S:P681R) |
| 2021-04-29 |          112 |              1 |              166.1429 |               0.4285714 |  0.0025795 |               3.0e-06 |             0.0149958 | (B.1.617.2) AND (S:P681R) |
| 2021-04-30 |          133 |              0 |              161.5714 |               0.4285714 |  0.0026525 |               3.0e-06 |             0.0153627 | (B.1.617.2) AND (S:P681R) |
| 2021-05-01 |           60 |              0 |              164.4286 |               0.4285714 |  0.0026064 |               3.0e-06 |             0.0151770 | (B.1.617.2) AND (S:P681R) |

#### Lineage Comparison Tool

Provides access to the mutations per lineage, to access the data
underlying the [outbreak.info Lineage Comparison
Tool](https://outbreak.info/compare-lineages?pango=P.1&gene=ORF1a&gene=ORF1b&gene=S&gene=E&gene=ORF3a&gene=M&gene=ORF10&gene=N&gene=ORF8&gene=ORF7b&gene=ORF7a&gene=ORF6&threshold=80&dark=true).

``` r
library(outbreakinfo)
#  Provide GISAID credentials
authenticateUser()
#> Please open this url in a browser and authenticate with your GISAID credentials.
#>
#> https://gpsapi-test.epicov.org/epi3/gps_authenticate/WVMXBRMQFBTEITETBCHXIKVGFTZKMITZJSLDKHGHAKQGHFQZJLGMDIJHNPQGGIEEPSVZRCVMQQRTXGOORITXQJCVVJNIKVVHJUWDLOMFEELKABVXBMSUQOTIGERFKLJT
#>
#> Waiting for authentication... [press CTRL-C to abort]
#> Authenticated successfully!
# Get all mutations in Pango lineage P.1 at at least 80% prevalence in all P.1 sequences.
knitr::kable(getMutationsByLineage(pangolin_lineage="P.1", frequency=0.8))
#> Retrieving data...
```

| mutation           | mutation\_count | lineage\_count | lineage | gene  | ref\_aa            | alt\_aa      | codon\_num | codon\_end | type         | prevalence | change\_length\_nt | query\_key |
| :----------------- | --------------: | -------------: | :------ | :---- | :----------------- | :----------- | ---------: | :--------- | :----------- | ---------: | :----------------- | :--------- |
| s:d614g            |           71709 |          72479 | P.1     | S     | D                  | G            |        614 | None       | substitution |  0.9893762 | None               | P.1        |
| s:v1176f           |           71660 |          72479 | P.1     | S     | V                  | F            |       1176 | None       | substitution |  0.9887002 | None               | P.1        |
| orf1a:k1795q       |           71562 |          72479 | P.1     | ORF1a | K                  | Q            |       1795 | None       | substitution |  0.9873481 | None               | P.1        |
| orf1b:p314l        |           71527 |          72479 | P.1     | ORF1b | P                  | L            |        314 | None       | substitution |  0.9868652 | None               | P.1        |
| n:p80r             |           71480 |          72479 | P.1     | N     | P                  | R            |         80 | None       | substitution |  0.9862167 | None               | P.1        |
| orf1b:e1264d       |           71390 |          72479 | P.1     | ORF1b | E                  | D            |       1264 | None       | substitution |  0.9849750 | None               | P.1        |
| orf3a:s253p        |           71327 |          72479 | P.1     | ORF3a | S                  | P            |        253 | None       | substitution |  0.9841057 | None               | P.1        |
| s:h655y            |           71208 |          72479 | P.1     | S     | H                  | Y            |        655 | None       | substitution |  0.9824639 | None               | P.1        |
| orf8:e92k          |           70980 |          72479 | P.1     | ORF8  | E                  | K            |         92 | None       | substitution |  0.9793181 | None               | P.1        |
| orf1a:s1188l       |           70949 |          72479 | P.1     | ORF1a | S                  | L            |       1188 | None       | substitution |  0.9788904 | None               | P.1        |
| s:l18f             |           70740 |          72479 | P.1     | S     | L                  | F            |         18 | None       | substitution |  0.9760068 | None               | P.1        |
| s:p26s             |           70633 |          72479 | P.1     | S     | P                  | S            |         26 | None       | substitution |  0.9745306 | None               | P.1        |
| s:t20n             |           70368 |          72479 | P.1     | S     | T                  | N            |         20 | None       | substitution |  0.9708743 | None               | P.1        |
| orf1a:del3675/3677 |           70364 |          72479 | P.1     | ORF1a | ORF1A:DEL3675/3677 | DEL3675/3677 |       3675 | 3677       | deletion     |  0.9708191 | 9                  | P.1        |
| s:t1027i           |           70134 |          72479 | P.1     | S     | T                  | I            |       1027 | None       | substitution |  0.9676458 | None               | P.1        |
| s:d138y            |           69857 |          72479 | P.1     | S     | D                  | Y            |        138 | None       | substitution |  0.9638240 | None               | P.1        |
| s:n501y            |           69321 |          72479 | P.1     | S     | N                  | Y            |        501 | None       | substitution |  0.9564288 | None               | P.1        |
| s:e484k            |           69173 |          72479 | P.1     | S     | E                  | K            |        484 | None       | substitution |  0.9543868 | None               | P.1        |
| s:k417t            |           69137 |          72479 | P.1     | S     | K                  | T            |        417 | None       | substitution |  0.9538901 | None               | P.1        |
| n:g204r            |           68165 |          72479 | P.1     | N     | G                  | R            |        204 | None       | substitution |  0.9404793 | None               | P.1        |
| n:r203k            |           67782 |          72479 | P.1     | N     | R                  | K            |        203 | None       | substitution |  0.9351950 | None               | P.1        |
| s:r190s            |           67516 |          72479 | P.1     | S     | R                  | S            |        190 | None       | substitution |  0.9315250 | None               | P.1        |

``` r
# Plot the mutations as a heatmap
```

### Cases & Deaths

Replicates the daily confirmed cases visualization on
[outbreak.info](https://outbreak.info/epidemiology?location=USA%3BMEX&log=false&variable=confirmed_rolling&xVariable=date&fixedY=false&percapita=true)

``` r
# Plots the daily confirmed cases per capita for the United States and Mexico.
library(outbreakinfo)
plotCovid(locations = c("United States of America", "Mexico"), variable = "confirmed_rolling_per_100k")
```

![](man/figures/daily_cases-1.png)<!-- -->

For more examples, please view our
[vignettes](https://outbreak-info.github.io/R-outbreak-info/docs/articles/index.html).

-----

## Data Sources

### SARS-CoV-2 virus sequences

We would like to thank the GISAID Initiative and are grateful to all of
the data contributors, i.e.the Authors, the Originating laboratories
responsible for obtaining the specimens, and the Submitting laboratories
for generating the genetic sequence and metadata and sharing via the
GISAID Initiative, on which this research is based. GISAID data provided
on this website are subject to GISAID’s [Terms and
Conditions](https://www.gisaid.org/registration/terms-of-use/).

The GISAID Initiative promotes the rapid sharing of data from all
influenza viruses and the coronavirus causing COVID-19. This includes
genetic sequence and related clinical and epidemiological data
associated with human viruses, and geographical as well as
species-specific data associated with avian and other animal viruses, to
help researchers understand how viruses evolve and spread during
epidemics and pandemics.

GISAID does so by overcoming disincentive hurdles and restrictions,
which discourage or prevented sharing of virological data prior to
formal publication.

The Initiative ensures that open access to data in GISAID is provided
free-of-charge to all individuals that agreed to identify themselves and
agreed to uphold the GISAID sharing mechanism governed through its
[Database Access
Agreement](https://www.gisaid.org/registration/terms-of-use/).

### Cases & deaths

Confirmed cases, recovered cases, and deaths over time for countries
outside the United States, and provinces in Australia, Canada, and China
are provided by
[https://github.com/CSSEGISandData/COVID-19](Johns%20Hopkins%20University%20Center%20for%20Systems%20Science%20and%20Engineering).
See [data
FAQ](https://systems.jhu.edu/research/public-health/2019-ncov-map-faqs/).

Confirmed cases and deaths over time for the United States, U.S. States,
U.S. Metropolitan Areas, U.S. cities and U.S. counties are provided by
the [New York Times](https://github.com/nytimes/covid-19-data). Note
that “New York City” refers to the combined totals for New York, Kings,
Queens, Bronx and Richmond Counties; “Kansas City” refers to cases
within the Missouri portion of the Kansas City Metropolitan area and
values for Jackson, Cass, Clay, and Platte counties are the totals
excluding the KCMO data; cities like St. Louis that are administered
separately from their containing county are reported separately. See
other [geographic
exceptions](https://github.com/nytimes/covid-19-data#geographic-exceptions).
