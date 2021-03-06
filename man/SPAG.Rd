% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPAG.R
\name{SPAG}
\alias{SPAG}
\title{Function calculating the coverage, distance and overlap components of the SPAG Index.}
\usage{
SPAG(companiesDF, shp, theoreticalSample = 1000, empiricalSample = 1000,
  numberOfSamples = 1, columnAreaName, companiesProjection, CRSProjection,
  totalOnly = FALSE, theoreticalDistributionType = "regular")
}
\arguments{
\item{companiesDF}{- data frame with information regarding the companies. The data frame needs four columns:
the longitude and latitude, the number of employees in each company and a category to which the company is assigned.
The columns should be provided in the aforementioned order.}

\item{shp}{- SpatialPolygonsDataFrame object obtained via loading a shapefile.}

\item{theoreticalSample}{- number of Companies used of the estimation of the average distance between companies assuming 
uniform distribution.}

\item{empiricalSample}{- number of companies used for the estimation of the average distance between companeis for which
the distance index is being calculated.}

\item{numberOfSamples}{-}
}
\description{
Function calculating the coverage, distance and overlap components of the SPAG Index.
}
\examples{
data(CompaniesPoland)
data(MapPoland)
spagIndex <- SPAG(CompaniesPoland,shp = MapPoland)
print(spagIndex)
plot(spagIndex)
plot(spagIndex, addCompanies = FALSE)
plot(spagIndex, category = "A")
plot(spagIndex, category = "A") + coord_map("orthographic")
plot(spagIndex, category = "B")
plot(spagIndex, category = "C")
}
