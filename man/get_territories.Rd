% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_territories.R
\name{get_territories}
\alias{get_territories}
\title{Get the list of territories of the specified region}
\usage{
get_territories(id)
}
\arguments{
\item{id}{a region identifier from \strong{Region_Id} in \strong{get_regions}}
}
\description{
Returns a data frame with information about territories in the
specified region.
}
\details{
The data frame contains the following variables

\itemize{
\item \strong{Region_Id} - a region identifier
\item \strong{ATO_Raj} - a territory neighborhood
\item \strong{ATO_Rad} - a territory local council name
\item \strong{ATO_Name} - a territory name
\item \strong{ATO_id} - a territory id
}
}
