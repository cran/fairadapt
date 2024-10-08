% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{compas}
\alias{compas}
\title{COMPAS dataset}
\format{
A data frame with 1,000 rows and 9 variables:
\describe{
\item{sex}{sex of the individual}
\item{age}{age, measured in years}
\item{race}{race, binary with values Non-White and White}
\item{juv_fel_count}{count of juvenile felonies}
\item{juv_misd_count}{count of juvenile misdemeanors}
\item{juv_other_count}{count of other juvenile offenses}
\item{priors_count}{count of prior offenses}
\item{c_charge_degree}{degree of charge, with two values, F (felony) and M (misdemeanor)}
\item{two_year_recid}{a logical TRUE/FALSE indicator of recidivism within two years after parole start}
}
}
\usage{
compas
}
\description{
A real dataset from Broward County, Florida. Contains information on individuals
released on parole, and whether they reoffended within two years.
}
\keyword{datasets}
