% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ageRangeClassifier.R
\docType{methods}
\name{ageRangeClassifier}
\alias{ageRangeClassifier}
\alias{ageRangeClassifier,numeric,character,character-method}
\title{Derive age range code}
\usage{
ageRangeClassifier(age, species, FIZ)

\S4method{ageRangeClassifier}{numeric,character,character}(age, species, FIZ)
}
\arguments{
\item{age}{numeric, Usually layer mean age. what does this mean?}

\item{species}{character, Tree basic species code, which is SP0 in VRI original data.}

\item{FIZ}{character, BC forest inventory zone.}
}
\value{
DWB age range code
}
\description{
This function derives age range code based on \code{age}, \code{species} and \code{FIZ}.
             The returned age range code includes: \code{1}-young(immature), \code{2}-older(immature),
             \code{3}-mature and \code{4}-overmature. This function is equivalent to age_rng.sas macro.
}
\author{
Yong Luo
}
