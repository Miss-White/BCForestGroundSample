% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/siteToolsSpeciesConvertor.R
\docType{methods}
\name{siteToolsSpeciesConvertor}
\alias{siteToolsSpeciesConvertor}
\alias{siteToolsSpeciesConvertor,character-method}
\title{Convert VRI species code to site tools species code}
\usage{
siteToolsSpeciesConvertor(species)

\S4method{siteToolsSpeciesConvertor}{character}(species)
}
\arguments{
\item{species}{character, Species code in VRI data sets.}
}
\value{
Converted species codes that can be recognized by site tools program. NA is the
        species that failed to be converted.
}
\description{
This function converts BC VRI species code to site tools species code. 
             The convertion is based on a hardcoded lookup table \code{spv_frd}.
}
\author{
Yong Luo
}
