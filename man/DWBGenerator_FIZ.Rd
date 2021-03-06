% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DWBGenerator_FIZ.R
\docType{methods}
\name{DWBGenerator_FIZ}
\alias{DWBGenerator_FIZ}
\alias{DWBGenerator_FIZ,numeric,character,character-method}
\title{Collect decay, waste and breakage factor in FIZ routine}
\usage{
DWBGenerator_FIZ(DBHClass, tabNumber, riskGroup)

\S4method{DWBGenerator_FIZ}{numeric,character,character}(DBHClass, tabNumber,
  riskGroup)
}
\arguments{
\item{DBHClass}{numeric, Tree DBH class. Currently, The function is capatable to the DBH 
class with 5 cm interval. It can be derived using \code{\link{DBHClassifier}}.}

\item{tabNumber}{character, This character consist of 4 numbers. The first two number is the species number,
and the last is the series number.}

\item{riskGroup}{character, Specifies the risk group.  It must be one of \code{1}, \code{2} or \code{3}.
It can be derived from \code{\link{riskGroupDeriver}}.}
}
\value{
A list of \code{decay}, \code{waste} and \code{breakage} percentage.
}
\description{
This function is to collect the dead, waste and breakage factor from lookup table and 
             join them into tree data. Instead of reading the lookup table from disk, the function
             uses hard-coded the lookup table. This function is equivalent to second part of 
             dwb_fct.sas macro. For \code{BEC} routine, the decay, waste and breakage are collected using funtion
             \code{\link{DWBGenerator_BEC}}
}
\author{
Yong Luo
}
