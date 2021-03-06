% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/callSiteTools.R
\docType{methods}
\name{ST_DefGICurve}
\alias{ST_DefGICurve}
\alias{ST_DefGICurve,numeric,character,character-method}
\alias{ST_DefGICurve,numeric,character,missing-method}
\title{Assign growth intercept curve reference by site index reference in sitetools}
\usage{
ST_DefGICurve(siteIndexRef, siteToolsDLLPath, sasExePath)

\S4method{ST_DefGICurve}{numeric,character,character}(siteIndexRef,
  siteToolsDLLPath, sasExePath)

\S4method{ST_DefGICurve}{numeric,character,missing}(siteIndexRef,
  siteToolsDLLPath)
}
\arguments{
\item{siteIndexRef}{numeric, Site index reference. It can be derived using \code{\link{ST_SpecRemap}} function.}

\item{siteToolsDLLPath}{character, Path to \code{SINDEX33.DLL}}

\item{sasExePath}{character, Path to sas executable, i.e., \code{sas.exe}. If missing, the function takes
\code{C:/Program Files/SASHome/x86/SASFoundation/9.3} as default. However, it will cause crush
if sas executable does not located in default folder.}
}
\value{
Growth intercept curve reference that can be recognized by sitetools
}
\description{
Assign growth intercept curve reference that can be recognized by site tools for a given site index reference.
}
\seealso{
\code{\link{ST_SpecRemap}}, \code{\link{ST_DefCurve}}, \code{\link{ST_HTAgeToSI}} and \code{\link{ST_YrsToBH}}
}
\author{
Yong Luo
}
