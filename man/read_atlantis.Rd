% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/read_atlantis.R
\name{read_atlantis}
\alias{read_atlantis}
\title{Read in Atlantis Files}
\usage{
read_atlantis(indir, outdir, bgm = TRUE, ssb = TRUE, fg = TRUE,
  bm = TRUE, boxbio = TRUE, vertsize = TRUE)
}
\arguments{
\item{indir}{The directory containing the Atlantis model input files}

\item{outdir}{The directory containing the Atlantis model output files}

\item{bgm}{Read in model BGM file (default: TRUE)}

\item{ssb}{Read in spawning stock biomass file (default: TRUE)}

\item{fg}{Read in functional group CSV (default: TRUE)}

\item{boxbio}{Read in the box biomass file (default: TRUE)}

\item{vertsize}{Read in the vertebrate size text file (default: TRUE)}
}
\description{
This function read in the appropriate input and output files for other rlantis routines
}
\details{
This function will sets up an rlantis object, at present just a list, which the other output functions call on.
}

