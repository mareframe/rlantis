\name{bio_dem}
\alias{bio_dem}
\title{Generate demersel flag from Fishbase}
\usage{
bio_dem(biol_prm = NULL, fun_groups, output_file = "biol.prm")
}
\arguments{
  \item{biol_prm}{Either user-specified or
  \code{\link{biol_template}}}

  \item{fun_groups}{Functional group in data.frame format.}

  \item{output_file}{Name of the output file. Defaults to
  biol.prm}
}
\description{
This function generates the demersal flag based on Fishbase
data. If data are missing from Fishbase, then the flag is
assigned 999.
}
\details{
This function generates the demersal flags sections of the
biological parameter file of Atlantis based on Fishbase and
inserts them in biological parameter file.
}

