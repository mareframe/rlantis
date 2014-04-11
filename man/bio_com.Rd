\name{bio_com}
\alias{bio_com}
\title{Generate biological parameter file for Atlantis using user-specified data and Fishbase}
\usage{
bio_com(biol_prm = NULL, fun_groups, output_file = "biol.prm")
}
\arguments{
  \item{biol_prm}{Either user-specified or
  \code{\link{biol_template}}}

  \item{fun_groups}{Functional group in data.frame format.}

  \item{output_file}{Name of the output file. Defaults to
  biol.prm}
}
\description{
This function generates comments at the top of the
biological parameter file. This adds no actual data just
comments.
}
\details{
This function generates sections of the biological
parameter file of Atlantis based on both user-specified
data and Fishbase
}

