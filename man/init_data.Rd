\name{init_data}
\alias{init_data}
\title{Format Atlantis initial conditions data}
\usage{
init_data(layers, sed = 1, bgm_file, output_file = "init_data.csv")
}
\arguments{
  \item{layers}{A vector containing the boundaries of the
  vertical layers}

  \item{sed}{Number of sediment layers. Defaults to 1.}

  \item{bgm_file}{Name of the bgm file. Include the file
  extension!}

  \item{output_file}{Name of the output file. Defaults to
  init_data.csv}
}
\description{
This function creates the correct format for the
\code{gen_init} function for the Atlantis cdf.
}
\details{
At present this function generates nominal_dz, dz, and
numlayers
}

