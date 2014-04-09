\name{dummy_hydro}
\alias{dummy_hydro}
\title{Generate dummy hydro data for Atlantis}
\usage{
dummy_hydro(b, z, t = 86400, model_name, bgm_file, hydro = TRUE,
  salt = TRUE, temp = TRUE, gen_nc = FALSE, keep_cdf = TRUE)
}
\arguments{
  \item{b}{Number of boxes}

  \item{z}{Number of vertical boxes (i.e. layers)}

  \item{t}{Time. Defaults to 86400.}

  \item{model_name}{Name of the model.}

  \item{bgm_file}{Name of the bgm file. Include the file
  extension!}

  \item{hydro}{Generate the flux data. Defaults to TRUE.}

  \item{salt}{Generate the dummy salinity data. Defaults to
  TRUE.}

  \item{temp}{Generate the dummy temperature data. Defaults
  to TRUE}

  \item{gen_nc}{Generate the nc binary? Defaults to FALSE
  and required netcdf-bin to be installed.}

  \item{keep_cdf}{Keep the readable cdf file? Defaults to
  TRUE.}
}
\description{
This function generates dummy flux, salinity, and
temperature files for Atlantis.
}
\details{
This function generates dummy hydrography data, namely
flux, salinity, and temperature data, in the Atlantis ncdf4
file format. This function can compress the resultant cdf
file if \code{gen_nc = TRUE} is set and can clean up after
itself if \code{keep_cdf = FALSE is set}. It can handle any
arbitrary number of boxes (set by \code{b}) and vertical
layers (set by \code{z}).
}
\examples{
dummy_hydro(b = 4, z, model_name = "dummy", bgm_file = "dummy.bgm", gen_nc = TRUE, keep_cdf = FALSE)
}
\seealso{
\code{\link{gen_init}},\code{\link{required_init}}
}
\keyword{gen}

