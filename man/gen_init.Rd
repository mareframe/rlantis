\name{gen_init}
\alias{gen_init}
\title{Generate an Atlantis initial conditions file}
\usage{
gen_init(b, z, output_file = "init", timesteps = "UNLIMITED", set_groups,
  model_name = "model_name", bgm_file, start = NULL, timezone = "UTC",
  data = NULL, fill_value = NULL, gen_nc = FALSE, keep_cdf = TRUE)
}
\arguments{
  \item{b}{Number of boxes.}

  \item{z}{Number of vertical boxes (i.e. layers).}

  \item{output_file}{Name of the output_file. Defaults to
  init.}

  \item{timesteps}{Number of timesteps. Defaults to
  UNLIMITED.}

  \item{set_groups}{Path to the functional group csv file.}

  \item{model_name}{Name of the model. Defaults to
  model_name.}

  \item{bgm_file}{Name of the bgm file. Include the file
  extension!}

  \item{start}{Specify the start date. Defaults to date().}

  \item{timezone}{Set the timezone. Defaults to UTC.}

  \item{data}{Initial conditions data as a csv. Defaults to
  generate blank data. See Details.}

  \item{fill_value}{Matrix or data frame where the first
  column is the variable name and the second is the
  fillValue. Defaults to 0 or Beth's defaults.}

  \item{gen_nc}{Generate the nc binary? Defaults to FALSE
  and required netcdf-bin to be installed.}

  \item{keep_cdf}{Keep the readable cdf file? Defaults to
  TRUE.}
}
\description{
This function generates the initial conditions file for
Atlantis
}
\details{
This function generates the initial conditions file in the
Atlantis ncdf4 file format. This function can compress the
resultant cdf file if \code{gen_nc = TRUE} is set and can
clean up after itself if \code{keep_cdf = FALSE is set}. By
default, \code{gen_init} will generate empty data matrices
that will be filled by the specified \code{fill_value} if
it is provided. If it is not provided, the function
specifies 0 for all the user defined functional groups and
uses the fill values for the required variables from the
SETas_model_New example (namely,
\code{init_vmpa_setas_25032013.nc}). The function combines
the essential variables in the \code{required} data set
with those specified in the functional group csv. The data
CSV should have one column of data per variable. The length
of each column should be either \code{b}*1 for 2D variables
or \code{b}*\code{z} for 3D variables. The \code{init_data}
function can be used to help create this CSV.
}
\examples{
gen_init(b = 3, z = 2, set_group = "funGroup.csv", bgm_file = "model.bgm", gen_nc = TRUE)
}
\seealso{
\code{\link{required_init}},\code{\link{dummy_hydro}},\code{\link{init_data}}
}
\keyword{gen}

