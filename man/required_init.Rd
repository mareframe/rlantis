\docType{data}
\name{required_init}
\alias{required_init}
\title{Required initial variables}
\source{
SETas_model_New example data from Atlantis Wiki
}
\usage{
required
}
\description{
This data set adds the required initial conditions
variables to the user-specified ones based on the
\code{gen_init} and the functional group csv. These were
extracted from the initial nc file from the SETas_model_New
example.
}
\details{
The required data set was created by extracting the
required tracer, phys, and epibenthos variables from
\code{init_vmpa_setas_25032013.nc}. This is just raw text
that was read in using the \code{readLines} function on the
uncompressed nc file. To see this data in user readable
format, see the example.
}
\examples{
cat(required, sep = "\\n")
}
\seealso{
\code{\link{gen_init}},\code{\link{dummy_hydro}}\code{\link{init_data}}
}

