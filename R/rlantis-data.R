#' Helper tools for creating, manipulating, and viewing Atlantis input and output files
#' 
#' rlantis is a set of helper functions for creating, manipulating, and viewing Atlantis files from R. 
#' @name rlantis-package
#' @aliases rlantis
#' @docType package
#' @title Helper tools for creating, manipulating, and viewing Atlantis input and output files
#' @author \email{cddesjardins@@gmail.com}
#' @keywords package
NULL

#' @name required_init
#' @title Required initial variables
#' @description This data set adds the required initial conditions variables to the user-specified ones based on the \code{gen_init} and the functional group csv. These were extracted from the initial nc file from the SETas_model_New example.
#' @docType data
#' @usage required
#' @details The required data set was created by extracting the required tracer, phys, and epibenthos variables from \code{init_vmpa_setas_25032013.nc}. This is just raw text that was read in using the \code{readLines} function on the uncompressed nc file. To see this data in user readable format, see the example.  
#' @source SETas_model_New example data from Atlantis Wiki
#' @examples
#' cat(required, sep = "\n") 
#' @seealso \code{\link{gen_init}},\code{\link{dummy_hydro}}\code{\link{init_data}} 
NULL

#' @name biol_template
#' @title Biological parameters template file
#' @description This data set uses a skeleton biological parameters file and inserts data from the user and from Fishbase.
#' @docType data
#' @usage biol_template
#' @seealso \code{\link{bio_dem}}. \code{\link{bio_com}}
NULL