#_______________________________________________________________________________
#----                     replication_settings interface                    ----
#_______________________________________________________________________________

#' 
#' Replication settings interface.
#' 
#' @export
setClass(
  "replication_settings",
  representation(
  )
)

#_______________________________________________________________________________
#----                       auto_replication_settings class                 ----
#_______________________________________________________________________________

#' 
#' Auto replication settings class.
#' 
#' @slot wishart logical, sample OMEGAs and SIGMAs from scaled inverse chi-squared or Wishart distributions
#' @slot odf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the OMEGAs
#' @slot sdf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the SIGMAs
#' @slot quiet logical, suppress info messages
#' @slot max_iterations number of iterations maximum to sample the parameters
#' @export
setClass(
  "auto_replication_settings",
  representation(
    wishart="logical",
    odf="integer",
    sdf="integer",
    quiet="logical",
    max_iterations="integer"
  ),
  contains="replication_settings",
  prototype=prototype(wishart=FALSE, odf=as.integer(NA), sdf=as.integer(NA), quiet=as.logical(NA), max_iterations=100L)
)

#'
#' Create auto replication settings.
#' By default, all model parameters are sampled from a multivariate normal 
#' distribution, whose characteristics are specified by the variance-covariance matrix.
#' OMEGAs and SIGMAs can be sampled from scaled inverse chi-squared or Wishart distributions
#' by setting the 'wishart' argument to 'TRUE'. In that case, THETAs are still sampled
#' from a multivariate normal distribution; while OMEGAS and SIGMAs are sampled from
#' scaled inverse chi-squared (univariate OMEGA/SIGMA distribution) and Wishart (block of OMEGA)
#' distribution, respectively. When 'wishart' is set to 'TRUE', the 'odf' (degrees of freedom 
#' concerning the OMEGAs) and 'sdf' (degrees of freedom concerning the SIGMAs) arguments must
#' be specified.
#' 
#' @param wishart logical, sample OMEGAs and SIGMAs from scaled inverse chi-squared (univariate OMEGA distribution)
#'  or Wishart distribution (block of OMEGAs)
#' @param odf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the OMEGAs
#' @param sdf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the SIGMAs
#' @param quiet logical, suppress info messages, default is NA. By default, messages will be printed out when the success rate of sampling the parameters is below 95\%.
#' @return replication settings
#' @export
AutoReplicationSettings <- function(wishart=FALSE, odf=NA, sdf=NA, quiet=NA) {
  return(new("auto_replication_settings", wishart=as.logical(wishart),
             odf=as.integer(odf), sdf=as.integer(sdf), quiet=as.logical(quiet)))
}

#_______________________________________________________________________________
#----                     manual_replication_settings class                 ----
#_______________________________________________________________________________

#' 
#' Manual replication settings class.
#' 
#' @slot replicated_parameters data frame, 1 row per replicate
#' @export
setClass(
  "manual_replication_settings",
  representation(
    replicated_parameters="data.frame"
  ),
  contains="replication_settings"
)

#'
#' Create manual replication settings.
#'
#' @param data data frame with 1 row per replicate, must contain a column named 'REPLICATE'
#'  with unique integers from 1 to nrow(data), other columns are model parameters to use.
#' @return replication settings
#' @export
ManualReplicationSettings <- function(data) {
  colnames <- colnames(data)
  assertthat::assert_that("REPLICATE" %in% colnames, msg="REPLICATE column is missing")
  assertthat::assert_that(all(data$REPLICATE %in% seq_len(nrow(data))), msg="REPLICATE column must contain unique integers from 1 to nrow(data)")
  return(new("manual_replication_settings", replicated_parameters=tibble::as_tibble(data)))
}
