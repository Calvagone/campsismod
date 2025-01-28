#_______________________________________________________________________________
#----                       replication_settings class                      ----
#_______________________________________________________________________________

#' 
#' Replication settings class.
#' 
#' @slot wishart logical, sample OMEGAs and SIGMAs from scaled inverse chi-squared or Wishart distributions
#' @slot nsub number of subjects
#' @slot nobs number of observations
#' @export
setClass(
  "replication_settings",
  representation(
    wishart="logical",
    nsub="integer",
    nobs="integer"
  )
)

#'
#' Create replication settings.
#' By default, all model parameters are sampled from a multivariate normal 
#' distribution, whose characteristics are specified by the variance-covariance matrix.
#' OMEGAs and SIGMAs can be sampled from scaled inverse chi-squared or Wishart distributions
#' by setting the 'wishart' argument to 'TRUE'. In that case, THETAs are still sampled
#' from a multivariate normal distribution; while OMEGAS and SIGMAs are sampled from
#' inverse chi-squared (univariate OMEGA/SIGMA distribution) and Wishart (block of OMEGA)
#' distribution, respectively. When 'wishart' is set to 'TRUE', the 'nsub' (number of 
#' subjects in modelling) and 'nobs' (total number of observations in modelling) arguments
#' must be specified. 
#' 
#' @param wishart logical, sample OMEGAs and SIGMAs from scaled inverse chi-squared (univariate OMEGA distribution)
#'  or Wishart distribution (block of OMEGAs)
#' @param nsub number of subjects
#' @param nobs number of observations
#' @return replication settings
#' @export
ReplicationSettings <- function(wishart=FALSE, nsub=NA, nobs=NA) {
  return(new("replication_settings", wishart=as.logical(wishart),
             nsub=as.integer(nsub), nobs=as.integer(nobs)))
}
