#_______________________________________________________________________________
#----                       replication_settings class                      ----
#_______________________________________________________________________________

#' 
#' Replication settings class.
#' 
#' @slot wishart logical, sample OMEGAs and SIGMAs from inverse Chi-Square or Wishart distributions
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
#' 
#' @param wishart logical, sample OMEGAs and SIGMAs from inverse Chi-Square or Wishart distributions
#' @param nsub number of subjects
#' @param nobs number of observations
#' @return replication settings
#' @export
ReplicationSettings <- function(wishart=FALSE, nsub=NA, nobs=NA) {
  return(new("replication_settings", wishart=as.logical(wishart),
             nsub=as.integer(nsub), nobs=as.integer(nobs)))
}
