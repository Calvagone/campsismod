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
#' @slot max_chunk_size maximum number of rows to sample at once
#' @slot min_max logical, check for min/max values when sampling the parameters
#' @slot positive_definite logical, check for positive definiteness when sampling the OMEGA/SIGMA parameters 
#' @export
setClass(
  "auto_replication_settings",
  representation(
    wishart="logical",
    odf="integer",
    sdf="integer",
    quiet="logical",
    max_iterations="integer",
    max_chunk_size="integer",
    min_max="logical",
    positive_definite="logical"
  ),
  contains="replication_settings",
  prototype=prototype(wishart=FALSE, odf=as.integer(NA), sdf=as.integer(NA), quiet=as.logical(NA),
                      max_iterations=100L, max_chunk_size=1000L, min_max=TRUE, positive_definite=FALSE)
)

#'
#' Create auto replication settings.
#' 
#' @description
#' By default, all model parameters are sampled from a multivariate normal 
#' distribution, whose characteristics are specified by the variance-covariance matrix.
#' OMEGAs and SIGMAs can be sampled from scaled inverse chi-squared or Wishart distributions
#' by setting the \code{wishart} argument to \code{TRUE}. In that case, THETAs are still sampled
#' from a multivariate normal distribution, while OMEGAS and SIGMAs are sampled from
#' scaled inverse chi-squared (univariate OMEGA/SIGMA distribution) and Wishart (block of
#' OMEGAs/SIGMAs) distribution, respectively. When \code{wishart} is set to \code{TRUE}, the degrees
#' of freedom of the distribution must be specified, respectively, \code{odf} for the OMEGAs and
#' \code{sdf} for the SIGMAs.
#' 
#' @param wishart logical, sample OMEGAs and SIGMAs from scaled inverse chi-squared (univariate OMEGA distribution)
#'  or Wishart distribution (block of OMEGAs)
#' @param odf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the OMEGAs
#' @param sdf the degrees of freedom for the scaled inverse chi-squared/Wishart distribution with regards to the SIGMAs
#' @param minMax logical, check for min/max values when sampling the parameters, default is TRUE
#' @param positiveDefinite logical, check for positive definiteness when sampling the OMEGA/SIGMA parameters from the variance-covariance matrix (i.e. when \code{wishart=FALSE}), default is FALSE (requires extra time)
#' @param quiet logical, suppress info messages, default is NA. By default, messages will be printed out when the success rate of sampling the parameters is below 95\%.
#' @return replication settings
#' @export
AutoReplicationSettings <- function(wishart=FALSE, odf=NA, sdf=NA, minMax=TRUE, positiveDefinite=FALSE, quiet=NA) {
  return(new("auto_replication_settings", wishart=as.logical(wishart),
             odf=as.integer(odf), sdf=as.integer(sdf),
             min_max=as.logical(minMax), positive_definite=as.logical(positiveDefinite),
             quiet=as.logical(quiet)))
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
#' @details Use these settings to import custom replicated model parameters.
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

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

setMethod("show", signature=c("auto_replication_settings"), definition=function(object) {
  if (identical(object, AutoReplicationSettings())) {
    cat("Replication settings: default (auto)")    
  } else {
    cat(sprintf("Replication settings: wishart=%s, odf=%s, sdf=%s", as.character(object@wishart),
                as.character(object@odf), as.character(object@sdf)))
  }
  cat("\n")
})

setMethod("show", signature=c("manual_replication_settings"), definition=function(object) {
  cat("Replication settings: manual") 
  cat("\n")
})
