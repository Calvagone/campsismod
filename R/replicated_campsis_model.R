#_______________________________________________________________________________
#----                      replicated_campsis_model class                   ----
#_______________________________________________________________________________

#' 
#' Replicated Campsis model class.
#' 
#' @export
setClass(
  "replicated_campsis_model",
  representation(
    original_model = "campsis_model",     # Original Campsis model
    replicated_parameters = "data.frame", # Replicated parameters
    settings = "replication_settings"     # Replication settings that were used
  )
)

#_______________________________________________________________________________
#----                           replicate                                   ----
#_______________________________________________________________________________

#' @rdname replicate
#' @importFrom methods validObject
#' @importFrom dplyr left_join
setMethod("replicate", signature = c("campsis_model", "integer", "replication_settings"), definition = function(object, n, settings) {
  
  # Validate original Campsis model before sampling parameter uncertainty
  methods::validObject(object, complete=TRUE)

  # Sort and standardise model first
  object <- object %>%
    campsismod::sort() %>%
    campsismod::standardise()
  
  # Initialize a new replicated Campsis model
  retValue <- new("replicated_campsis_model", original_model=object, settings=settings)
  
  # Disable OMEGAs and SIGMAs in variance-covariance if Wishart is used
  if (settings@wishart) {
    object <- object %>%
      disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  }

  # Get variance-covariance matrix
  varcov <- object %>% getVarCov()

  # No variance-covariance matrix is detected
  if (varcov %>% length() == 0) {
    # No parameters sampled
    # The data frame is just filled in with all replicates ID
    retValue@replicated_parameters <- tibble::tibble(REPLICATE=seq_len(n))
    return(retValue)
  }
  
  # Sample parameters in variance-covariance matrix from a multivariate normal distribution
  varcovParameters <- extractModelParametersFromNames(parameters=object@parameters, names=colnames(varcov))
  table <- sampleFromMultivariateNormalDistribution(parameters=varcovParameters, varcov=varcov, n=n)
  
  # Sample parameters (possibly OMEGA and SIGMA) from inverse chi-squared or Wishart distribution
  if (settings@wishart) {
    omegas <- object@parameters %>% select("omega")
    if (omegas %>% length() > 0) {
      sampledOmegas <- sampleFromInverseChiSquaredOrWishart(parameters=omegas, n=n, df=settings@nsub)
      table <- table %>%
        dplyr::left_join(sampledOmegas, by="REPLICATE")
    }
    sigmas <- object@parameters %>% select("sigma")
    if (sigmas %>% length() > 0) {
      sampledSigmas <- sampleFromInverseChiSquaredOrWishart(parameters=sigmas, n=n, df=settings@nobs)
      table <- table %>%
        dplyr::left_join(sampledSigmas, by="REPLICATE")
    }
  }
  
  retValue@replicated_parameters <- table
  return(retValue)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @param index index of the replicated Campsis model to export
#' @rdname export
setMethod("export", signature=c("replicated_campsis_model", "campsis_model"), definition=function(object, dest=CampsisModel(), index, ...) {
  # Get the index of the last replicate
  maxIndex <- object@replicated_parameters %>%
    dplyr::pull("REPLICATE") %>%
    max()
  
  # Check the user-given index is in range
  if (index < 1 || index > maxIndex) {
    stop(sprintf("Index must be in the range [1, %i]", maxIndex))
  }
  
  # Find row
  row <- object@replicated_parameters %>%
    dplyr::filter(.data$REPLICATE==index) %>%
    dplyr::select(-c("REPLICATE"))
  
  retValue <- updateParameters(model=object@original_model, row=row)
  return(retValue)
})

#' Update model parameters based on the parameters issued from the model replication.
#' 
#' @param model Campsis model
#' @param row a data frame row containing the new parameter values
#' @return updated Campsis model
#' @importFrom purrr pluck
#' @importFrom assertthat assert_that
#' 
updateParameters <- function(model, row) {
  assertthat::assert_that(nrow(row) == 1, msg="Only one row is expected.")
  paramNames <- names(row)
  paramValues <- as.numeric(row)
  originalParams <- extractModelParametersFromNames(parameters=model@parameters, names=paramNames)
  
  for (paramIndex in seq_along(paramNames)) {
    # pluck can be used because originalParams is a named list
    originalParam <- originalParams %>% purrr::pluck(paramNames[paramIndex])
    originalParam@value <- paramValues[paramIndex]
    model@parameters <- model@parameters %>% replace(originalParam)
  }
  
  # Update OMEGA's according that are same
  model <- updateOMEGAs(model)
  
  # Reset varcov
  model@parameters@varcov <- matrix(numeric(0), nrow=0, ncol=0)
  
  return(model)
}

#'
#' Update OMEGAs that are same. Same OMEGAs are written as follows:
#' OMEGA1 same is FALSE (first one, estimated)
#' OMEGA2 same is TRUE
#' OMEGA3 same is TRUE, etc.
#' OMEGA2 and OMEGA3 will take the same value as OMEGA1.
#' 
#' @param model Campsis model
#' @return updated Campsis model
#' @importFrom purrr accumulate
#' 
updateOMEGAs <- function(model) {
  # Still need to update the omegas 'SAME'
  # .x is the accumulated results or initial value (a 'parameters' object here)
  # .y next value in sequence (an omega here)
  omegas <- model@parameters %>% select("omega")
  if (omegas %>% length() > 1) {
    omegas_ <- Parameters()
    omegas_ <- omegas_ %>% add(omegas@list[[1]])
    
    returned_omega_ <- purrr::accumulate(.x=omegas@list[2:length(omegas@list)], .f=function(.x, .y) {
      lastOmega <- .x@list[[.x@list %>% length()]]
      currentOmega <- .y
      if (isTRUE(currentOmega@same)) {
        if (is.na(lastOmega@same)) {
          stop("Inconsistent same column. Slot 'same' of Previous OMEGA can't be NA.")
        }
        # Take value just above
        currentOmega@value <- lastOmega@value
      }
      
      # Accumulate here
      .x <- .x %>% add(currentOmega)
      
      return(.x)
    }, .init=omegas_)
    
    # Replace all previous omega's by new ones
    model@parameters <- model@parameters %>% replace(returned_omega_)
  }
  return(model)
}
