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
    original_model = "campsis_model",     # Original Campsis model (sorted and standardised)
    replicated_parameters = "data.frame" # Replicated parameters
  ),
  prototype = prototype(original_model=CampsisModel(),
                        replicated_parameters=tibble::tibble())
)

#_______________________________________________________________________________
#----                           replicate                                   ----
#_______________________________________________________________________________

#' @rdname replicate
#' @importFrom methods validObject
#' @importFrom dplyr left_join
setMethod("replicate", signature = c("campsis_model", "integer", "auto_replication_settings"), definition = function(object, n, settings) {
  
  # Validate original Campsis model before sampling parameter uncertainty
  methods::validObject(object, complete=TRUE)

  # Sort and standardise model first
  object <- object %>%
    sort() %>%
    standardise()
  
  # Initialize a new replicated Campsis model
  retValue <- new("replicated_campsis_model", original_model=object)
  
  # Disable OMEGAs and SIGMAs in variance-covariance if Wishart is used
  if (settings@wishart) {
    object <- object %>%
      disable(c("VARCOV_OMEGA", "VARCOV_SIGMA"))
  }

  # Get variance-covariance matrix
  varcov <- object %>% getVarCov()

  if (varcov %>% length() == 0) {
    # No variance-covariance matrix is detected
    table <- tibble::tibble(REPLICATE=seq_len(n))
  } else {
    # Sample parameters in variance-covariance matrix from a multivariate normal distribution
    table <- sampleFromMultivariateNormalDistribution(parameters=object@parameters, n=n, settings=settings)
  }
  
  # Sample parameters (possibly OMEGA and SIGMA) from inverse chi-squared or Wishart distribution
  if (settings@wishart) {
    omegas <- object@parameters %>% select("omega")
    if (omegas %>% length() > 0) {
      sampledOmegas <- sampleFromInverseChiSquaredOrWishart(parameters=omegas, n=n, settings=settings)
      table <- table %>%
        dplyr::left_join(sampledOmegas, by="REPLICATE")
    }
    sigmas <- object@parameters %>% select("sigma")
    if (sigmas %>% length() > 0) {
      sampledSigmas <- sampleFromInverseChiSquaredOrWishart(parameters=sigmas, n=n, settings=settings)
      table <- table %>%
        dplyr::left_join(sampledSigmas, by="REPLICATE")
    }
  }
  
  retValue@replicated_parameters <- table
  return(retValue)
})

#' @rdname replicate
#' @importFrom methods validObject
setMethod("replicate", signature = c("campsis_model", "integer", "manual_replication_settings"), definition = function(object, n, settings) {
  
  # Validate original Campsis model
  methods::validObject(object, complete=TRUE)
  
  # Sort and standardise model first
  object <- object %>%
    sort() %>%
    standardise()
  
  # In the future, do additional checks on the data coming from the manual replication settings
  data <- settings@replicated_parameters
  if (nrow(data) < n) {
    stop("The number of rows in the data frame must be greater than or equal to the number of replicates 'n'.")
  }
  
  # Initialize a new replicated Campsis model
  retValue <- new("replicated_campsis_model", original_model=object, replicated_parameters=data[1:n,])
  return(retValue)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @param index index of the replicated Campsis model to export
#' @rdname export
setMethod("export", signature=c("replicated_campsis_model", "campsis_model"), definition=function(object, dest=CampsisModel(), index, ...) {
  # If table is empty, return the original model
  if (nrow(object@replicated_parameters) == 0) {
    return(object@original_model)  
  }
  
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
#' @importFrom assertthat assert_that
#' @keywords internal
#' 
updateParameters <- function(model, row) {
  assertthat::assert_that(nrow(row) == 1, msg="Only one row is expected.")
  sampledParameterNames <- names(row)
  sampledParameterValues <- as.numeric(row)
  parameters <- model@parameters
  parameterNames <- parameters %>% getNames()

  for (index in seq_along(sampledParameterNames)) {
    sampledParameterName <- sampledParameterNames[index]
    sampledParameterValue <- sampledParameterValues[index]
    parameterIndex <- which(sampledParameterName==parameterNames)
    if (length(index)==0) {
      stop(sprintf("Parameter %s not found", sampledParameterName))
    }
    model@parameters@list[[parameterIndex]]@value <- sampledParameterValue
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
#' @importFrom purrr map_lgl
#' @keywords internal
#' 
updateOMEGAs <- function(model) {
  isOmega <- model@parameters@list %>%
    purrr::map_lgl(~is(.x, "omega"))
  
  omegaIndexes <- which(isOmega)
  omegas <- model@parameters@list[omegaIndexes]
  
  sameVector <- omegas %>%
    purrr::map_lgl(~.x@same)
  
  # 'SAME' vector only contains NA's, do nothing
  allNAOmegas <- all(is.na(sameVector))
  if (allNAOmegas) {
    return(model)
  }
  
  # Apply run length encoding on 'SAME' vector
  rleRes <- rle(sameVector)
  lengths <- rleRes$lengths
  values <- rleRes$values
  falseIndexes <- which(values==FALSE)
  trueIndexes <- which(values==TRUE)
  
  for (falseIndex in falseIndexes) {
    # Check FALSE is followed by TRUE
    if ((falseIndex + 1) %in% trueIndexes) {
      falseOmegaIndex <- sum(lengths[1:falseIndex])
      trueOmegaIndexes <- falseOmegaIndex + seq_len(lengths[falseIndex + 1])
      
      # Estimated value
      falseOmegaValue <- omegas[[falseOmegaIndex]]@value
      
      # Replace where 'same' is TRUE
      for (trueOmegaIndex in trueOmegaIndexes) {
        omegas[[trueOmegaIndex]]@value <- falseOmegaValue
      }
    }
  }
  
  # Replace OMEGA's in the model
  model@parameters@list[omegaIndexes] <- omegas
    
  return(model)
}

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

#' @importFrom ggplot2 aes ggplot geom_vline geom_density theme_bw facet_wrap
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_dbl
#' @importFrom assertthat assert_that
#' @importFrom tibble tibble
setMethod("show", signature=c("replicated_campsis_model"), definition=function(object) {
  
  model <- object@original_model
  parameters <- model@parameters
  parameterNames <- parameters %>% getNames()
  
  samplingData <- object@replicated_parameters %>%
    tidyr::pivot_longer(cols=-c("REPLICATE"), names_to="Parameter", values_to="Value")
  
  sampledParameterNames <- unique(samplingData$Parameter)
  
  assertthat::assert_that(all(sampledParameterNames %in% parameterNames),
                          msg="Some of the sampled parameters cannot be found in the model parameters.")
  
  sampledParameters <- parameters
  correspondingIndexes <- match(x=sampledParameterNames, table=parameterNames)
  sampledParameters@list <- sampledParameters@list[correspondingIndexes]

  parameterInfo <- tibble::tibble(Parameter=sampledParameters %>% getNames(),
                                  Value=sampledParameters@list %>% purrr::map_dbl(~.x@value)) %>%
    dplyr::mutate(Parameter=factor(.data$Parameter, levels=sampledParameterNames)) # Natural order
  
  samplingData <- samplingData %>%
    dplyr::mutate(Parameter=factor(.data$Parameter, levels=sampledParameterNames)) # Natural order
  
  plot <- ggplot2::ggplot(data=samplingData, mapping=ggplot2::aes(x=.data$Value, group=.data$Parameter)) +
    ggplot2::geom_vline(data=parameterInfo, mapping=ggplot2::aes(xintercept=.data$Value, group=.data$Parameter), linetype="dashed", colour="black") +
    ggplot2::geom_density() +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~Parameter, scales="free")
  print(plot)
})
