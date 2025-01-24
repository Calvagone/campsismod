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
    original_model = "campsis_model",    # Original Campsis model
    replicated_parameters = "data.frame" # Replicated parameters
  )
)

#_______________________________________________________________________________
#----                           replicate                                   ----
#_______________________________________________________________________________

#' @rdname replicate
#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
#' @importFrom purrr map
#' @importFrom methods validObject
setMethod("replicate", signature = c("campsis_model", "integer"), definition = function(object, n) {
  
  # Validate original Campsis model before sampling parameter uncertainty
  methods::validObject(object, complete=TRUE)
  
  # Initialse a new replicated Campsis model
  retValue <- new("replicated_campsis_model", original_model=object)

  # Get variance-covariance matrix
  varcov <- object %>% getVarCov()

  # No variance-covariance matrix is detected
  if (varcov %>% length() == 0) {
    # No parameters sampled
    # The data frame is just filled in with all replicates ID
    retValue@replicated_parameters <- tibble::tibble(REPLICATE=seq_len(n), VALID=rep(TRUE, n))
    return(retValue)
  }
  
  # Variance-covariance matrix detected, collect the model parameters
  varcovNames <- colnames(varcov)
  originalParams <- identifyModelParametersFromVarcov(parameters=object@parameters)
  
  # Retrieve min, max and mean fields
  minMaxMean <- originalParams %>% purrr::map_df(.f=function(x) {
    return(tibble::tibble(min=ifelse(is.na(x@min), -Inf, x@min), max=ifelse(is.na(x@max), Inf, x@max), mean=x@value))
  }) %>% dplyr::mutate(name=varcovNames)

  # Sample parameters from the variance-covariance matrix
  tempTable <- sampleMore(n=n, mean=minMaxMean$mean, varcov=varcov, shift=0)
  table <- flagOutOfRangeParameterRows(table=tempTable, minMaxMean=minMaxMean)

  # Re-sample if more parameters are needed due to constraints
  while(sum(table$VALID) < n) {
    # Compute success rate
    successRate <- sum(table$VALID) / nrow(table)
    
    # Missing valid items
    missing <- n - sum(table$VALID)
    print(sprintf("%i valid replicate missing", missing))

    # Optimize next value
    nextN <- round(missing/successRate)
    if (nextN < 1) {
      nextN <- 1
    }
    print(sprintf("Generate %i rows", nextN))
    shift <- max(table$REPLICATE)
    tempTable <- sampleMore(n=nextN, mean=minMaxMean$mean, varcov=varcov, shift=shift)
    table <- dplyr::bind_rows(table, flagOutOfRangeParameterRows(table=tempTable, minMaxMean=minMaxMean))
  }
  
  # Discard extra rows
  lastValidIndex <- which(table$VALID)[n]
  table <- table[1:lastValidIndex,]
  
  # Numbering the valid rows only
  validIndexes <- which(table$VALID)
  invalidIndexes <- which(!table$VALID)
  table[validIndexes, "REPLICATE"] <- seq_len(n)
  table[invalidIndexes, "REPLICATE"] <- as.integer(NA)
  
  table <- table %>%
    dplyr::relocate(c("REPLICATE", "VALID"))
  
  retValue@replicated_parameters <- table
  
  return(retValue)
})

identifyModelParametersFromVarcov <- function(parameters) {
  varcov <- parameters@varcov
  varcovNames <- colnames(varcov)
  
  # Variance-covariance matrix detected, collect the model parameters
  retValue <- varcovNames %>% purrr::map(.f=function(.x){
    return(parameters %>% getByName(.x)) 
  }) %>% stats::setNames(varcovNames)
  
  return(retValue)
}

#' Sample more parameters.
#' 
#' @param n numbers of rows to sample
#' @param mean mean values to give to the multivariate normal distribution
#' @param varcov variance-covariance matrix
#' @param shift shift to apply to the replicate ID column
#' @importFrom dplyr mutate
#' @importFrom MASS mvrnorm
#' @importFrom tibble as_tibble
sampleMore <- function(n, mean, varcov, shift) {
  retValue <- MASS::mvrnorm(n=n, mu=mean, Sigma=varcov) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(REPLICATE=seq_len(n) + shift) %>%
    dplyr::mutate(VALID=TRUE)
  return(retValue)
}

#' Flag all parameter rows that have at least one parameter out of the specified range.
#' 
#' @param table a data frame returned by sampleMore
#' @param minMaxMean a data frame with min, max and mean values for each parameter
#' @importFrom dplyr mutate
#' @importFrom purrr map flatten_int
flagOutOfRangeParameterRows <- function(table, minMaxMean) {
  # For each column, check min and max
  # Return the indexes where at least on parameter is out of range
  indexesToDiscard <- colnames(table) %>% purrr::map(.f=function(.x) {
    values <- table[[.x]]
    limits <- minMaxMean %>% dplyr::filter(name==.x)
    return(which(values < limits$min | values > limits$max))
  }) %>% purrr::flatten_int() %>% unique() %>% base::sort()
  
  outOfRange <- rep(FALSE, nrow(table))
  outOfRange[indexesToDiscard] <- TRUE

  retValue <- table %>% 
    dplyr::mutate(VALID=!outOfRange)
  
  return(retValue)
}

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' @rdname export
setMethod("export", signature=c("replicated_campsis_model", "campsis_model"), definition=function(object, dest=CampsisModel(), index, ...) {
  # Get the index of the last replicate
  maxIndex <- object@replicated_parameters %>%
    dplyr::filter(.data$VALID) %>%
    dplyr::pull("REPLICATE") %>%
    max()
  
  # Check the user-given index is in range
  if (index < 1 || index > maxIndex) {
    stop(sprintf("Index must be in the range [1, %i]", maxIndex))
  }
  
  # Find row
  row <- object@replicated_parameters %>%
    dplyr::filter(.data$REPLICATE==index) %>%
    dplyr::select(-c("REPLICATE", "VALID"))
  
  retValue <- replaceOriginalParameters(model=object@original_model, row=row)
  return(retValue)
})

replaceOriginalParameters <- function(model, row) {

  paramNames <- names(row)
  paramValues <- as.numeric(row)
  originalParams <- identifyModelParametersFromVarcov(parameters=model@parameters)
  
  for (paramIndex in seq_along(paramNames)) {
    # pluck can be used because originalParams is a named list
    originalParam <- originalParams %>% purrr::pluck(paramNames[paramIndex])
    originalParam@value <- paramValues[paramIndex]
    model@parameters <- model@parameters %>% replace(originalParam)
  }
  
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
  
  # Reset varcov
  model@parameters@varcov <- matrix(numeric(0), nrow=0, ncol=0)
  
  return(model)
}
