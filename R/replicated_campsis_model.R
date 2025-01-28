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
#' @importFrom stats setNames
#' @importFrom MASS mvrnorm
#' @importFrom purrr map
#' @importFrom methods validObject
setMethod("replicate", signature = c("campsis_model", "integer", "replication_settings"), definition = function(object, n, settings) {
  
  # Validate original Campsis model before sampling parameter uncertainty
  methods::validObject(object, complete=TRUE)

  # Sort and standardise model first
  object <- object %>%
    campsismod::sort() %>%
    campsismod::standardise()
  
  # Initialize a new replicated Campsis model
  retValue <- new("replicated_campsis_model", original_model=object, settings=settings)
  
  # Disable OMEGAs and SIGMAs in variance-covariance if wishart is used
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
  
  # Sample now the OMEGAs and SIGMAs
  if (settings@wishart) {
    sampledOmegas <- sampleOmegasSigmas(model=object, type="omega", n=n, df=settings@nsub)
    sampledSigmas <- sampleOmegasSigmas(model=object, type="sigma", n=n, df=settings@nobs)
    table <- table %>%
      dplyr::left_join(sampledOmegas, by="REPLICATE") %>%
      dplyr::left_join(sampledSigmas, by="REPLICATE")
  }
  
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

#' Sample the OMEGAs and SIGMAs from scaled inverse chi-square or wishart distributions.
#' 
#' @param model Campsis model
#' @param type type of parameter to sample (omega or sigma)
#' @param n number of rows to sample
#' @param df degree of freedom for the scaled inverse chi-square or wishart distribution
#' @return a data frame with the sampled parameters
#' @importFrom assertthat assert_that
#' @importFrom LaplacesDemon rinvchisq rinvwishart
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' 
sampleOmegasSigmas <- function(model, type="omega", n, df) {
  assertthat::assert_that(type %in% c("omega", "sigma"))
  
  parameters <- model@parameters %>%
    campsismod::select(type)

  blocks <- OmegaBlocks() %>%
    add(parameters)
  
  retValue <- tibble::tibble(REPLICATE=seq_len(n))
  
  for (block in blocks@list) {
    if (length(block) == 1) {
      onDiagElements <- block@on_diag_omegas
      assertthat::assert_that(length(onDiagElements)==1)
      elem <- onDiagElements@list[[1]]
      paramName <- elem %>% getName()
      retValue <- retValue %>%
        dplyr::mutate(!!paramName:=LaplacesDemon::rinvchisq(n=n, df=df, scale=elem@value)) 
    } else {
      params <- Parameters()
      params@list <- c(block@on_diag_omegas@list, block@off_diag_omegas@list)
      mat <- rxodeMatrix(params, type=type)

      mappingMat <- getMappingMatrix(parameters=params, type=type)
      allColnames <- as.vector(mappingMat)
      indexesToKeep <- which(allColnames != "")

      tmp <- seq_len(n) %>%
        purrr::map_df(~data.frame(t(as.vector(LaplacesDemon::rinvwishart(nu=df, S=mat*df))))[, indexesToKeep])
      colnames(tmp) <- allColnames[indexesToKeep]
      retValue <- dplyr::bind_cols(retValue, tmp)
    }
  }
  return(retValue)
}

#' Return a matrix filled in with OMEGA/SIGMA names to be mapped with the values.
#' Un-existing parameters are filled in with the empty string.
#' 
#' @param parameters subset of parameters
#' @param type type of parameter to map (omega or sigma)
#' @return a matrix with the names of the OMEGA/SIGMA parameters
#' 
getMappingMatrix <- function(parameters, type) {
  size <- parameters %>% keep(~isDiag(.x)) %>% length()
  retValue <- matrix(rep(0, size*size), nrow=size)
  for (i in 1:size) {
    for (j in 1:size) {
      if (type=="omega") {
        refParameter <- Omega(index=i, index2=j)
      } else {
        refParameter <- Sigma(index=i, index2=j)
      }
      parameter <- parameters %>% getByIndex(refParameter)
      if (length(parameter) == 0) {
        retValue[i, j] <- ""
      } else {
        retValue[i, j] <- parameter %>%
          getName()
      }
    }
  }
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
    limits <- minMaxMean %>% dplyr::filter(.data$name==.x)
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

#' @param index index of the replicated Campsis model to export
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
  
  retValue <- updateParameters(model=object@original_model, row=row)
  return(retValue)
})

#' Update model parameters based on the parameters issued from the model replication.
#' 
#' @param model Campsis model
#' @param row a data frame row containing the new parameter values
#' @return updated Campsis model
#' @importFrom purrr pluck
#' 
updateParameters <- function(model, row) {
  paramNames <- names(row)
  paramValues <- as.numeric(row)
  originalParams <- identifyModelParametersFromVarcov(parameters=model@parameters)
  
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
