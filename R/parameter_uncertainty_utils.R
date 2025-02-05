
#' Extract model parameters based on parameter names.
#' 
#' @param parameters all Campsis parameters
#' @param names names of the parameters to extract
#' @return subset of Campsis parameters corresponding to the provided names
#' @importFrom stats setNames
#' @keywords internal
#' 
extractModelParametersFromNames <- function(parameters, names) {
  paramsAsList <- names %>% purrr::map(.f=function(.x){
    parameter <- parameters %>% getByName(.x)
    if (is.null(parameter)) {
      stop(sprintf("Parameter %s not found", .x))
    }
    return(parameter)
  })
  retValue <- Parameters() %>%
    add(paramsAsList)
  
  return(retValue)
}

#' Min/max default values for the given parameter.
#' 
#' @param parameter Campsis parameter
#' @return a tibble with the min and max values
#' @importFrom tibble tibble
#' @keywords internal
minMaxDefault <- function(parameter) {
  min <- parameter@min
  max <- parameter@max
  minNA <- is.na(min)
  maxNA <- is.na(max)
  
  if (is(parameter, "theta")) {
    return(tibble::tibble(min=ifelse(minNA, -Inf, min), max=ifelse(maxNA, Inf, max)))
  } else if (is(parameter, "double_array_parameter")) {
    if (parameter %>% isDiag()) {
      return(tibble::tibble(min=ifelse(minNA, 0, min), max=ifelse(maxNA, Inf, max)))
    } else {
      return(tibble::tibble(min=ifelse(minNA, -Inf, min), max=ifelse(maxNA, Inf, max)))
    }
  }
  stop("Unknown parameter type")
}

#' Sample from a multivariate normal distribution.
#' 
#' @param parameters model parameters (all), already standardised
#' @param n number of rows to sample
#' @param settings replication settings
#' @return a data frame with the sampled parameters
#' @keywords internal
#' 
sampleFromMultivariateNormalDistribution <- function(parameters, n, settings) {
  varcov <- parameters@varcov
  varcovParameters <- extractModelParametersFromNames(parameters=parameters, names=colnames(varcov))
  
  # Retrieve variance-covariance matrix names
  varcovNames <- colnames(varcov)
  
  # Retrieve min, max
  minMax <- varcovParameters@list %>%
    purrr::map_df(~minMaxDefault(.x)) %>%
    dplyr::mutate(name=varcovNames)
  
  # Retrieve mean
  mean <- varcovParameters@list %>%
    purrr::map_dbl(~.x@value)
  
  # Parameters used to assess positive definiteness
  if (settings@wishart) {
    parameters <- NULL  
  } else {
    # Only keep OMEGA/SIGMA parameters
    parameters@list <- parameters@list %>%
      purrr::keep(~is(.x, "double_array_parameter"))
  }
  
  # Sample parameters from the variance-covariance matrix
  msg <- getSamplingMessageTemplate(what="parameters", from="variance-covariance matrix")
  table <- sampleGeneric(fun=sampleFromMultivariateNormalDistributionCore,
                         args=list(mean=mean, varcov=varcov), n=n, minMax=minMax, msg=msg, settings=settings, parameters=parameters)
  
  return(table)
}

#' Sample parameters from inverse scaled chi-squared or wishart distribution(s).
#' 
#' @param parameters subset of Campsis parameters (OMEGAs or SIGMAs)
#' @param n number of rows to sample
#' @param settings replication settings
#' @return a data frame with the sampled parameters
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' @keywords internal
#' 
sampleFromInverseChiSquaredOrWishart <- function(parameters, n, settings) {
  assertthat::assert_that(parameters %>% length() > 0)
  
  # Type of parameter
  type <- class(parameters@list[[1]]) %>% as.character()
  
  if (type=="omega") {
    df <- settings@odf
  } else if (type=="sigma") {
    df <- settings@sdf
  } else {
    stop("Should be either omega or sigma")
  }

  # Detect blocks
  blocks <- OmegaBlocks() %>%
    add(parameters)
  
  retValue <- tibble::tibble(REPLICATE=seq_len(n))
  
  # Iterate over blocks
  for (block in blocks@list) {
    if (isBlockFixed(block)) {
      next
    }
    if (length(block) == 1) {
      onDiagElements <- block@on_diag_omegas
      assertthat::assert_that(!hasOffDiagonalOmegas(block))
      elem <- onDiagElements@list[[1]]
      variable <- elem %>% getName()
      minMax <- minMaxDefault(elem) %>%
        dplyr::mutate(name=variable)
      msg <- getSamplingMessageTemplate(what=variable, from="scaled inverse chi-squared distribution")
      tmp <- sampleGeneric(fun=sampleFromInverseChiSquaredCore, args=list(df=df, scale=elem@value, variable=variable), n=n, minMax=minMax, msg=msg, settings=settings)
      retValue <- dplyr::bind_cols(retValue, tmp[, -1])
    } else {
      params <- Parameters()
      params@list <- c(block@on_diag_omegas@list, block@off_diag_omegas@list)
      mat <- rxodeMatrix(params, type=type)

      size <- params@list %>% purrr::keep(~isDiag(.x)) %>% length()
      mappingMat <- getMappingMatrix(parameters=params, type=type)
      allColnames <- as.vector(mappingMat)

      minMax <- params@list %>%
        purrr::map_df(~minMaxDefault(.x)) %>%
        dplyr::mutate(name=params@list %>% purrr::map_chr(~.x %>% getName()))
      
      msg <- getSamplingMessageTemplate(what=sprintf("OMEGA BLOCK(%i)", size), from="scaled inverse Wishart distribution")
      tmp <- sampleGeneric(fun=sampleFromInverseWishartCore, args=list(df=df, mat=mat, allColnames=allColnames), n=n, minMax=minMax, msg=msg, settings=settings)
      retValue <- dplyr::bind_cols(retValue, tmp[, -1])
    }
  }
  return(retValue)
}

#' Say if the block is fixed (i.e. all parameters are fixed).
#' 
#' @param block block of OMEGAs or SIGMAs
#' @return logical value
#' @importFrom purrr map_lgl
#' @keywords internal
isBlockFixed <- function(block) {
  list <- c(block@on_diag_omegas@list, block@off_diag_omegas@list)
  retValue <- list %>%
    purrr::map_lgl(~.x@fix)
  return(all(retValue))
}

#' Sample from multivariate normal distribution (core method).
#' 
#' @param n numbers of rows to sample
#' @param mean mean values to give to the multivariate normal distribution
#' @param varcov variance-covariance matrix
#' @return a data frame with the sampled parameters
#' @importFrom MASS mvrnorm
#' @importFrom tibble as_tibble
#' @keywords internal
sampleFromMultivariateNormalDistributionCore <- function(n, mean, varcov) {
  retValue <- MASS::mvrnorm(n=n, mu=mean, Sigma=varcov) %>%
    tibble::as_tibble()
  # If n=1, mvrnorm returns an unnamed vector
  if (n==1) {
    retValue <- data.frame(t(retValue)) %>%
      tibble::as_tibble()
    colnames(retValue) <- colnames(varcov)
  }
  return(retValue)
}

#' Sample from scaled inverse chi-squared distribution (core method).
#' 
#' @param n numbers of rows to sample
#' @param df degree of freedom
#' @param scale scale parameter
#' @param variable variable name
#' @return a data frame with the unique sampled parameter
#' @importFrom LaplacesDemon rinvchisq
#' @importFrom tibble tibble
#' @keywords internal
sampleFromInverseChiSquaredCore <- function(n, df, scale, variable) {
  table <- tibble::tibble(!!variable:=LaplacesDemon::rinvchisq(n=n, df=df, scale=scale))
  return(table)
}

#' Sample from scaled inverse Wishart distribution (core method).
#' 
#' @param n numbers of rows to sample
#' @param df degree of freedom
#' @param mat scaling matrix
#' @param allColnames all column names as they are going to appear when as.vector is called
#' @return a data frame with the sampled parameters
#' @importFrom LaplacesDemon rinvwishart
#' @keywords internal
sampleFromInverseWishartCore <- function(n, df, mat, allColnames) {
  indexesToKeep <- which(allColnames != "") # See getMappingMatrix method
  table <- seq_len(n) %>%
    purrr::map_df(~data.frame(t(as.vector(LaplacesDemon::rinvwishart(nu=df, S=mat*df))))[, indexesToKeep])
  colnames(table) <- allColnames[indexesToKeep]
  return(table)
}

#' Generic function for parameter sampling according to the minimum and maximum values.
#' This function will sample parameters a first time and check if some parameters are out of range.
#' Based on the success rate, it will sample more parameters to reach the desired number of rows.
#' 
#' @param fun function to call to sample parameters
#' @param args arguments to pass to the function
#' @param n number of rows to sample
#' @param minMax a data frame with min, max values for each parameter
#' @param msg message template
#' @param settings replication settings
#' @param parameters double array parameters to check for positive definiteness
#' @return tibble with the sampled parameters (1 parameter per column + REPLICATE column)
#' @keywords internal
sampleGeneric <- function(fun, args, n, minMax, msg, settings, parameters=NULL) {
  # First call to method
  tempTable <- do.call(what=fun, args=args %>% append(list(n=n))) %>%
    dplyr::mutate(REPLICATE=seq_len(n)) %>%
    dplyr::mutate(VALID=NA)
  table <- flagSampledParameterRows(table=tempTable, minMax=minMax,
                                       settings=settings, parameters=parameters)
  
  # Re-sample if more parameters are needed due to constraints
  iterations <- 0
  
  while(sum(table$VALID) < n) {
    # Compute success rate
    successRate <- sum(table$VALID) / nrow(table)
    
    # Missing valid items
    missing <- n - sum(table$VALID)
    # print(sprintf("%i valid replicate missing", missing))
    
    # Optimize next value
    nextN <- round(missing/successRate)
    if (nextN < 1) {
      nextN <- 1 # At least 1 sample required
    }
    if (is.infinite(nextN) || nextN > settings@max_chunk_size) {
      nextN <- settings@max_chunk_size # At most <MAX_CHUNK_SIZE> samples
    }
    
    # Increment and check
    iterations <- iterations + 1
    if (iterations > settings@max_iterations) {
      stop("Too many iterations, please check your min and max constraints.")
    }
    
    # print(sprintf("Generate %i rows", nextN))
    shift <- max(table$REPLICATE)
    tempTable <- do.call(what=fun, args=args %>% append(list(n=nextN)))  %>%
      dplyr::mutate(REPLICATE=seq_len(nextN) + shift) %>%
      dplyr::mutate(VALID=NA)
    table <- dplyr::bind_rows(table, flagSampledParameterRows(table=tempTable, minMax=minMax,
                                                                 settings=settings, parameters=parameters))
  }
  
  # Discard extra rows
  lastValidIndex <- which(table$VALID)[n]
  table <- table[1:lastValidIndex,]
  
  # Computing the success rate
  successRate <- sum(table$VALID) / nrow(table)
  
  # Show message logic
  quiet <- settings@quiet
  if (is.na(quiet)) {
    if (successRate < 0.95) {
      showMessage <- TRUE
    } else {
      showMessage <- FALSE
    }
  } else {
    showMessage <- !quiet
  }
  if (showMessage) {
    cat(sprintf(msg, successRate*100), "\n")
  }
  
  # Clean table before returning it
  table <- table %>%
    dplyr::relocate(c("REPLICATE")) %>%
    dplyr::filter(.data$VALID) %>%
    dplyr::select(-c("VALID")) %>%
    dplyr::mutate(REPLICATE=seq_len(n))
  
  return(table)
}

getSamplingMessageTemplate <- function(what, from) {
  return(sprintf("Success rate when sampling %s from %s: %%.1f%%%%", what, from))
}

#' Return a matrix filled in with OMEGA/SIGMA names to be mapped with the values.
#' Nonexistent parameters are filled in with the empty string.
#' 
#' @param parameters subset of parameters
#' @param type type of parameter to map (omega or sigma)
#' @return a matrix with the names of the OMEGA/SIGMA parameters
#' @keywords internal
#' 
getMappingMatrix <- function(parameters, type) {
  size <- parameters@list %>% purrr::keep(~isDiag(.x)) %>% length()
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

#' Flag all parameter rows that have at least one parameter out of the specified range
#' or that have a non positive definite OMEGA or SIGMA matrix.
#' 
#' @param table a data frame returned by \code{sampleMore}
#' @param minMax a data frame with min, max values for each parameter
#' @param settings replication settings
#' @param parameters double array parameters to check for positive definiteness
#' @importFrom dplyr bind_rows filter mutate
#' @importFrom purrr map flatten_int
#' @keywords internal
flagSampledParameterRows <- function(table, minMax, settings, parameters) {
  parameterNames <- colnames(table)
  parameterNames <- parameterNames[!parameterNames %in% c("REPLICATE", "VALID")]
  
  alreadyChecked <- table %>%
    dplyr::filter(!is.na(.data$VALID))
  
  toBeChecked <- table %>%
    dplyr::filter(is.na(.data$VALID)) %>%
    dplyr::mutate(VALID=TRUE) # Default
  
  if (settings@min_max) {
    # For each column, check min and max
    # Return the indexes where at least on parameter is out of range
    indexesToDiscard <- parameterNames %>% purrr::map(.f=function(.x) {
      values <- toBeChecked[[.x]]
      limits <- minMax %>% dplyr::filter(.data$name==.x)
      assertthat::assert_that(nrow(limits) == 1)
      return(which(values < limits$min | values > limits$max))
    }) %>% purrr::flatten_int() %>% unique() %>% base::sort()
    
    outOfRange <- rep(FALSE, nrow(toBeChecked))
    outOfRange[indexesToDiscard] <- TRUE
    
    toBeChecked <- toBeChecked %>% 
      dplyr::mutate(VALID=!outOfRange)
  }

  if (settings@positive_definite && !is.null(parameters)) {
    # before <- sum(toBeChecked$VALID)
    # print(sprintf("Before: %i rows", before))
          
    toBeChecked <- toBeChecked %>%
      dplyr::left_join(checkMatrixIsPositiveDefinite(table=toBeChecked, parameters=parameters), by="REPLICATE")
    
    toBeChecked <- toBeChecked %>%
      dplyr::mutate(VALID=.data$VALID & .data$POSITIVE_DEFINITE) %>%
      dplyr::select(-c("POSITIVE_DEFINITE"))
    
    # after <- sum(toBeChecked$VALID)
    # print(sprintf("After: %i rows", after))
  }
  
  retValue <- dplyr::bind_rows(alreadyChecked, toBeChecked)
  
  return(retValue)
}

#' Check OMEGA/SIGMA matrix for positive definiteness.
#' 
#' @param table data frame with the sampled parameters to check
#' @param parameters double array parameters to check for positive definiteness
#' @importFrom dplyr across group_split select starts_with
#' @importFrom purrr map_df flatten_int
#' @importFrom tibble tibble
#' @keywords internal
checkMatrixIsPositiveDefinite <- function(table, parameters) {
  # Remove THETA_ columns
  table <- table %>%
    dplyr::select(-dplyr::starts_with("THETA_"))
  
  retValue <- table %>%
    dplyr::group_split(dplyr::across("REPLICATE")) %>%
    purrr::map_df(.f=function(row) {
      replicate <- row$REPLICATE
      valid <- row$VALID
      if (isFALSE(valid)) {
        # If the row is not valid, we don't need to check the matrix
        # isFALSE is called because valid could be NA
        return(tibble::tibble(REPLICATE=replicate, POSITIVE_DEFINITE=FALSE))
      }
      row <- row %>%
        dplyr::select(-c("REPLICATE", "VALID"))
      model <- CampsisModel()
      model@parameters <- parameters
      model <- updateParameters(model=model, row=row)
      omegaMatrix <- rxodeMatrix(model=model, type="omega")
      sigmaMatrix <- rxodeMatrix(model=model, type="sigma")
      omegaMatrixOK <- ifelse(length(omegaMatrix) == 0, TRUE, isMatrixPositiveDefinite(omegaMatrix))
      sigmaMatrixOK <- ifelse(length(sigmaMatrix) == 0, TRUE, isMatrixPositiveDefinite(sigmaMatrix))
      return(tibble::tibble(REPLICATE=replicate, POSITIVE_DEFINITE=omegaMatrixOK && sigmaMatrixOK))
    })
  return(retValue)
}

#' Is matrix positive definite. Same check as \code{mvtnorm} does.
#' 
#' @param matrix matrix to check
#' @param tol tolerance when checking the eigenvalues
#' @export
isMatrixPositiveDefinite <- function(matrix, tol=1e-06) {
  eS <- eigen(matrix, symmetric=TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) {
    return(FALSE)
  }
  return(TRUE)
}
