
#' Extract model parameters based on parameter names.
#' 
#' @param parameters all Campsis parameters
#' @param names names of the parameters to extract
#' @return subset of Campsis parameters
#' @importFrom stats setNames
#' 
extractModelParametersFromNames <- function(parameters, names) {

  retValue <- names %>% purrr::map(.f=function(.x){
    parameter <- parameters %>% getByName(.x)
    if (is.null(parameter)) {
      stop(sprintf("Parameter %s not found", .x))
    }
    return(parameter)
  }) %>% stats::setNames(names)
  
  return(retValue)
}

#' Min/max default values for the given parameter.
#' 
#' @param parameter Campsis parameter
#' @return a tibble with the min and max values
#' @importFrom tibble tibble
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
#' @param parameters Campsis parameters present in the variance-covariance matrix
#' @param varcov variance-covariance matrix
#' @param n number of rows to sample
#' @param quiet suppress messages
#' @return a data frame with the sampled parameters
#' 
sampleFromMultivariateNormalDistribution <- function(parameters, varcov, n, quiet) {
  # Retrieve variance-covariance matrix names
  varcovNames <- colnames(varcov)
  
  # Retrieve min, max
  minMax <- parameters %>%
    purrr::map_df(~minMaxDefault(.x)) %>%
    dplyr::mutate(name=varcovNames)
  
  # Retrieve mean
  mean <- parameters %>% purrr::map_dbl(~.x@value)
  
  # Sample parameters from the variance-covariance matrix
  msg <- getSamplingMessageTemplate(what="parameters", from="variance-covariance matrix")
  table <- sampleGeneric(fun=sampleFromMultivariateNormalDistributionCore,
                         args=list(mean=mean, varcov=varcov), n=n, minMax=minMax, msg=msg, quiet=quiet)
  
  return(table)
}

#' Sample parameters from inverse scaled chi-squared or wishart distribution(s).
#' 
#' @param parameters subset of Campsis parameters (OMEGAs or SIGMAs)
#' @param n number of rows to sample
#' @param df degree of freedom for the scaled inverse chi-squared or wishart distribution
#' @param quiet suppress messages
#' @return a data frame with the sampled parameters
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols
#' @importFrom tibble tibble
#' 
sampleFromInverseChiSquaredOrWishart <- function(parameters, n, df, quiet) {
  assertthat::assert_that(parameters %>% length() > 0)
  
  # Type of parameter
  type <- class(parameters@list[[1]]) %>% as.character()

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
      assertthat::assert_that(length(onDiagElements)==1)
      elem <- onDiagElements@list[[1]]
      variable <- elem %>% getName()
      minMax <- minMaxDefault(elem) %>%
        dplyr::mutate(name=variable)
      msg <- getSamplingMessageTemplate(what=variable, from="scaled inverse chi-squared distribution")
      tmp <- sampleGeneric(fun=sampleFromInverseChiSquaredCore, args=list(df=df, scale=elem@value, variable=variable), n=n, minMax=minMax, msg=msg, quiet=quiet)
      retValue <- dplyr::bind_cols(retValue, tmp[, -1])
    } else {
      params <- Parameters()
      params@list <- c(block@on_diag_omegas@list, block@off_diag_omegas@list)
      mat <- rxodeMatrix(params, type=type)

      size <- params %>% keep(~isDiag(.x)) %>% length()
      mappingMat <- getMappingMatrix(parameters=params, type=type)
      allColnames <- as.vector(mappingMat)

      minMax <- params@list %>%
        purrr::map_df(~minMaxDefault(.x)) %>%
        dplyr::mutate(name=params@list %>% purrr::map_chr(~.x %>% getName()))
      
      msg <- getSamplingMessageTemplate(what=sprintf("OMEGA BLOCK(%i)", size), from="scaled inverse Wishart distribution")
      tmp <- sampleGeneric(fun=sampleFromInverseWishartCore, args=list(df=df, mat=mat, allColnames=allColnames), n=n, minMax=minMax, msg=msg, quiet=quiet)
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
sampleFromMultivariateNormalDistributionCore <- function(n, mean, varcov) {
  retValue <- MASS::mvrnorm(n=n, mu=mean, Sigma=varcov) %>%
    tibble::as_tibble()
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
#' @param quiet suppress messages
#' @return tibble with the sampled parameters (1 parameter per column + REPLICATE column)
#' 
sampleGeneric <- function(fun, args, n, minMax, msg, quiet) {
  # First call to method
  tempTable <- do.call(what=fun, args=args %>% append(list(n=n))) %>%
    dplyr::mutate(REPLICATE=seq_len(n)) %>%
    dplyr::mutate(VALID=TRUE)
  table <- flagOutOfRangeParameterRows(table=tempTable, minMax=minMax)
  
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
    if (is.infinite(nextN) || nextN > 1000) {
      nextN <- 1000 # At most 1000 samples
    }
    
    # Increment and check
    iterations <- iterations + 1
    if (iterations > 100) {
      stop("Too many iterations, please check your min and max constraints.")
    }
    
    # print(sprintf("Generate %i rows", nextN))
    shift <- max(table$REPLICATE)
    tempTable <- do.call(what=fun, args=args %>% append(list(n=nextN)))  %>%
      dplyr::mutate(REPLICATE=seq_len(nextN) + shift) %>%
      dplyr::mutate(VALID=TRUE)
    table <- dplyr::bind_rows(table, flagOutOfRangeParameterRows(table=tempTable, minMax=minMax))
  }
  
  # Discard extra rows
  lastValidIndex <- which(table$VALID)[n]
  table <- table[1:lastValidIndex,]
  
  # Computing the success rate
  successRate <- sum(table$VALID) / nrow(table)
  
  # Show message logic
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

#' Flag all parameter rows that have at least one parameter out of the specified range.
#' 
#' @param table a data frame returned by sampleMore
#' @param minMax a data frame with min, max values for each parameter
#' @importFrom dplyr mutate
#' @importFrom purrr map flatten_int
flagOutOfRangeParameterRows <- function(table, minMax) {
  # For each column, check min and max
  # Return the indexes where at least on parameter is out of range
  indexesToDiscard <- colnames(table) %>% purrr::map(.f=function(.x) {
    values <- table[[.x]]
    limits <- minMax %>% dplyr::filter(.data$name==.x)
    return(which(values < limits$min | values > limits$max))
  }) %>% purrr::flatten_int() %>% unique() %>% base::sort()
  
  outOfRange <- rep(FALSE, nrow(table))
  outOfRange[indexesToDiscard] <- TRUE
  
  retValue <- table %>% 
    dplyr::mutate(VALID=!outOfRange)
  
  return(retValue)
}

