
#' Identify the model parameters present in the variance-covariance matrix.
#' 
#' @param parameters all Campsis parameters, including the variance-covariance matrix
#' @return subset of Campsis parameters
#' 
identifyModelParametersFromVarcov <- function(parameters) {
  varcov <- parameters@varcov
  varcovNames <- colnames(varcov)
  
  # Variance-covariance matrix detected, collect the model parameters
  retValue <- varcovNames %>% purrr::map(.f=function(.x){
    return(parameters %>% getByName(.x)) 
  }) %>% stats::setNames(varcovNames)
  
  return(retValue)
}

#' Sample from a multivariate normal distribution.
#' 
#' @param parameters Campsis parameters present in the variance-covariance matrix
#' @param varcov variance-covariance matrix
#' @param n number of rows to sample
#' @return description
#' 
sampleFromMultivariateNormalDistribution <- function(parameters, varcov, n) {
  # Retrieve variance-covariance matrix names
  varcovNames <- colnames(varcov)
  
  # Retrieve min, max and mean fields
  minMaxMean <- parameters %>% purrr::map_df(.f=function(x) {
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
  
  return(table)
}

#' Sample parameters from inverse scaled chi-squared or wishart distribution(s).
#' 
#' @param parameters subset of Campsis parameters (OMEGAs or SIGMAs)
#' @param n number of rows to sample
#' @param df degree of freedom for the scaled inverse chi-squared or wishart distribution
#' @return a data frame with the sampled parameters
#' @importFrom assertthat assert_that
#' @importFrom LaplacesDemon rinvchisq rinvwishart
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' 
sampleFromInverseChiSquaredOrWishart <- function(parameters, n, df) {
  assertthat::assert_that(parameters %>% length() > 0)
  
  # Type of parameter
  type <- class(parameters@list[[1]]) %>% as.character()
  
  # Detect blocks
  blocks <- OmegaBlocks() %>%
    add(parameters)
  
  retValue <- tibble::tibble(REPLICATE=seq_len(n))
  
  # Iterate over blocks
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

