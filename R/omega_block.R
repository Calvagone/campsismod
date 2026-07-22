
#_______________________________________________________________________________
#----                           omega_block class                           ----
#_______________________________________________________________________________

setClass(
  "omega_block",
  representation(
    block_index="integer",
    start_index="integer",
    on_diag_omegas="parameters",
    off_diag_omegas="parameters"
  ),
  contains="pmx_element"
)

#' Create a block of OMEGA's.
#'
#' @export
OmegaBlock <- function() {
  return(new("omega_block"))
}

#_______________________________________________________________________________
#----                                add                                    ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature=c("omega_block", "double_array_parameter"), definition=function(object, x) {
  if (x %>% is_diag()) {
    object@on_diag_omegas <- object@on_diag_omegas %>% add(x)
  } else {
    object@off_diag_omegas <- object@off_diag_omegas %>% add(x)
  }
  return(object)
})

#_______________________________________________________________________________
#----                              get_name                                  ----
#_______________________________________________________________________________

#' @rdname get_name
setMethod("get_name", signature=c("omega_block"), definition=function(x) {
  return(paste0("Omega block ", x@block_index))
})

#_______________________________________________________________________________
#----                           get_omega_indexes                             ----
#_______________________________________________________________________________

#' Get the indexes of the omegas.
#' 
#' @param object omega block
#' @return a list of integers
#' @export
#' @rdname get_omega_indexes
get_omega_indexes <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_omega_indexes", function(object) {
  standardGeneric("get_omega_indexes")
})

#' @rdname get_omega_indexes
setMethod("get_omega_indexes", signature=c("omega_block"), definition=function(object) {
  # Careful: don't use parameters1 %>% add(parameters2) because it will shift indexes in parameters2
  omegas <- object@on_diag_omegas@list %>% append(object@off_diag_omegas@list)
  retValue <- omegas %>% purrr::map(.f=~c(.x@index, .x@index2)) %>% purrr::flatten_int() %>% unique()
  return(retValue)
})

#_______________________________________________________________________________
#----                       has_off_diagonal_omegas                            ----
#_______________________________________________________________________________

#' Has off-diagonal omegas.
#' 
#' @param object omega block
#' @return TRUE or FALSE
#' @export
#' @rdname has_off_diagonal_omegas
has_off_diagonal_omegas <- function(object) {
  stop("No default function is provided")
}

setGeneric("has_off_diagonal_omegas", function(object) {
  standardGeneric("has_off_diagonal_omegas")
})

#' @rdname has_off_diagonal_omegas
setMethod("has_off_diagonal_omegas", signature=c("omega_block"), definition=function(object) {
  return(object@off_diag_omegas %>% length() > 0)
})

#_______________________________________________________________________________
#----                         shift_omega_indexes                             ----
#_______________________________________________________________________________

#' Shift OMEGA indexes.
#' 
#' @param object omega block
#' @return same block but shifted
#' @export
#' @rdname shift_omega_indexes
shift_omega_indexes <- function(object) {
  stop("No default function is provided")
}

setGeneric("shift_omega_indexes", function(object) {
  standardGeneric("shift_omega_indexes")
})

shiftOmega <- function(omega, x) {
  omega@index <- omega@index + x
  omega@index2 <- omega@index2 + x
  return(omega)
}

#' @rdname shift_omega_indexes
setMethod("shift_omega_indexes", signature=c("omega_block"), definition=function(object) {
  object@on_diag_omegas@list <- object@on_diag_omegas@list %>% purrr::map(.f=~shiftOmega(.x, -object@start_index))
  object@off_diag_omegas@list <- object@off_diag_omegas@list %>% purrr::map(.f=~shiftOmega(.x, -object@start_index))
  return(object)
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the number of OMEGA's on the diagonal.
#' 
#' @param x omega block
#' @return a number
#' @rdname length
setMethod("length", signature=c("omega_block"), definition=function(x) {
  return(length(x@on_diag_omegas))
})

#_______________________________________________________________________________
#----                                  show                                 ----
#_______________________________________________________________________________

getBlockLabel <- function(object) {
  omegaNames <- object@on_diag_omegas@list %>%
    purrr::map_chr(.f=~.x %>% get_name())
  retValue <- sprintf("BLOCK(%i) - %s", length(omegaNames), omegaNames %>% paste(collapse=" / "))
  return(retValue)
}

setMethod("show", signature=c("omega_block"), definition=function(object) {
  cat(getBlockLabel(object))
})
