#_______________________________________________________________________________
#----                   compartment_property class                    ----
#_______________________________________________________________________________

#' 
#' Compartment property class.
#' 
#' @slot compartment related compartment index
#' @slot rhs right-hand side formula
#' @slot comment comment if any, single character string
#' @export
setClass(
  "compartment_property",
  representation(
    compartment = "integer",
    rhs = "character",
    comment = "character"
  ),
  contains="pmx_element",
  prototype=prototype(comment=as.character(NA), rhs=""),
  validity=function(object) {
    return(expectOneForAll(object, c("compartment", "rhs")))
  } 
)

#_______________________________________________________________________________
#----                            get_prefix                                 ----
#_______________________________________________________________________________

#' Get prefix.
#' 
#' @param object generic object
#' @param ... e.g. \code{dest='mrgsolve'}
#' @return the prefix of this object
#' @export
#' @rdname get_prefix
get_prefix <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("get_prefix", function(object, ...) {
  standardGeneric("get_prefix")
})

#_______________________________________________________________________________
#----                         get_record_name                               ----
#_______________________________________________________________________________

#' Get record name.
#' 
#' @param object generic object
#' @return the name of the record
#' @export
#' @rdname get_record_name
get_record_name <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_record_name", function(object) {
  standardGeneric("get_record_name")
})

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' @rdname replace_all
setMethod("replace_all", signature=c("compartment_property", "pattern", "character"), definition=function(object, pattern, replacement, ...) {
  object@rhs <- object@rhs %>% replace_all(pattern=pattern, replacement=replacement, ...)
  return(object)
})

#_______________________________________________________________________________
#----                               show                                    ----
#_______________________________________________________________________________

setMethod("show", signature=c("compartment_property"), definition=function(object) {
  cat(paste0(object %>% get_name(), ": ", object@rhs))
})

#_______________________________________________________________________________
#----                             to_string                                 ----
#_______________________________________________________________________________

#' @rdname to_string
setMethod("to_string", signature=c("compartment_property"), definition=function(object, ...) {
  model <- process_extra_arg(args=list(...), name="model", mandatory=TRUE)
  dest <- process_extra_arg(args=list(...), name="dest", mandatory=TRUE)
  
  compartmentIndex <- object@compartment
  compartment <- model@compartments %>% find(Compartment(index=compartmentIndex))
  
  if (is_rxode(dest)) {
    return(paste0(object %>% get_prefix(dest=dest), "(", compartment %>% to_string(), ")=", object@rhs))
  } else if (dest=="mrgsolve") {
    return(paste0(object %>% get_prefix(dest=dest), "_", compartment %>% to_string(), "=", object@rhs))
  } else if (dest=="campsis") {
    return(paste0(compartment %>% to_string(), "=", object@rhs))
  } else {
    UnsupportedDestException()
  }
})

