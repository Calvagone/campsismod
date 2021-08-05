
#_______________________________________________________________________________
#----                            addEquation                                ----
#_______________________________________________________________________________

#' Add equation.
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable
#' @param rhs right-hand-side expression
#' @param ... extra arguments like before or after
#' @return updated object
#' @export
#' @rdname addEquation
addEquation <- function(object, lhs, rhs, ...) {
  stop("No default function is provided")
}

setGeneric("addEquation", function(object, lhs, rhs, ...) {
  standardGeneric("addEquation")
})

#_______________________________________________________________________________
#----                         autoDetectNONMEM                              ----
#_______________________________________________________________________________

#' Auto-detect special variables from NONMEM to fill in compartment properties.
#' Bioavailabilities, infusion durations/rates and lag times will be automatically
#' detected.
#' 
#' @param object object that has NONMEM special variables to be identified
#' @return updated object
#' @export
#' @rdname autoDetectNONMEM
autoDetectNONMEM <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("autoDetectNONMEM", function(object, ...) {
  standardGeneric("autoDetectNONMEM")
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' Disable.
#' 
#' @param object generic object
#' @param x what needs to be disabled
#' @param ... extra arguments needed for disabling
#' @return standardised object
#' @export
#' @rdname disable
disable <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("disable", function(object, x, ...) {
  standardGeneric("disable")
})

#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' Export type class.
#' 
#' @export
setClass(
  "export_type",
  representation(
  )
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export function.
#' 
#' @param object generic object
#' @param dest destination
#' @param ... optional arguments
#' @return specific object depending on given destination
#' @export
#' @rdname export
export <- function(object, dest, ...) {
  stop("No default function is provided")
}

setGeneric("export", function(object, dest, ...) {
  standardGeneric("export")
})

#_______________________________________________________________________________
#----                          getCompartmentIndex                          ----
#_______________________________________________________________________________

#' Get the compartment index for the specified compartment name.
#' 
#' @param object generic object that contains compartments information
#' @param name compartment name
#' @return the corresponding compartment index
#' @export
#' @rdname getCompartmentIndex
getCompartmentIndex <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("getCompartmentIndex", function(object, name) {
  standardGeneric("getCompartmentIndex")
})

#_______________________________________________________________________________
#----                              getEquation                              ----
#_______________________________________________________________________________

#' Get equation from code record(s).
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable
#' @return equation, character or NULL if not found
#' @export
#' @rdname getEquation
getEquation <- function(object, lhs) {
  stop("No default function is provided")
}

setGeneric("getEquation", function(object, lhs) {
  standardGeneric("getEquation")
})

#_______________________________________________________________________________
#----                            hasEquation                                ----
#_______________________________________________________________________________

#' Has equation.
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable to search 
#' @return TRUE or FALSE
#' @export
#' @rdname hasEquation
hasEquation <- function(object, lhs) {
  stop("No default function is provided")
}

setGeneric("hasEquation", function(object, lhs) {
  standardGeneric("hasEquation")
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Read generic object.
#' 
#' @param file path where to read the file
#' @param ... extra arguments
#' @export
read <- function(file, ...) {
  stop("No default function is provided")
}

setGeneric("read", function(file, ...) {
  standardGeneric("read")
})

#_______________________________________________________________________________
#----                           removeEquation                              ----
#_______________________________________________________________________________

#' Remove equation from code record.
#' 
#' @param object object containing code record(s)
#' @param lhs left-hand-side variable
#' @return updated code record
#' @export
#' @rdname removeEquation
removeEquation <- function(object, lhs) {
  stop("No default function is provided")
}

setGeneric("removeEquation", function(object, lhs) {
  standardGeneric("removeEquation")
})

#_______________________________________________________________________________
#----                           replaceEquation                             ----
#_______________________________________________________________________________

#' Replace equation in code records.
#' 
#' @param object object containing code records
#' @param lhs left-hand-side variable
#' @param rhs right-hand-side expression
#' @return updated code records
#' @export
#' @rdname replaceEquation
replaceEquation <- function(object, lhs, rhs) {
  stop("No default function is provided")
}

setGeneric("replaceEquation", function(object, lhs, rhs) {
  standardGeneric("replaceEquation")
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

#' Select something from given object.
#' 
#' @param object generic object
#' @param ... arguments to select
#' @return filtered object
#' @export
#' @rdname select
select <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("select", function(object, ...) {
  standardGeneric("select")
})

#' @rdname select
setMethod("select", signature=c("data.frame"), definition=function(object, ...) {
  return(return(dplyr::select(.data=object, ...)))
})

#_______________________________________________________________________________
#----                            standardise                                ----
#_______________________________________________________________________________

#' Standardise.
#' 
#' @param object generic object
#' @param ... extra arguments needed for standardisation
#' @return standardised object
#' @export
#' @rdname standardise
standardise <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("standardise", function(object, ...) {
  standardGeneric("standardise")
})

#_______________________________________________________________________________
#----                             toString                                  ----
#_______________________________________________________________________________

#' ToString generic method
#' 
#' @param object generic object
#' @param ... extra arguments needed for toString conversion
#' @return character value/vector
#' @export
#' @rdname toString
toString <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("toString", function(object, ...) {
  standardGeneric("toString")
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write generic object.
#' 
#' @param object generic object
#' @param file path of the output dir or ZIP filename
#' @param ... extra arguments
#' @export
#' @rdname write
write <- function(object, file, ...) {
  stop("No default function is provided")
}

setGeneric("write", function(object, file, ...) {
  standardGeneric("write")
})
