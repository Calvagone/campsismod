#_______________________________________________________________________________
#----                               add_rse                                  ----
#_______________________________________________________________________________

#' Add relative standard error (RSE) to the specified parameter.
#'
#' @param object model or parameters object
#' @param parameter parameter object (Theta, Omega or Sigma)
#' @param value RSE value, in percent
#' @param ... extra arguments, unused
#' @return updated object
#' @export
#' @rdname add_rse
add_rse <- function(object, parameter, value, ...) {
  stop("No default function is provided")
}

setGeneric("add_rse", function(object, parameter, value, ...) {
  standardGeneric("add_rse")
})
#_______________________________________________________________________________
#----                        auto_detect_nonmem                             ----
#_______________________________________________________________________________

#' Auto-detect special variables from NONMEM as compartment properties.
#' Bioavailabilities, infusion durations/rates and lag times will be automatically
#' detected.
#'
#' @param object object that has NONMEM special variables to be identified
#' @param ... extra arguments, unused
#' @return updated object
#' @export
#' @rdname auto_detect_nonmem
auto_detect_nonmem <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("auto_detect_nonmem", function(object, ...) {
  standardGeneric("auto_detect_nonmem")
})

#_______________________________________________________________________________
#----                              disable                                  ----
#_______________________________________________________________________________

#' Disable.
#'
#' @param object generic object
#' @param x what needs to be disabled
#' @param ... extra arguments needed for disabling
#' @return object with some disabled features
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
  representation()
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
#----                         get_compartment_index                         ----
#_______________________________________________________________________________

#' Get the compartment index for the specified compartment name.
#'
#' @param object generic object that contains compartments information
#' @param name compartment name
#' @return the corresponding compartment index
#' @export
#' @rdname get_compartment_index
get_compartment_index <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("get_compartment_index", function(object, name) {
  standardGeneric("get_compartment_index")
})

#_______________________________________________________________________________
#----                           get_uncertainty                             ----
#_______________________________________________________________________________

#' Get uncertainty on the parameters.
#'
#' @param object generic object
#' @param ... extra arguments
#' @return data frame with standard error (se) and relative standard error (rse%) columns
#' @export
#' @rdname get_uncertainty
get_uncertainty <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("get_uncertainty", function(object, ...) {
  standardGeneric("get_uncertainty")
})

#_______________________________________________________________________________
#----                          export_to_json                               ----
#_______________________________________________________________________________

#' Export the given object to a JSON object, ready to be written to files.
#'
#' @param object any object
#' @param ... extra arguments, unused
#' @return the loaded S4 object
#' @export
#' @rdname export_to_json
export_to_json <- function(object, ...) {
  stop(sprintf("No default function is provided for 'object': %s", class(object)))
}

setGeneric("export_to_json", function(object, ...) {
  standardGeneric("export_to_json")
})

#_______________________________________________________________________________
#----                            get_var_cov                                ----
#_______________________________________________________________________________

#' Get variance-covariance matrix.
#'
#' @param object generic object
#' @return a variance-covariance matrix (data frame) or NULL if no matrix present
#' @export
#' @rdname get_var_cov
get_var_cov <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_var_cov", function(object) {
  standardGeneric("get_var_cov")
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

#' Fill-in S4 object from the JSON content.
#'
#' @param object pre-initiated S4 object
#' @param json JSON (usually a list)
#' @return the loaded S4 object
#' @export
#' @rdname load_from_json
load_from_json <- function(object, json) {
  stop(sprintf("No default function is provided for 'object': %s, 'json': %s", class(object), class(json)))
}

setGeneric("load_from_json", function(object, json) {
  standardGeneric("load_from_json")
})

#_______________________________________________________________________________
#----                              move                                     ----
#_______________________________________________________________________________

#' Move element 'x' from object to a certain place.
#'
#' @param object generic object (e.g. model, code records, etc.)
#' @param x element to move
#' @param to destination (e.g. a position)
#' @param ... extra arguments, unused
#' @return updated object
#' @export
#' @rdname move
move <- function(object, x, to, ...) {
  stop("No default function is provided")
}

setGeneric("move", function(object, x, to, ...) {
  standardGeneric("move")
})

#_______________________________________________________________________________
#----                                 read                                  ----
#_______________________________________________________________________________

#' Generic read method to read data from a file or a folder.
#'
#' @param file path to the file or folder to be read
#' @param ... extra arguments
#' @return the object representation of the data contained in the file
#' @export
read <- function(file, ...) {
  stop("No default function is provided")
}

setGeneric("read", function(file, ...) {
  standardGeneric("read")
})

#_______________________________________________________________________________
#----                             replace_all                                ----
#_______________________________________________________________________________

#' Replace all occurrences in object.
#'
#' @param object generic object (e.g. model, code_record(s), etc.)
#' @param pattern pattern to be replaced
#' @param replacement replacement string
#' @param ... extra arguments
#' @return the same object with all occurrences replaced
#' @export
replace_all <- function(object, pattern, replacement, ...) {
  stop("No default function is provided")
}

setGeneric("replace_all", function(object, pattern, replacement, ...) {
  standardGeneric("replace_all")
})

#_______________________________________________________________________________
#----                           replicate                                   ----
#_______________________________________________________________________________

#' Replicate generic object.
#'
#' @param object generic object
#' @param n number of replicates required
#' @param settings settings for replication
#' @param ... extra arguments
#' @return object replicated n times
#' @export
#' @rdname replicate
replicate <- function(object, n, settings, ...) {
  stop("No default function is provided")
}

setGeneric("replicate", function(object, n, settings = NULL, ...) {
  n <- as.integer(n)
  if (is.null(settings)) {
    settings <- AutoReplicationSettings()
  }
  standardGeneric("replicate")
})

#_______________________________________________________________________________
#----                                 select                                ----
#_______________________________________________________________________________

#' Get a subset of an object.
#'
#' @param object generic object
#' @param ... arguments to select
#' @return subset of an object
#' @export
#' @rdname select
select <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("select", function(object, ...) {
  standardGeneric("select")
})

#' @rdname select
setMethod("select", signature = c("data.frame"), definition = function(object, ...) {
  return(return(dplyr::select(.data = object, ...)))
})

#_______________________________________________________________________________
#----                           set_min_max                                 ----
#_______________________________________________________________________________

#' Set the minimum and maximum value on a model parameter.
#'
#' @param object model or parameters object
#' @param parameter parameter object (Theta, Omega or Sigma)
#' @param min minimum value for this parameter when parameter uncertainty is enabled
#' @param max maximum value for this parameter when parameter uncertainty is enabled
#' @param ... extra arguments, unused
#' @return updated object
#' @export
#' @rdname set_min_max
set_min_max <- function(object, parameter, min, max, ...) {
  stop("No default function is provided")
}

setGeneric("set_min_max", function(object, parameter, min, max, ...) {
  min <- as.numeric(min)
  max <- as.numeric(max)
  standardGeneric("set_min_max")
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
#----                             to_string                                 ----
#_______________________________________________________________________________

#' to_string generic method.
#'
#' @param object generic object
#' @param ... extra arguments needed for to_string conversion
#' @return character value/vector
#' @export
#' @rdname to_string
to_string <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("to_string", function(object, ...) {
  standardGeneric("to_string")
})

#_______________________________________________________________________________
#----                                 write                                 ----
#_______________________________________________________________________________

#' Write generic object to files.
#'
#' @param object generic object
#' @param file path of the output file or directory
#' @param ... extra arguments
#' @return logical value, TRUE for success, FALSE for failure
#' @export
#' @rdname write
write <- function(object, file, ...) {
  stop("No default function is provided")
}

setGeneric("write", function(object, file, ...) {
  standardGeneric("write")
})
