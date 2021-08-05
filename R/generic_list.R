#_______________________________________________________________________________
#----                         pmx_list class                             ----
#_______________________________________________________________________________

validatePmxList <- function(object) {
  return(expectOneForAll(object, c("type")))
}

#' 
#' PMX list class.
#' 
#' @export
setClass(
  "pmx_list",
  representation(
    list="list",
    type="character" # Interface / Main class type
  ),
  prototype=prototype(list=list()),
  validity= validatePmxList
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

#' Add element to list.
#' 
#' @param object list object
#' @param x element to add
#' @param ... extra arguments
#' @return object
#' @export
#' @rdname add
add <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("add", function(object, x, ...) {
  standardGeneric("add")
})

#' @rdname add
setMethod("add", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  if (validObject(x)) {
    if (!is(x, object@type)) {
      stop(paste0("Element '", x %>% getName(), "' does not extend type '", object@type, "'."))
    
    } else if(object %>% contains(x)) {
      stop(paste0("Element '", x %>% getName(), "' is already present."))
    
    } else {
      object@list <- c(object@list, x)
    }
  }
  return(object)
})

#' @rdname add
setMethod("add", signature=c("pmx_list", "pmx_list"), definition=function(object, x) {
  for (element in x@list) {
    object <- object %>% add(element)
  }
  return(object)
})

#' @rdname add
setMethod("add", signature=c("pmx_list", "list"), definition=function(object, x) {
  for (element in x) {
    object <- object %>% add(element)
  }
  return(object)
})

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

#' Replace element by another in list.
#' 
#' @param object list object
#' @param x element to replace
#' @return list object
#' @export
#' @rdname replace
replace <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("replace", function(object, x) {
  standardGeneric("replace")
})

#' @rdname replace
setMethod("replace", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  if (object %>% contains(x)) {
    index <- object %>% indexOf(x)
    object@list[[index]] <- x
  } else {
    stop(paste("Element", x %>% getName(), "does not exist."))
  }
  return(object)
})

#_______________________________________________________________________________
#----                             indexOf                                   ----
#_______________________________________________________________________________

#' Get the index of an element in list.
#' 
#' @param object list object
#' @param x element to know the index
#' @return index of this element
#' @export
#' @rdname indexOf
indexOf <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("indexOf", function(object, x) {
  standardGeneric("indexOf")
})

#' @rdname indexOf
setMethod("indexOf", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  logicalVector <- object@list %>% purrr::map_lgl(~(.x %>% getName()==x %>% getName()))
  index <- which(logicalVector)
  if (length(index) > 0) {
    index <- index[[1]]
  }
  return(index)
})

#_______________________________________________________________________________
#----                           getByName                                   ----
#_______________________________________________________________________________

#' Get an element from a list by name.
#' 
#' @param object list object
#' @param name element name to search for
#' @return index of this element
#' @export
#' @rdname getByName
getByName <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("getByName", function(object, name) {
  standardGeneric("getByName")
})

#' @rdname getByName
setMethod("getByName", signature=c("pmx_list", "character"), definition=function(object, name) {
  if (is.na(name)) {
    return(NULL)
  }
  element <- object@list %>% purrr::keep(~(!is.na(.x %>% getName()) && .x %>% getName()==name))
  if (length(element) > 0) {
    element <- element[[1]]
  }
  return(element)
})

#_______________________________________________________________________________
#----                            contains                                   ----
#_______________________________________________________________________________

#' Check if an element exists in list.
#' 
#' @param object list object
#' @param x element to check if exists
#' @return logical value
#' @export
#' @rdname contains
contains <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("contains", function(object, x) {
  standardGeneric("contains")
})

#' @rdname contains
setMethod("contains", signature=c("pmx_list", "pmx_element"), definition=function(object, x) {
  return(object %>% getByName(x %>% getName()) %>% length() != 0)
})

#_______________________________________________________________________________
#----                            getNames                                   ----
#_______________________________________________________________________________

#' Get element names from list.
#' 
#' @param object list object
#' @return character vector
#' @export
#' @rdname getNames
getNames <- function(object) {
  stop("No default function is provided")
}

setGeneric("getNames", function(object) {
  standardGeneric("getNames")
})

#' @rdname getNames
setMethod("getNames", signature=c("pmx_list"), definition=function(object) {
  return(object@list %>% purrr::map_chr(~.x %>% getName()))
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the length of this list.
#' 
#' @param x list object
#' @rdname length
setMethod("length", signature=c("pmx_list"), definition=function(x) {
  return(length(x@list))
})

#_______________________________________________________________________________
#----                           getByIndex                                  ----
#_______________________________________________________________________________

#' Get element by index.
#' 
#' @param object list object
#' @param x something to search for, either indexed element or index itself
#' @return element from the list whose index matches with provided index
#' @export
#' @rdname getByIndex
getByIndex <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("getByIndex", function(object, x) {
  standardGeneric("getByIndex")
})

#' @rdname getByIndex
setMethod("getByIndex", signature=c("pmx_list", "integer"), definition=function(object, x) {
  len <- object %>% length()
  assertthat::assert_that(len > 0, msg="x must be greater than 0")
  if (x > len) {
    stop(paste0("Can't find element at index ", x, " in list. List has a length of ", len, "."))
  }
  return(object@list[[x]])
})

#' @rdname getByIndex
setMethod("getByIndex", signature=c("pmx_list", "numeric"), definition=function(object, x) {
  return(getByIndex(object, x=as.integer(x)))
})


#_______________________________________________________________________________
#----                         sort (ABSTRACT)                               ----
#_______________________________________________________________________________

# Reuse base::sort(x, decreasing = FALSE, ...) definition

#' Sort the specified list.
#' 
#' @param x list object
#' @param decreasing increasing or decreasing order
#' @param ... extra arguments
#' @return same list but ordered
#' @export
#' @rdname sort
sort <- function(x, decreasing = FALSE, ...) {
  stop("No default function is provided")
}

setGeneric("sort", function(x, decreasing = FALSE, ...) {
  standardGeneric("sort")
})

#_______________________________________________________________________________
#----                       default (ABSTRACT)                              ----
#_______________________________________________________________________________

#' Get default element from list.
#' 
#' @param object list object
#' @param ... additional arguments
#' @return the default element from list
#' @export
default <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("default", function(object, ...) {
  standardGeneric("default")
})
