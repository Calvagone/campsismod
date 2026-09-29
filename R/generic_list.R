#_______________________________________________________________________________
#----                         pmx_list class                             ----
#_______________________________________________________________________________

#'
#' PMX list class.
#'
#' @slot list effective list which will contain the elements
#' @slot type type of the elements this list may contain
#' @export
#' @keywords internal
setClass(
  "pmx_list",
  representation(
    list = "list",
    type = "character" # Interface / Main class type
  ),
  prototype = prototype(list = list()),
  validity = function(object) {
    check <- expect_one(object, "type")
    for (elem in object@list) {
      methods::validObject(elem, complete = TRUE) # TEST=FALSE (default) raises error
    }
    return(check)
  }
)

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

#' Add element to list.
#'
#' @param object list object
#' @param x element to add
#' @param ... extra arguments, unused by this generic list
#' @return modified list object
#' @export
#' @rdname add
add <- function(object, x, ...) {
  stop("No default function is provided")
}

setGeneric("add", function(object, x, ...) {
  standardGeneric("add")
})

#' @param pos position where x needs to be added in list
#' @rdname add
setMethod("add", signature = c("pmx_list", "pmx_element"), definition = function(object, x, pos = NULL) {
  if (methods::validObject(x)) {
    if (!is(x, object@type)) {
      stop(paste0("Element '", x %>% get_name(), "' does not extend type '", object@type, "'."))
    } else if (object %>% contains(x)) {
      stop(paste0("Element '", x %>% get_name(), "' is already present."))
    } else {
      if (is.null(pos)) {
        pos <- Position(object %>% length(), after = TRUE)
      }
      if (pos@by_index) {
        index <- pos@index
      } else if (pos@by_element) {
        index <- object %>% index_of(pos@element)
      } else {
        stop("Element position can only by index or by position")
      }
      if (!pos@after) {
        index <- index - 1
      }
      object@list <- object@list %>% append(x, after = index)
    }
  }
  return(object)
})

#' @rdname add
setMethod("add", signature = c("pmx_list", "pmx_list"), definition = function(object, x) {
  return(object %>% add(x@list))
})

#' @rdname add
setMethod("add", signature = c("pmx_list", "list"), definition = function(object, x) {
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
#' @return list object or an error if the element does not exist in the list
#' @export
#' @rdname replace
replace <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("replace", function(object, x) {
  standardGeneric("replace")
})

#' @rdname replace
setMethod("replace", signature = c("pmx_list", "pmx_element"), definition = function(object, x) {
  if (object %>% contains(x)) {
    index <- object %>% index_of(x)
    object@list[[index]] <- x
  } else {
    stop(paste("Element", x %>% get_name(), "does not exist."))
  }
  return(object)
})

#' @rdname replace
setMethod("replace", signature = c("pmx_list", "pmx_list"), definition = function(object, x) {
  return(object %>% replace(x@list))
})

#' @rdname replace
setMethod("replace", signature = c("pmx_list", "list"), definition = function(object, x) {
  for (element in x) {
    object <- object %>% replace(element)
  }
  return(object)
})

#_______________________________________________________________________________
#----                             index_of                                  ----
#_______________________________________________________________________________

#' Get the index of an element in list.
#'
#' @param object list object
#' @param x element to know the index
#' @return index of this element
#' @export
#' @rdname index_of
index_of <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("index_of", function(object, x) {
  standardGeneric("index_of")
})

#' @rdname index_of
setMethod("index_of", signature = c("pmx_list", "pmx_element"), definition = function(object, x) {
  logicalVector <- object@list %>%
    purrr::map_lgl(.f = function(.x) {
      retValue <- .x %>% get_name() == x %>% get_name()
      return(ifelse(is.na(retValue), FALSE, retValue))
    })
  index <- which(logicalVector)
  if (length(index) > 0) {
    index <- index[[1]]
  }
  return(index)
})

#_______________________________________________________________________________
#----                          get_by_name                                  ----
#_______________________________________________________________________________

#' Get an element from a list by name.
#' Never return more than 1 element.
#'
#' @param object list object
#' @param name element name to search for
#' @return the element that was found or NULL if no element was found with the same name
#' @export
#' @rdname get_by_name
get_by_name <- function(object, name) {
  stop("No default function is provided")
}

setGeneric("get_by_name", function(object, name) {
  standardGeneric("get_by_name")
})

#' @rdname get_by_name
setMethod("get_by_name", signature = c("pmx_list", "character"), definition = function(object, name) {
  if (is.na(name)) {
    return(NULL)
  }
  return(object@list %>% purrr::detect(~ (!is.na(.x %>% get_name()) && .x %>% get_name() == name)))
})

#_______________________________________________________________________________
#----                            contains                                   ----
#_______________________________________________________________________________

#' Check if an element exists in list.
#'
#' @param object list object
#' @param x element to check if exists
#' @return logical value, TRUE or FALSE
#' @export
#' @rdname contains
#' @keywords internal
contains <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("contains", function(object, x) {
  standardGeneric("contains")
})

#' @rdname contains
setMethod("contains", signature = c("pmx_list", "pmx_element"), definition = function(object, x) {
  return(!is.null(object %>% find(x)))
})

#_______________________________________________________________________________
#----                             delete                                    ----
#_______________________________________________________________________________

#' Delete an element from this list.
#'
#' @param object list object
#' @param x element to delete or element index
#' @return the updated list
#' @export
#' @rdname delete
delete <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("delete", function(object, x) {
  if (is.numeric(x)) {
    x <- as.integer(x)
  }
  if (is.integer(x) && x %>% length() > 1) {
    stop("Only 1 element can be deleted at a time")
  }
  standardGeneric("delete")
})

#' @rdname delete
setMethod("delete", signature = c("pmx_list", "pmx_element"), definition = function(object, x) {
  index <- object %>% index_of(x)
  if (index %>% length() > 0) {
    object@list <- object@list[-index]
    return(object)
  } else {
    stop(paste("Element", x %>% get_name(), "does not exist."))
  }
})

#' @rdname delete
setMethod("delete", signature = c("pmx_list", "integer"), definition = function(object, x) {
  if (x %>% length() != 1) {
    stop("x must be a single integer/numeric value")
  }
  if (x > 0 && x <= object %>% length()) {
    object@list <- object@list[-x]
    return(object)
  } else {
    stop(paste("No element exists at index", x))
  }
})

#_______________________________________________________________________________
#----                              find                                     ----
#_______________________________________________________________________________

#' Find an element in list.
#'
#' @param object list object
#' @param x element to find, only key slots need to be filled in
#' @return the element from the list that has same name as x, or NULL if no element was found
#' @export
#' @rdname find
find <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("find", function(object, x) {
  standardGeneric("find")
})

#' @rdname find
setMethod("find", signature = c("pmx_list", "pmx_element"), definition = function(object, x) {
  return(object %>% get_by_name(x %>% get_name()))
})

#_______________________________________________________________________________
#----                            get_names                                  ----
#_______________________________________________________________________________

#' Get element names from list.
#'
#' @param object list object
#' @return character vector with all the element names of this list
#' @export
#' @rdname get_names
get_names <- function(object) {
  stop("No default function is provided")
}

setGeneric("get_names", function(object) {
  standardGeneric("get_names")
})

#' @rdname get_names
setMethod("get_names", signature = c("pmx_list"), definition = function(object) {
  return(object@list %>% purrr::map_chr(~ .x %>% get_name()))
})

#_______________________________________________________________________________
#----                             length                                    ----
#_______________________________________________________________________________

#' Return the length of this list.
#'
#' @param x list object
#' @return the length of this list, integer value
#' @rdname length
#' @keywords internal
setMethod("length", signature = c("pmx_list"), definition = function(x) {
  return(length(x@list))
})

#_______________________________________________________________________________
#----                          get_by_index                                 ----
#_______________________________________________________________________________

#' Get element by index.
#'
#' @param object list object
#' @param x element index
#' @return element from the list whose index matches with provided index
#' @export
#' @rdname get_by_index
get_by_index <- function(object, x) {
  stop("No default function is provided")
}

setGeneric("get_by_index", function(object, x) {
  standardGeneric("get_by_index")
})

#' @rdname get_by_index
setMethod("get_by_index", signature = c("pmx_list", "integer"), definition = function(object, x) {
  len <- object %>% length()
  assertthat::assert_that(len > 0, msg = "x must be greater than 0")
  if (x > len) {
    stop(paste0("Can't find element at index ", x, " in list. List has a length of ", len, "."))
  }
  return(object@list[[x]])
})

#' @rdname get_by_index
setMethod("get_by_index", signature = c("pmx_list", "numeric"), definition = function(object, x) {
  return(get_by_index(object, x = as.integer(x)))
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
