#_______________________________________________________________________________
#----                  covariate_definitions class                          ----
#_______________________________________________________________________________

#'
#' Covariate definitions class.
#'
#' @export
setClass(
  "covariate_definitions",
  contains = "pmx_list",
  prototype = prototype(type = "covariate_definition")
)

#'
#' Create a list of covariate definitions.
#'
#' @return an empty list of covariate definitions
#' @export
CovariateDefinitions <- function() {
  return(new("covariate_definitions"))
}

#_______________________________________________________________________________
#----                              add                                      ----
#_______________________________________________________________________________

#' @rdname add
setMethod("add", signature = c("covariate_definitions", "covariate_definition"), definition = function(object, x) {
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                             find                                      ----
#_______________________________________________________________________________

#' @rdname find
setMethod("find", signature = c("covariate_definitions", "covariate_definition"), definition = function(object, x) {
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                             delete                                    ----
#_______________________________________________________________________________

#' @rdname delete
setMethod("delete", signature = c("covariate_definitions", "covariate_definition"), definition = function(object, x) {
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                             replace                                   ----
#_______________________________________________________________________________

#' @rdname replace
setMethod("replace", signature = c("covariate_definitions", "covariate_definition"), definition = function(object, x) {
  return(methods::callNextMethod(object, x))
})

#_______________________________________________________________________________
#----                                 sort                                  ----
#_______________________________________________________________________________

#' @rdname sort
setMethod("sort", signature = c("covariate_definitions"), definition = function(x, decreasing = FALSE, ...) {
  if (length(x) > 0) {
    names <- x %>% get_names()
    x@list <- x@list[order(names, decreasing = decreasing)]
  }
  return(x)
})

#_______________________________________________________________________________
#----                          export_to_json                               ----
#_______________________________________________________________________________

#' @rdname export_to_json
setMethod("export_to_json", signature = c("covariate_definitions"), definition = function(object, ...) {
  json <- object@list %>%
    purrr::map(function(x) {
      return(export_to_json(x)@data)
    })
  return(JSONElement(json))
})
