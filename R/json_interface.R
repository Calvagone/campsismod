
mapJSONPropertiesToSlot <- function(object, json, discard_type=TRUE) {
  json <- json@data
  properties <- names(json)
  if (discard_type) {
    properties <- properties[properties != "type"]
  }
  
  for (property in properties) {
    value <- json[[property]]
    isList <- is.list(value)
    
    if (isList && !is.null(value$type)) {
      # Recursion
      value <- mapJSONPropertiesToSlot(object=new(value$type), json=JSONElement(value), discard_type=TRUE)
    } else {
      if (isList) {
        value <- unlist(value)
      }
      if (is.null(value)) {
        value <- character(0)
      }
    }
    slot(object, property) <- value
  }
  return(object)
}

#' JSON to Campsis dataset.
#' 
#' @param object empty dataset
#' @param json json element
#' @return Campsis dataset
#' @importFrom jsonlite parse_json
#' @keywords internal
#' 
jsonToCampsisModel <- function(object, json) {

  json <- json@data
  model <- object
  
  # Parse model code
  model@model <- read.model(text=unlist(json$model))
  
  # Parse parameters
  jsonParameters <- json$parameters
  jsonThetas <- jsonParameters %>%
    purrr::keep(~.x$type=="theta")
  jsonOmegasOnDiag <- jsonParameters %>%
    purrr::keep(~.x$type=="omega" && is.null(.x$name2))
  jsonOmegasOffDiag <- jsonParameters %>%
    purrr::keep(~.x$type=="omega" && !is.null(.x$name2))
  jsonSigmasOnDiag <- jsonParameters %>%
    purrr::keep(~.x$type=="sigma" && is.null(.x$name2))
  jsonSigmasOffDiag <- jsonParameters %>%
    purrr::keep(~.x$type=="sigma" && !is.null(.x$name2))

  thetas <- jsonThetas %>%
    purrr::imap(.f=function(x, index) {
      x$type <- NULL
      theta <- Theta(index=index)
      loadFromJSON(object=theta, JSONElement(x))
    })
  omegas <- jsonOmegasOnDiag %>%
    purrr::imap(.f=function(x, index) {
      x$type <- x$var_type
      x$var_type <- NULL
      omega <- Omega(index=index, index2=index)
      loadFromJSON(object=omega, JSONElement(x))
    })
  sigmas <- jsonSigmasOnDiag %>%
    purrr::imap(.f=function(x, index) {
      x$type <- x$var_type
      x$var_type <- NULL
      sigma <- Sigma(index=index, index2=index)
      loadFromJSON(object=sigma, JSONElement(x))
    })
  
  model@parameters@list <- c(thetas, omegas, sigmas)
  
  # Update compartments
  model <- model %>%
    updateCompartments()
  
  return(model)
}

#' Open JSON file.
#' 
#' @param json JSON in its string form or path to JSON file
#' @param schema JSON schema
#' 
#' @return parsed JSON object
#' @importFrom jsonlite parse_json
#' @importFrom jsonvalidate json_schema
#' @keywords internal
#' 
openJSON <- function(json, schema=NULL) {
  assertthat::assert_that(length(json)==1, msg="Argument json must be a path or the JSON string")
  
  if (grepl(pattern="\\s*\\[", x=json)) {
    rawJson <- json
  } else {
    rawJson <- suppressWarnings(paste0(readLines(json), collapse="\n"))
  }
  
  # Validate content against schema
  if (getCampsismodOption(name="VALIDATE_JSON", default=TRUE)) {
    obj <- jsonvalidate::json_schema$new(schema)
    obj$validate(rawJson, error=TRUE)
  }
  
  json_ <- jsonlite::parse_json(rawJson, simplifyVector=FALSE)
  
  return(JSONElement(json_))
} 

