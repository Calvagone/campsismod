
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
  text <- unlist(json$model)
  if (!is.null(text)) {
    model@model <- read.model(text=text)
  }
  
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
    purrr::imap(~jsonToParameter(.x, .y))
  omegas <- jsonOmegasOnDiag %>%
    purrr::imap(~jsonToParameter(.x, .y))
  sigmas <- jsonSigmasOnDiag %>%
    purrr::imap(~jsonToParameter(.x, .y))
  
  model@parameters@list <- c(thetas, omegas, sigmas)
  
  # Update compartments
  model <- model %>%
    updateCompartments()
  
  return(model)
}

#' JSON to Campsis parameter.
#' 
#' @param x JSON data
#' @param index parameter index to add
#' @return Campsis parameter
#' @export
#' 
jsonToParameter <- function(x, index=NULL) {
  if (x$type=="theta") {
    if (is.null(index)) {
      theta <- Theta()
    } else {
      theta <- Theta(index=index)
    }
    x$type <- NULL
    return(loadFromJSON(object=theta, JSONElement(x)))
    
  } else if (x$type=="omega") {
    if (is.null(index)) {
      omega <- Omega()
    } else {
      omega <- Omega(index=index, index2=index)
    }
    x$type <- x$var_type
    x$var_type <- NULL
    return(loadFromJSON(object=omega, JSONElement(x)))
    
  } else if (x$type=="sigma")  {
    if (is.null(index)) {
      sigma <- Sigma()
    } else {
      sigma <- Sigma(index=index, index2=index)
    }
    x$type <- x$var_type
    x$var_type <- NULL
    return(loadFromJSON(object=sigma, JSONElement(x)))
    
  } else {
    stop("Unknown parameter type")
  }
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
  
  if (grepl(pattern="\\s*[\\[\\{]", x=json)) {
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

