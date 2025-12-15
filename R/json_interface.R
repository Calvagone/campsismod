#' Map JSON properties to S4 slots.
#' 
#' @param object S4 object
#' @param json JSON element, json_element class
#' @param discard_type discard JSON property 'type'
#' @return a S4 object
#' @export
#' 
mapJSONPropertiesToS4Slots <- function(object, json, discard_type=TRUE) {
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
      value <- mapJSONPropertiesToS4Slots(object=new(value$type), json=JSONElement(value), discard_type=TRUE)
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

#' Map S4 slots to JSON properties.
#' 
#' @param object S4 object
#' @param add_type add type as a property, TRUE by default
#' @param optional properties that are optional in JSON, character vector
#' @param ignore slots to be ignored
#' @return a JSON object ready to be serialised
#' @export
#' 
mapS4SlotsToJSONProperties <- function(object, add_type=TRUE, optional=NULL, ignore=NULL) {
  if (!isS4(object)) {
    stop("Input must be an S4 object.")
  }
  
  # Initialize list for JSON properties
  json <- list()
  
  # Optionally add the type field
  if (add_type) {
    json$type <- class(object)[[1]]
  }
  
  # Iterate over slots
  slotNames <- slotNames(object)
  for (property in slotNames[!slotNames %in% ignore]) {
    value <- slot(object, property)
    
    if (isS4(value)) {
      # Recursive call for nested S4
      json[[property]] <- mapS4SlotsToJSONProperties(value, add_type=TRUE)
      
    } else if (is.list(value)) {
      # Handle lists: check if elements are S4
      json[[property]] <- lapply(value, function(v) {
        if (isS4(v)) {
          mapS4SlotsToJSONProperties(v, add_type=TRUE)
        } else {
          v
        }
      })
      
    } else if (length(value) == 0) {
      # Map empty slot to NULL
      if (!property %in% optional) {
        json[[property]] <- NULL
      }

    } else {
      # Atomic vectors, scalars, etc.
      if (!(property %in% optional && is.na(value))) {
        json[[property]] <- value
      }
    }
  }
  
  return(json)
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
  text <- unlist(json$code)
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
    purrr::imap(~jsonToParameter(x=.x, index=.y, index2=.y))
  omegas <- jsonOmegasOnDiag %>%
    purrr::imap(~jsonToParameter(x=.x, index=.y, index2=.y))
  sigmas <- jsonSigmasOnDiag %>%
    purrr::imap(~jsonToParameter(x=.x, index=.y, index2=.y))
  
  omegaNames <- omegas %>%
    purrr::map(~.x@name)
  sigmaNames <- sigmas %>%
    purrr::map(~.x@name)
  
  omegasOffDiag <- jsonOmegasOffDiag %>%
    purrr::map(~jsonToOffDiagParameter(json=.x, diag_names=omegaNames))
  sigmasOffDiag <- jsonSigmasOffDiag %>%
    purrr::map(~jsonToOffDiagParameter(json=.x, diag_names=sigmaNames))
  
  model@parameters@list <- c(thetas, omegas, omegasOffDiag, sigmas, sigmasOffDiag)
  
  # Update compartments
  model <- model %>%
    updateCompartments()
  
  # Sort model parameters
  model <- model %>%
    campsismod::sort()
  
  return(model)
}

#' Convert JSON correlation parameter (OMEGA or SIGMA) into a Campsis parameter.
#' 
#' @param json JSON data
#' @param diag_names parameter names on the diagonal, character vector
#' @return the corresponding Campsis parameter
#' 
jsonToOffDiagParameter <- function(json, diag_names) {
  name <- json$name
  name2 <- json$name2
  index <- which(diag_names==name)
  index2 <- which(diag_names==name2)
  return(jsonToParameter(x=json, index=index, index2=index2))
}

#' Process JSON double array parameter.
#' 
#' @param x JSON data, OMEGA or SIGMA parameter
#' @return updated JSON data with updated 'name' field and removed 'name2' field
#' 
processJSONDoubleArrayParameter <- function(x) {
  if (!is.null(x$name2)) {
    x$name <- paste0(x$name, "_", x$name2)
    x$name2 <- NULL
  }
  return(x)
}

#' JSON to Campsis parameter.
#' 
#' @param x JSON data
#' @param index parameter index to add
#' @param index2 second parameter index to add for OMEGAs and SIGMAs
#' @return Campsis parameter
#' @export
#' 
jsonToParameter <- function(x, index=NULL, index2=NULL) {
  if (x$type=="theta") {
    if (is.null(index)) {
      theta <- Theta()
    } else {
      theta <- Theta(index=index)
    }
    x$type <- NULL
    return(loadFromJSON(object=theta, JSONElement(x)))
    
  } else if (x$type=="omega") {
    x <- processJSONDoubleArrayParameter(x)
    if (is.null(index)) {
      omega <- Omega()
    } else {
      omega <- Omega(index=index, index2=index2)
    }
    x$type <- x$var_type
    x$var_type <- NULL
    return(loadFromJSON(object=omega, JSONElement(x)))
    
  } else if (x$type=="sigma")  {
    x <- processJSONDoubleArrayParameter(x)
    if (is.null(index)) {
      sigma <- Sigma()
    } else {
      sigma <- Sigma(index=index, index2=index2)
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

