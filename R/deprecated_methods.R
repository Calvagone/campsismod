#_______________________________________________________________________________
#----                           generic.R                                   ----
#_______________________________________________________________________________

#' Get the compartment index for the specified compartment name.
#' 
#' `getCompartmentIndex()` is deprecated in favor of `get_compartment_index()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_compartment_index
#' @return the corresponding compartment index
#' @export
#' @rdname getCompartmentIndex
#' @keywords internal
getCompartmentIndex <- function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getCompartmentIndex()", "get_compartment_index()")
  get_compartment_index(object = object, name = name)
}

setGeneric("getCompartmentIndex", function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getCompartmentIndex()", "get_compartment_index()")
  get_compartment_index(object = object, name = name)
})

#' Get uncertainty on the parameters.
#' 
#' `getUncertainty()` is deprecated in favor of `get_uncertainty()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_uncertainty
#' @return data frame with standard error (se) and relative standard error (rse%) columns 
#' @export
#' @rdname getUncertainty
#' @keywords internal
getUncertainty <- function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "getUncertainty()", "get_uncertainty()")
  get_uncertainty(object = object, ...)
}

setGeneric("getUncertainty", function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "getUncertainty()", "get_uncertainty()")
  get_uncertainty(object = object, ...)
})

#' Fill-in S4 object from the JSON content.
#' 
#' `loadFromJSON()` is deprecated in favor of `load_from_json()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams load_from_json
#' @return the loaded S4 object
#' @export
#' @rdname loadFromJSON
#' @keywords internal
loadFromJSON <- function(object, json) {
  lifecycle::deprecate_warn("1.4.0", "loadFromJSON()", "load_from_json()")
  load_from_json(object = object, json = json)
}

setGeneric("loadFromJSON", function(object, json) {
  lifecycle::deprecate_warn("1.4.0", "loadFromJSON()", "load_from_json()")
  load_from_json(object = object, json = json)
})

#' Replace all occurrences in object.
#' 
#' `replaceAll()` is deprecated in favor of `replace_all()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams replace_all
#' @return the loaded S4 object
#' @export
#' @rdname replaceAll
#' @keywords internal
replaceAll <- function(object, pattern, replacement, ...) {
  lifecycle::deprecate_warn("1.4.0", "replaceAll()", "replace_all()")
  replace_all(object = object, pattern = pattern, replacement = replacement, ...)
}

setGeneric("replaceAll", function(object, pattern, replacement, ...) {
  lifecycle::deprecate_warn("1.4.0", "replaceAll()", "replace_all()")
  replace_all(object = object, pattern = pattern, replacement = replacement, ...)
})

#' to_string generic method.
#' 
#' `toString()` is deprecated in favor of `to_string()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams to_string
#' @return character value/vector
#' @export
#' @rdname toString
#' @keywords internal
toString <- function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "toString()", "to_string()")
  to_string(object = object, ...)
}

setGeneric("toString", function(object, ...) {
  lifecycle::deprecate_warn("1.4.0", "toString()", "to_string()")
  to_string(object = object, ...)
})

#_______________________________________________________________________________
#----                        generic_list.R                                 ----
#_______________________________________________________________________________

#' Get the index of an element in list.
#' 
#' `indexOf()` is deprecated in favor of `index_of()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams index_of
#' @return index of this element
#' @export
#' @rdname indexOf
#' @keywords internal
indexOf <- function(object, x) {
  lifecycle::deprecate_warn("1.4.0", "indexOf()", "index_of()")
  index_of(object = object, x = x)
}

setGeneric("indexOf", function(object, x) {
  lifecycle::deprecate_warn("1.4.0", "indexOf()", "index_of()")
  index_of(object = object, x = x)
})

#' #' Get an element from a list by name.
#' 
#' `getByName()` is deprecated in favor of `get_by_name()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_by_name
#' @return the element that was found or NULL if no element was found with the same name
#' @export
#' @rdname getByName
#' @keywords internal
getByName <- function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getByName()", "get_by_name()")
  get_by_name(object = object, name = name)
}

setGeneric("getByName", function(object, name) {
  lifecycle::deprecate_warn("1.4.0", "getByName()", "get_by_name()")
  get_by_name(object = object, name = name)
})

#' Get element names from list.
#' 
#' `getName()` is deprecated in favor of `get_name()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_name
#' @return character vector with all the element names of this list
#' @export
#' @rdname getName
#' @keywords internal
getName <- function(x) {
  lifecycle::deprecate_warn("1.4.0", "getName()", "get_name()")
  get_name(x = x)
}

setGeneric("getName", function(x) {
  lifecycle::deprecate_warn("1.4.0", "getName()", "get_name()")
  get_name(x = x)
})

#' Get element names from list.
#' 
#' `getNames()` is deprecated in favor of `get_names()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_names
#' @return character vector with all the element names of this list
#' @export
#' @rdname getNames
#' @keywords internal
getNames <- function(object) {
  lifecycle::deprecate_warn("1.4.0", "getNames()", "get_names()")
  get_names(object = object)
}

setGeneric("getNames", function(object) {
  lifecycle::deprecate_warn("1.4.0", "getNames()", "get_names()")
  get_names(object = object)
})

#' Get element by index.
#' 
#' `getByIndex()` is deprecated in favor of `get_by_index()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_by_index
#' @return index of this element
#' @export
#' @rdname getByIndex
#' @keywords internal
getByIndex <- function(object, x) {
  lifecycle::deprecate_warn("1.4.0", "getByIndex()", "get_by_index()")
  get_by_index(object = object, x = x)
}

setGeneric("getByIndex", function(object, x) {
  lifecycle::deprecate_warn("1.4.0", "getByIndex()", "get_by_index()")
  get_by_index(object = object, x = x)
})

#_______________________________________________________________________________
#----                           utilities.R                                 ----
#_______________________________________________________________________________

#' Process extra arguments.
#' 
#' `processExtraArg()` is deprecated in favor of `process_extra_arg()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams process_extra_arg
#' @return requested argument value
#' @export
#' @rdname processExtraArg
#' @keywords internal
processExtraArg <- function(args, name, default=NULL, mandatory=FALSE) {
  lifecycle::deprecate_warn("1.4.0", "processExtraArg()", "process_extra_arg()")
  process_extra_arg(args = args, name = name, default = default, mandatory = mandatory)
}

#_______________________________________________________________________________
#----                        json_interface.R                               ----
#_______________________________________________________________________________

#' Map JSON properties to S4 slots.
#' 
#' `mapJSONPropertiesToS4Slots()` is deprecated in favor of `map_json_properties_to_s4_slots()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams process_extra_arg
#' @return a JSON object ready to be serialised
#' @export
#' @rdname mapJSONPropertiesToS4Slots
#' @keywords internal
mapJSONPropertiesToS4Slots <- function(object, json, discard_type=TRUE) {
  lifecycle::deprecate_warn("1.4.0", "mapJSONPropertiesToS4Slots()", "map_json_properties_to_s4_slots()")
  map_json_properties_to_s4_slots(object = object, json = json, discard_type = discard_type)
}

#' Map S4 slots to JSON properties.
#' 
#' `mapS4SlotsToJSONProperties()` is deprecated in favor of `map_s4_slots_to_json_properties()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams process_extra_arg
#' @return Campsis dataset
#' @export
#' @rdname mapS4SlotsToJSONProperties
#' @keywords internal
mapS4SlotsToJSONProperties <- function(object, add_type=TRUE, optional=NULL, ignore=NULL) {
  lifecycle::deprecate_warn("1.4.0", "mapS4SlotsToJSONProperties()", "map_s4_slots_to_json_properties()")
  map_s4_slots_to_json_properties(object = object, add_type = add_type, optional = optional, ignore = ignore)
}

#' JSON to Campsis parameter.
#' 
#' `jsonToParameter()` is deprecated in favor of `json_to_parameter()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams json_to_parameter
#' @return Campsis parameter
#' @export
#' @keywords internal
jsonToParameter <- function(x, index=NULL, index2=NULL) {
  lifecycle::deprecate_warn("1.4.0", "jsonToParameter()", "json_to_parameter()")
  json_to_parameter(x = x, index = index, index2 = index2)
}

#_______________________________________________________________________________
#----                       model_add_suffix.R                              ----
#_______________________________________________________________________________

#' Generic function to add a suffix to various objects like parameters, code records,
#' compartment names or a model (all previous objects at the same time).
#' This makes it an extremely powerful function to combine 2 models or more (using function 'add'),
#' that have similar equation, parameter or compartment names.
#' 
#' `addSuffix()` is deprecated in favor of `add_suffix()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams add_suffix
#' @return updated object of the same class as the provided object, unless 'model' was specified, in that case the model is returned
#' @export
#' @rdname addSuffix
#' @keywords internal
addSuffix <- function(object, suffix, separator, ...) {
  lifecycle::deprecate_warn("1.4.0", "addSuffix()", "add_suffix()")
  add_suffix(object = object, suffix = suffix, separator = separator, ...)
}

setGeneric("addSuffix", function(object, suffix, separator, ...) {
  lifecycle::deprecate_warn("1.4.0", "addSuffix()", "add_suffix()")
  add_suffix(object = object, suffix = suffix, separator = separator, ...)
})

#_______________________________________________________________________________
#----                          parameter(s).R                               ----
#_______________________________________________________________________________

#' Fix omega matrix for SAME OMEGA parameters that have NA values due to imperfections in Pharmpy import.
#' 
#' `fixOmega()` is deprecated in favor of `fix_omega()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams fix_omega
#' @return the parameter that matches
#' @export
#' @rdname fixOmega
#' @keywords internal
fixOmega <- function(object) {
  lifecycle::deprecate_warn("1.4.0", "fixOmega()", "fix_omega()")
  fix_omega(object = object)
}

setGeneric("fixOmega", function(object) {
  lifecycle::deprecate_warn("1.4.0", "fixOmega()", "fix_omega()")
  fix_omega(object = object)
})

#' Get the name of the given parameter in the Campsis model.
#' 
#' 
#' `getNameInModel()` is deprecated in favor of `get_name_in_model()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams get_name_in_model
#' @return the name of this parameter
#' @export
#' @rdname getNameInModel
#' @keywords internal
getNameInModel <- function(x) {
  lifecycle::deprecate_warn("1.4.0", "getNameInModel()", "get_name_in_model()")
  get_name_in_model(x = x)
}

setGeneric("getNameInModel", function(x) {
  lifecycle::deprecate_warn("1.4.0", "getNameInModel()", "get_name_in_model()")
  get_name_in_model(x = x)
})

#' Is diagonal.
#' 
#' 
#' `isDiag()` is deprecated in favor of `is_diag()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams is_diag
#' @return logical value
#' @export
#' @rdname isDiag
#' @keywords internal
isDiag <- function(object) {
  lifecycle::deprecate_warn("1.4.0", "isDiag()", "is_diag()")
  is_diag(object = object)
}

setGeneric("isDiag", function(object) {
  lifecycle::deprecate_warn("1.4.0", "isDiag()", "is_diag()")
  is_diag(object = object)
})

#' Min index.
#' 
#' `minIndex()` is deprecated in favor of `min_index()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams min_index
#' @return min index
#' @export
#' @rdname minIndex
#' @keywords internal
minIndex <- function(object) {
  lifecycle::deprecate_warn("1.4.0", "minIndex()", "min_index()")
  min_index(object = object)
}

setGeneric("minIndex", function(object) {
  lifecycle::deprecate_warn("1.4.0", "minIndex()", "min_index()")
  min_index(object = object)
})

#' Max index.
#' 
#' `maxIndex()` is deprecated in favor of `max_index()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams max_index
#' @return max index
#' @export
#' @rdname maxIndex
#' @keywords internal
maxIndex <- function(object) {
  lifecycle::deprecate_warn("1.4.0", "maxIndex()", "max_index()")
  max_index(object = object)
}

setGeneric("maxIndex", function(object) {
  lifecycle::deprecate_warn("1.4.0", "maxIndex()", "max_index()")
  max_index(object = object)
})

#_______________________________________________________________________________
#----                           Other methods                               ----
#_______________________________________________________________________________

#' Get the OMEGA/SIGMA matrix for rxode2.
#' 
#' `rxodeMatrix()` is deprecated in favor of `rxode_matrix()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams rxode_matrix
#' @return omega/sigma named matrix
#' @export
#' @rdname rxodeMatrix
#' @keywords internal
rxodeMatrix <- function(model, type) {
  lifecycle::deprecate_warn("1.4.0", "rxodeMatrix()", "rxode_matrix()")
  rxode_matrix(model = model, type = type)
}

setGeneric("rxodeMatrix", function(model, type) {
  lifecycle::deprecate_warn("1.4.0", "rxodeMatrix()", "rxode_matrix()")
  rxode_matrix(model = model, type = type)
})

#' Update compartments list from the persisted records.
#' Exported especially for package \code{campsistrans}.
#' However, this method should not be called.
#' 
#' `updateCompartments()` is deprecated in favor of `update_compartments()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams update_compartments
#' @return an updated Campsis model, with an updated compartments list
#' @export
#' @rdname updateCompartments
#' @keywords internal
updateCompartments <- function(model) {
  lifecycle::deprecate_warn("1.4.0", "updateCompartments()", "update_compartments()")
  update_compartments(model = model)
}

setGeneric("updateCompartments", function(model) {
  lifecycle::deprecate_warn("1.4.0", "updateCompartments()", "update_compartments()")
  update_compartments(model = model)
})
