
getSlotLength <- function(object, slot) {
  return(length(eval(parse(text = paste0("object@", slot)))))
}

checkLength <- function(object, slot, expected=1) {
  lengthSlot <- getSlotLength(object, slot)
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected, ".")
  }
  return(error)
}

expectOne <- function(object, slot) {
  return(checkLength(object, slot, expected=1))
}

addError <- function(error, errors) {
  if (length(error)==0) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

expectOneForAll <- function(object, attrs) {
  errors <- character()
  
  for(attr in attrs) {
    errors <- addError(expectOne(object, attr), errors)
  }
  
  return(errors)
}

