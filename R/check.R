
get_slot_length <- function(object, slot) {
  return(length(eval(parse(text = paste0("object@", slot)))))
}

check_length <- function(object, slot, expected=1) {
  lengthSlot <- get_slot_length(object, slot)
  error <- character()
  if (lengthSlot != expected) {
    error <- paste0(slot, " is length ", lengthSlot, ". Should be ", expected, ".")
  }
  return(error)
}

expect_one <- function(object, slot) {
  return(check_length(object, slot, expected=1))
}

add_error <- function(error, errors) {
  if (length(error)==0) {
    return(errors)
  } else {
    return(c(errors, error))
  }
}

expect_one_for_all <- function(object, attrs) {
  errors <- character()
  
  for(attr in attrs) {
    errors <- add_error(expect_one(object, attr), errors)
  }
  
  return(errors)
}

