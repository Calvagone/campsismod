
#' Parse statements code and return CAMPSIS statements.
#' 
#' @param code character vector containing all statements (text form)
#' @return a list of CAMPSIS statements
#' 
parseStatements <- function(code) {
  statements <- ModelStatements()
  
  for (index in seq_along(code)) {
    line <- code[index]
    hasComment <- hasComment(line) 
    comment <- as.character(NA)
    if (hasComment) {
      comment <- extractRhs(line, split="#") %>% trim()
      line_ <- extractLhs(line, split="#")
    } else {
      line_ <- line
    }
    
    if (isEmptyLine(line)) {
      statements <- statements %>% add(LineBreak())
      
    } else if (isComment(line)) {
      statements <- statements %>% add(Comment(comment))
      
    } else if (isODE(line_)) {
      lhs <- extractTextBetweenBrackets(line_)
      rhs <- extractRhs(line_) %>% trim()
      statements <- statements %>% add(Ode(lhs, rhs, comment=comment))
    
    } else if (isEquation(line_)) {
      lhs <- extractLhs(line_) %>% trim()
      rhs <- extractRhs(line_) %>% trim()
      statements <- statements %>% add(Equation(lhs, rhs, comment=comment))
      
    } else  {
      statements <- statements %>% add(UnknownStatement(line, comment=comment))
    }
  }
  return(statements)
}

#' Parse properties and return CAMPSIS statements.
#' 
#' @param code character vector containing all properties (text form)
#' @return a list of equations
#' 
parseProperties <- function(code) {
  statements <- parseStatements()
  odes <- statements@list %>% purrr::keep(~is(.x, "ode"))
  if (odes %>% length() > 0) {
    stop("ODE's detected in compartment property record. Please fix CAMPSIS model.")
  }
  unknowStatements <- statements@list %>% purrr::keep(~is(.x, "unknown_statement"))
  if (unknowStatements %>% length() > 0) {
    stop("Unrecognised statements in compartment property record. Please fix CAMPSIS model.")
  }
  ifStatements <- statements@list %>% purrr::keep(~is(.x, "if_statement"))
  if (ifStatements %>% length() > 0) {
    stop("IF-statements in compartment property record are not supported. Please fix CAMPSIS model.")
  }
  return(statements %>% purrr::keep(~is(.x, "equation")))
}

