

#' @rdname parseRecord
setMethod("parseRecord", signature=c("statement_record"), definition=function(object) {
  statements <- ModelStatements()
  
  for (index in seq_len(length(object@code))) {
    line <- object@code[index]
    hasComment <- hasComment(line) 
    comment <- as.character(NA)
    if (hasComment) {
      comment <- extractRhs(line, split="#")
    } else {
      line_ <- extractLhs(line, split="#")
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
  # Save statements
  object@statements <- statements
  
  return(object)
})

#' @rdname parseRecord
setMethod("parseRecord", signature=c("code_record"), definition=function(object) {
  return(object)
})