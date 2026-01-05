
#' Parse statements code and return Campsis statements.
#' 
#' @param code character vector containing all statements (text form)
#' @return a list of Campsis statements
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
      
    } else if (isIfStatement(line_)) {
      statements <- statements %>% add(parseIfStatement(line_, comment=comment))
      
    } else  {
      statements <- statements %>% add(UnknownStatement(line_, comment=comment))
    }
  }
  return(statements)
}

#' Parse IF-statement.
#' Assumption: \code{isIfStatement} method already called and returned TRUE.
#' 
#' @param line IF-statement as single character string value, comment omitted
#' @param comment any comment, NA by default
#' @return an IF statement object
#' 
parseIfStatement <- function(line, comment=as.character(NA)) {
  # Trim input
  line <- line %>% trim()
  
  # Lhs/rhs extraction
  tmp1 <- regexpr(pattern=paste0("^", ifStatementPatternStr()), line, ignore.case=TRUE)
  equalSymbolIndex <- attr(tmp1, "match.length")
  lhs <- substring(line, first=1, last=equalSymbolIndex - 1) %>% trim()
  rhs <- substring(line, first=equalSymbolIndex + 1, last=nchar(line)) %>% trim()
  
  # Identify first parenthesis
  tmp2 <- regexpr("^if\\s*\\(", lhs, ignore.case=TRUE)
  firstParenthesisIndex <- attr(tmp2, "match.length")
  
  # Identify variable start
  variableStartIndex <- regexpr(paste0(variablePatternStr(), "$"), lhs) %>% as.integer()
  
  # Identify condition
  conditionWithParentheses <- substring(lhs, first=firstParenthesisIndex, last=variableStartIndex-1) %>% trim()
  condition <- substring(conditionWithParentheses, first=2, last=nchar(conditionWithParentheses) - 1) %>% trim()
  
  # Identify variable
  variable <- substring(lhs, first=variableStartIndex, last=nchar(lhs)) %>% trim()
  
  # Return IF-statement
  return(IfStatement(condition=condition, equation=Equation(variable, rhs=rhs), comment=comment))
}
