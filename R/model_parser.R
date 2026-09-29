#' Parse statements code and return Campsis statements.
#'
#' @param code character vector containing all statements (text form)
#' @return a list of Campsis statements
#'
parse_statements <- function(code) {
  statements <- ModelStatements()

  for (index in seq_along(code)) {
    line <- code[index]
    has_comment <- has_comment(line)
    comment <- as.character(NA)
    if (has_comment) {
      comment <- extract_rhs(line, split = "#") %>% trim()
      line_ <- extract_lhs(line, split = "#")
    } else {
      line_ <- line
    }

    if (is_empty_line(line)) {
      statements <- statements %>% add(LineBreak())
    } else if (is_comment(line)) {
      statements <- statements %>% add(Comment(comment))
    } else if (is_ode(line_)) {
      lhs <- extract_text_between_brackets(line_)
      rhs <- extract_rhs(line_) %>% trim()
      statements <- statements %>% add(Ode(lhs, rhs, comment = comment))
    } else if (is_equation(line_)) {
      lhs <- extract_lhs(line_) %>% trim()
      rhs <- extract_rhs(line_) %>% trim()
      statements <- statements %>% add(Equation(lhs, rhs, comment = comment))
    } else if (is_if_statement(line_)) {
      statements <- statements %>% add(parse_if_statement(line_, comment = comment))
    } else {
      statements <- statements %>% add(UnknownStatement(line_, comment = comment))
    }
  }
  return(statements)
}

#' Parse IF-statement.
#' Assumption: \code{is_if_statement} method already called and returned TRUE.
#'
#' @param line IF-statement as single character string value, comment omitted
#' @param comment any comment, NA by default
#' @return an IF statement object
#'
parse_if_statement <- function(line, comment = as.character(NA)) {
  # Trim input
  line <- line %>% trim()

  # Lhs/rhs extraction
  tmp1 <- regexpr(pattern = paste0("^", if_statement_pattern_str()), line, ignore.case = TRUE)
  equalSymbolIndex <- attr(tmp1, "match.length")
  lhs <- substring(line, first = 1, last = equalSymbolIndex - 1) %>% trim()
  rhs <- substring(line, first = equalSymbolIndex + 1, last = nchar(line)) %>% trim()

  # Identify first parenthesis
  tmp2 <- regexpr("^if\\s*\\(", lhs, ignore.case = TRUE)
  firstParenthesisIndex <- attr(tmp2, "match.length")

  # Identify variable start
  variableStartIndex <- regexpr(paste0(variable_pattern_str(), "$"), lhs) %>% as.integer()

  # Identify condition
  conditionWithParentheses <- substring(lhs, first = firstParenthesisIndex, last = variableStartIndex - 1) %>% trim()
  condition <- substring(conditionWithParentheses, first = 2, last = nchar(conditionWithParentheses) - 1) %>% trim()

  # Identify variable
  variable <- substring(lhs, first = variableStartIndex, last = nchar(lhs)) %>% trim()

  # Return IF-statement
  return(IfStatement(condition = condition, equation = Equation(variable, rhs = rhs), comment = comment))
}
