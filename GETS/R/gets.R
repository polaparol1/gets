
#'  General-to-Specific Regression Procedure
#'
#' @param data
#' @param dependent_variable
#' @param independent_variables
#' @param p_value
#'
#' @return
#' @export
#'
#' @examples
general_to_specific <- function(data, dependent_variable, independent_variables, p_value){
  model_gets <- lm(as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = "+"))), data = data)

  while (TRUE) {
    coef_summary <- summary(model_gets)$coef
    p_values <- coef_summary[, ncol(coef_summary)]
    p_values <- p_values[2:length(p_values)] # without intercept
    max_value <- max(p_values) # value
    max_index <- which.max(p_values) # column name

    if(p_values[max_index] <= p_value){
      break
    }

    independent_variables <- independent_variables[-max_index]
    lm_formula <- paste(dependent_variable, "~", paste(independent_variables, collapse="+"))
    model_gets <- update(model_gets, as.formula(lm_formula))
  }
  return(summary(model_gets))
}
