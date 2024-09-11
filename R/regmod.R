#' Title Regression Goodness of Fit
#'
#' @param x Numeric Value
#'
#' @return  Dataframe of Goodness of Fit
#' @export
#'
#' @examples regdia(model1)
regdiag <- function(x) {
  # Adjusted R-squared
  adj_r_squared <- summary(x)$adj.r.squared

  # AIC and BIC
  aic_value <- AIC(x)
  bic_value <- BIC(x)

  # F-statistics and p-value
  f_stat <- summary(x)$fstatistic
  f_value <- f_stat[1]
  f_p_value <- pf(f_value, f_stat[2], f_stat[3], lower.tail = FALSE)

  # Create a data frame with the results
  diagnostics_df <- data.frame(
    R.Bar = adj_r_squared,
    AIC = aic_value,
    BIC = bic_value,
    F.Statistic = f_value,
    F.Pvalue = f_p_value
  )

  return(diagnostics_df)
}
