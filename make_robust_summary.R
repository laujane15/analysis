## Construct a dataframe with coefficient summaires using both model-based and
## robust standard error estimates.

make_robust_summary <- function(model) {
  # Get out model-based estimates and ses
  sum_tab <- summary(model)$coefficients
  # Get Beta coefficients
  model_coefs <- coef(model)
  # Get Robust SEs
  sand_se <- sqrt(diag(sandwich::sandwich(model)))
  # Calculate P-values and confidence intervals based on the robust SEs
  pvals <- 2 * pnorm(abs(model_coefs / sand_se), lower.tail = FALSE)
  confint <- cbind(model_coefs + qnorm(0.025) * sand_se,
                   model_coefs + qnorm(0.975) * sand_se) %>% exp()
  # Make dataframe with coefficients robust standard errors, CIs, and p-values
  sandwhich_df <- data.frame(
    type = "robust", "estimate" = model_coefs %>% exp(),
    "CI" = confint, "p-value" = pvals)
  # Make the same dataframe, but this time using the model based SEs
  model_df <- data.frame(
    "type" = "model-based",
    "estimate" = sum_tab[, 1] %>% exp(),
    "CI" = cbind(sum_tab[, 1] + qnorm(0.025) * sum_tab[, 2],
                 sum_tab[, 1] + qnorm(0.975) * sum_tab[, 2]) %>% exp(),
    "p-value" = sum_tab[, 4])
  # Combine tables
  final <- bind_rows(sandwhich_df, model_df)
  # Re-order dataframe so it is easier to compare model-base and robust values.
  ordered <- final[order(rownames(final)), ]
  rownames(ordered) <- NULL
  ordered <- cbind("coefficient" = rep(rownames(model_df), each = 2), ordered)
  return(ordered)
}