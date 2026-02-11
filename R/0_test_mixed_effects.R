# load df_epi_coded_eng and epi_cpos first

# prepare formulae for multivariable logistic regression
get_columns <- epi_cpos %>%
  dplyr::select(-final_pneumo_decision) %>%
  colnames()

# test ICC (Intracluster Correlation Coefficient)
# Mixed-effects logistic model
m_icc <- lme4::glmer(final_pneumo_decision ~ 1 + (1|area), 
               data = epi_cpos, 
               family = binomial)

# ICC
icc_value <- performance::icc(m_icc)
icc_value


# Are your assumptions correct?
# Yes:
# ICC = 0.02 → good for island-level
# 500 per island → sufficient
# Baseline prevalence = 0.30 → typical
# Vaccination RR ~ 0.7 → realistic effect size


rr_multivar_model_report_v <- data.frame(
  value   = names(coef(model_v)),
  estimate   = coef(model_v),
  std.error  = model_v[, "Std. Error"],
  robust_RR        = exp(coef(model_v)),
  robust_RR_lower   = exp(coef(model_v) - 1.96 * model_v[, "Std. Error"]),
  robust_RR_upper   = exp(coef(model_v) + 1.96 * model_v[, "Std. Error"]),
  robust_p.value    = model_v[, "Pr(>|z|)"]
) %>%
  dplyr::mutate(
    value = ifelse(value ==  "(Intercept)", "(Intercept multivar)", value),
    robust_RR_report = paste0(round(robust_RR, 2), " (",
                              round(robust_RR_lower, 2), "-",
                              round(robust_RR_upper, 2), ")"),
    robust_significance = case_when(
      robust_p.value < 0.01 ~ "< 0.01",
      robust_p.value >= 0.01 & robust_p.value < 0.05 ~ "< 0.05",
      TRUE ~ as.character(round(robust_p.value, 2))),
    AIC = mod_a$aic,
    loglik = as.numeric(logLik(model_v))
  ) %>% 
  dplyr::arrange(value) %>% 
  dplyr::rename_with(~ paste0("multivar_", .)) %>%
  glimpse()

# combine full report for pneumo carriage
# can be saved to *.csv
combined_unimultivar_carr_v <- dplyr::full_join(
  rr_univar_model_report, rr_multivar_model_report_v
  ,
  by = c("value" = "multivar_value")
) %>% 
  glimpse()


rr_carr_v <- combined_unimultivar_carr %>% 
  dplyr::filter(!value %in% c("(Intercept)", "(Intercept multivar)"),
                crude_robust_p.value %in% c("< 0.01", "< 0.05") |
                  multivar_robust_significance %in% c("< 0.01", "< 0.05"),
                !is.na(multivar_robust_significance)
  ) %>% 
  dplyr::select(value,
                crude_robust_RR_report, crude_robust_significance,
                multivar_robust_RR_report, multivar_robust_significance) %>% 
  glimpse()
