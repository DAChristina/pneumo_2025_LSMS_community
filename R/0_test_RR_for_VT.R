


```{r, echo = FALSE, message = FALSE, warning = FALSE, results = "hide"}
# load df_epi_gen_pneumo
epi_VTall <- df_epi_coded_eng %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(
        specimen_id, serotype_classification_PCV13_final_decision
      )
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::select(
    -specimen_id,
    -consent,
    -vaccination_pcv13_dc_n_regroup, # avoid collinearity
    -illness_past3days_fever_regroup,
    -illness_past24h_cough,
    -illness_past24h_runny_nose,
    -illness_past24h_difficulty_breathing,
    -final_pneumo_decision
  ) %>% 
  # factorise all character-based
  dplyr::mutate(
    # I use poisson instead
    vt_group = case_when(
      serotype_classification_PCV13_final_decision == "VT" ~ 1,
      TRUE ~ 0
      )
  ) %>% 
  dplyr::select(
    -serotype_classification_PCV13_final_decision
  ) %>% 
  glimpse()


# univariable logistic regression for crude RR
# I modified global/fun.R to include robust SE
rr_univar_all <- generate_univar_pois_report(df_input = epi_VTall %>% 
                                               dplyr::select(where(~ all(!is.na(.)))),
                                             binary_disease = "vt_group",
                                             cluster = "area")

rr_univar_model_report <- purrr::imap_dfr(rr_univar_all, ~{
  model <- .x$model
  robust <- .x$robust
  
  # Extract coefficients + robust values
  coef_df <- broom::tidy(model) %>%
    mutate(
      variable = .y,
      robust_estimate = robust[,1],
      robust_se = robust[,2],
      robust_p.value = robust[,4],
      
      robust_RR = exp(robust_estimate), # estimate is log(OR)
      robust_RR_lower = exp(robust_estimate - 1.96*robust_se),
      robust_RR_upper = exp(robust_estimate + 1.96*robust_se),
      robust_RR_report = paste0(round(robust_RR,2)," (",
                                round(robust_RR_lower,2),"-",
                                round(robust_RR_upper,2),")"),
      robust_significance = case_when(
        robust_p.value < 0.01 ~ "< 0.01",
        robust_p.value >= 0.01 & robust_p.value < 0.05 ~ "< 0.05",
        TRUE ~ as.character(round(robust_p.value, 2)))
    )
  
  # Add model-level statistics TO EVERY ROW
  coef_df %>%
    mutate(
      null_deviance = model$null.deviance,
      residual_deviance = model$deviance,
      df_null = model$df.null,
      df_residual = model$df.residual,
      AIC = model$aic,
      loglik = as.numeric(logLik(model))
    )
}) %>% 
  # calculating RR
  dplyr::mutate(RR = exp(estimate), # estimate is log(OR)
                RR_lower = exp(estimate - 1.96*std.error),
                RR_upper = exp(estimate + 1.96*std.error),
                RR_report = paste0(round(RR,2)," (",
                                   round(RR_lower,2),"-",
                                   round(RR_upper,2),")"),
                significance = case_when(
                  p.value < 0.01 ~ "< 0.01",
                  p.value >= 0.01 & p.value < 0.05 ~ "< 0.05",
                  TRUE ~ as.character(round(p.value, 2)))
  ) %>%
  dplyr::arrange(variable) %>%
  dplyr::rename_all(~ paste0("crude_", .)) %>%
  dplyr::rename(variable = crude_variable,
                value = crude_term) %>%
  # # compare ori & robust side-by-side
  dplyr::select(variable, value, crude_estimate, crude_robust_estimate,
                crude_std.error, crude_robust_se,
                crude_RR, crude_robust_RR,
                crude_RR_lower, crude_robust_RR_lower,
                crude_RR_upper, crude_robust_RR_upper,
                crude_RR_report, crude_robust_RR_report,
                crude_p.value, crude_robust_p.value,
                crude_significance, crude_robust_significance,
                crude_statistic, crude_null_deviance, crude_residual_deviance,
                crude_df_null, crude_df_residual, crude_AIC, crude_loglik) %>%
  glimpse()

```





```{r, echo = FALSE, message = FALSE, warning = FALSE, results = "hide"}
# load df_epi_gen_pneumo and epi_VTall first

# prepare formulae for multivariable logistic regression
# GGally::ggpairs(epi_VTall[, c("area", "vaccination_pcv13_dc_n_regroup")])
# strong collinearity between model & vaccination status, I test it to 2 model

# with vacc_area
get_columns_area <- epi_VTall %>%
  dplyr::select(-vt_group,
                -area
  ) %>%
  colnames()
formula_a <- as.formula(paste("vt_group", "~",
                              paste(get_columns_area, collapse = " + ")))
mod_a <- glm(formula_a, family = poisson(link="log"), data = epi_VTall)
# car::vif(mod_a)
robust_se_a <- sandwich::vcovHC(mod_a, type = "HC0",
                                cluster = epi_VTall$area)
model_a <- lmtest::coeftest(mod_a, vcov = robust_se_a)

# with area
get_columns_vacc <- epi_VTall %>%
  dplyr::select(-vt_group
  ) %>%
  colnames()
formula_v <- as.formula(paste("vt_group", "~",
                              paste(get_columns_vacc, collapse = " + ")))
mod_v <- glm(formula_v, family = poisson(link="log"), data = epi_VTall)
# car::vif(mod_v)
robust_se_v <- sandwich::vcovHC(mod_v, type = "HC0",
                                cluster = epi_VTall$area)
model_v <- lmtest::coeftest(mod_v, vcov = robust_se_v)

#  test AIC
# not much difference but I would rather choose model_a
AIC(model_a, model_v)

# other calculations showed good results:
# print(performance::check_model(model_a))
# Poisson with robust SE, overdispersion becomes irrelevant, I check it anyway!
# model are not overdispersed (< 1.5)
sum(residuals(mod_a, type="pearson")^2) / df.residual(mod_a)

# Check multicollinearity (VIF), see GVIF^(1/(2*Df)) column
# < 3 → perfect, no collinearity
# 3–5 → moderate but acceptable
# > 5 → problematic
# > 10 → very serious collinearity
car::vif(mod_a)

# check separation (complete or quasi-complete separation)
# no separation found
# library(detectseparation)
# detect <- glm(formula, family = poisson(link="log"), data = epi_VTall,
#               method = "detect_separation")

# cook's distance
# some data points  are anomalies
cooksd <- cooks.distance(mod_a)
# plot(cooksd, type="h")
which(cooksd > 4/length(cooksd))

# compile report (I have tested them using model, model_a and model_v)
rr_multivar_model_report <- data.frame(
  value   = names(coef(model_a)),
  estimate   = coef(model_a),
  std.error  = model_a[, "Std. Error"],
  robust_RR        = exp(coef(model_a)),
  robust_RR_lower   = exp(coef(model_a) - 1.96 * model_a[, "Std. Error"]),
  robust_RR_upper   = exp(coef(model_a) + 1.96 * model_a[, "Std. Error"]),
  robust_p.value    = model_a[, "Pr(>|z|)"]
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
    loglik = as.numeric(logLik(model_a))
  ) %>% 
  dplyr::arrange(value) %>% 
  dplyr::rename_with(~ paste0("multivar_", .)) %>%
  glimpse()

# combine full report for pneumo carriage
# can be saved to *.csv
combined_unimultivar_vtall <- dplyr::full_join(
  rr_univar_model_report, rr_multivar_model_report
  ,
  by = c("value" = "multivar_value")
) %>% 
  dplyr::mutate(
    value = stringr::str_remove(value, variable),
    value = ifelse(is.na(value), "(Intercept multivar)", value)
  ) %>% 
  glimpse()

rr_vtall <- combined_unimultivar_vtall %>% 
  dplyr::filter(!value %in% c("(Intercept)", "(Intercept multivar)"),
                crude_robust_p.value %in% c("< 0.01", "< 0.05") |
                  multivar_robust_significance %in% c("< 0.01", "< 0.05"),
                !is.na(multivar_robust_significance)
  ) %>% 
  # aRR = 0.72 (95% CI: 0.58–0.90; p = 0.01)
  dplyr::mutate(
    crude_robust_RR_report_text = ifelse(crude_robust_p.value < 0.05,
                                         paste0("cRR = ", round(crude_robust_RR, 2),
                                                " (95% CI: ", round(crude_robust_RR_lower, 2),
                                                "-", round(crude_robust_RR_upper, 2),
                                                "; p ", crude_robust_significance,
                                                ")"
                                         ),
                                         paste0("cRR = ", round(crude_robust_RR, 2),
                                                " (95% CI: ", round(crude_robust_RR_lower, 2),
                                                "-", round(crude_robust_RR_upper, 2),
                                                "; p = ", crude_robust_significance,
                                                ")"
                                         )
    ),
    multivar_robust_RR_report_text = ifelse(multivar_robust_p.value < 0.05,
                                            paste0("aRR = ", round(multivar_robust_RR, 2),
                                                   " (95% CI: ", round(multivar_robust_RR_lower, 2),
                                                   "-", round(multivar_robust_RR_upper, 2),
                                                   "; p ", multivar_robust_significance,
                                                   ")"
                                            ),
                                            paste0("aRR = ", round(multivar_robust_RR, 2),
                                                   " (95% CI: ", round(multivar_robust_RR_lower, 2),
                                                   "-", round(multivar_robust_RR_upper, 2),
                                                   "; p = ", multivar_robust_significance,
                                                   ")"
                                            )
    ),
  ) %>% 
  dplyr::select(variable, value,
                crude_robust_RR_report_text, crude_robust_significance,
                multivar_robust_RR_report_text, multivar_robust_significance) %>% 
  glimpse()

# I store model_v result in case comparisons between area wanna be justified
# overall, all is fine, but Manado is the oddball
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
  dplyr::filter(stringr::str_detect(multivar_value, "area")) %>% 
  dplyr::mutate(
    multivar_robust_RR_report_text = ifelse(multivar_robust_p.value < 0.05,
                                            paste0("aRR = ", round(multivar_robust_RR, 2),
                                                   " (95% CI: ", round(multivar_robust_RR_lower, 2),
                                                   "-", round(multivar_robust_RR_upper, 2),
                                                   "; p ", multivar_robust_significance,
                                                   ")"
                                            ),
                                            paste0("aRR = ", round(multivar_robust_RR, 2),
                                                   " (95% CI: ", round(multivar_robust_RR_lower, 2),
                                                   "-", round(multivar_robust_RR_upper, 2),
                                                   "; p = ", multivar_robust_significance,
                                                   ")"
                                            )
    ),
  ) %>% 
  glimpse()


```






```{r, echo = FALSE, message = FALSE, warning = FALSE}
DT::datatable(combined_unimultivar_vtall %>% 
                dplyr::select(variable, value,
                              crude_robust_RR_report, crude_robust_significance,
                              multivar_robust_RR_report, multivar_robust_significance
                )
              ,
              filter = "top",
              options = list(
                dom = 'Bfrtip',  # 'B' enables buttons, 'frtip' keeps search/filtering
                buttons = list('colvis'),  # column visibility toggle
                pageLength = 100,
                lengthMenu = list(c(5, 10, 25, -1),
                                  c("5 rows", "10 rows", "25 rows", "All"))
              ),
              extensions = 'Buttons')
```