library(tidyverse)

# source:
# https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html

df_epi_gen_combined <- dplyr::left_join(
  read.csv("inputs/epiData_eng.csv")
  ,
  read.csv("inputs/genData_all.csv")
  ,
  by = "specimen_id"
) %>% 
  dplyr::filter(!is.na(serotype_classification_PCV13_final_decision)) %>% 
  dplyr::mutate(serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("nontypeable",
                                                                                 "NVT",
                                                                                 "VT"))
                ) %>% 
  dplyr::select(-specimen_id,
                -contains("work"),
                -area.y,
                -serotype_final_decision,
                -serotype_classification_PCV15_final_decision,
                ) %>% 
  glimpse()


# test multinomial with x = epiData and y = VT, NVT, nontypeable output
test_multin <- nnet::multinom(serotype_classification_PCV13_final_decision ~
                                area.x + contact_kindergarten + contact_otherChildren,
                              data = df_epi_gen_combined,
                              model = TRUE)
test_multin

or_univar_multinom_all <- generate_univar_multinom_report(df_input = df_epi_gen_combined,
                                                          multinom_disease = "serotype_classification_PCV13_final_decision")

or_univar_multinom_model_report <- purrr::imap_dfr(or_univar_multinom_all, ~{
  model <- .x$model
  coefficients_df <- broom::tidy(model) %>%
    mutate(variable = .y) %>%
    rename(value = term)
  
  model_stats <- dplyr::tibble(
    variable = .y,
    null_deviance = model$null.deviance,
    residual_deviance = model$deviance,
    df_null = model$df.null,
    df_residual = model$df.residual,
    AIC = model$aic
  )
  # Combine everything into one table
  dplyr::left_join(coefficients_df, model_stats, by = "variable")
}) %>% 
  dplyr::mutate(OR = exp(estimate), # estimate is log(OR)
                OR_lower_CI = exp(estimate - 1.96 * std.error),
                OR_upper_CI = exp(estimate + 1.96 * std.error),
                significance = case_when(
                  p.value < 0.05 ~ "occur",
                  !is.na(p.value) ~ "no",
                  TRUE ~ NA_character_)
  ) %>% 
  dplyr::arrange(variable) %>% 
  dplyr::rename_all(~ paste0("crude_", .)) %>% 
  dplyr::rename(variable = crude_variable,
                value = crude_value) %>% 
  dplyr::mutate(crude_OR_report = paste0(round(crude_OR, 2),
                                         " (", round(crude_OR_lower_CI, 2),
                                         "-", round(crude_OR_upper_CI, 2), ")")) %>% 
  dplyr::select(variable, value, crude_estimate, crude_std.error,
                crude_OR, crude_OR_lower_CI, crude_OR_upper_CI,
                crude_OR_report,
                crude_statistic, crude_p.value, crude_significance,
                crude_null_deviance, crude_residual_deviance,
                crude_df_null, crude_df_residual, crude_AIC) %>% 
  glimpse()



