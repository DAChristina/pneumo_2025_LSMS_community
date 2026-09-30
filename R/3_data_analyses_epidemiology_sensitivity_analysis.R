# sensitivity analysis for vaccinated children

library(tidyverse)
source("global/fun.R")

df_epi_coded_eng <- read.csv("inputs/epiData_eng.csv") %>% 
  dplyr::filter(consent == "yes") %>% 
  dplyr::mutate(
    age_year_3groups = factor(age_year_3groups,
                              levels = c("< 1 year old",
                                         "1-2 years old",
                                         "3-5 years old")),
    area = factor(area,
                  levels = c("Lombok", "Sumbawa", "Minahasa", "Sorong")),
    
    # add vaccination status according to area
    vacc_area = case_when(
      area == "Lombok" | area == "Sumbawa" ~ "vaccinated",
      TRUE ~ "no"
    ),
    vacc_area = factor(vacc_area,
                       levels = c("no", "vaccinated")),
    
    sex = factor(sex,
                 levels = c("female", "male")),
    contact_kindergarten = factor(contact_kindergarten,
                                  levels = c("no", "yes")),
    contact_otherChildren = factor(contact_otherChildren,
                                   levels = c("no", "yes")),
    contact_cigarettes = factor(contact_cigarettes,
                                levels = c("no", "yes")),
    contact_cooking_fuel = factor(contact_cooking_fuel,
                                  levels = c("natural gas",
                                             "kerosene",
                                             "wood")),
    contact_cooking_place = factor(contact_cooking_place,
                                   levels = c("indoor", "outdoor")),
    breastFeed_compiled = factor(breastFeed_compiled,
                                 levels = c("never breastfeed",
                                            "currently breastfeed",
                                            "ever breastfeed")),
    house_roof_regroup = factor(house_roof_regroup,
                                levels = c("clay tile",
                                           "metal sheet", "spandek",
                                           "asbestos", "wood",
                                           "others"      )),
    house_building_regroup = factor(house_building_regroup,
                                    levels = c("brick",
                                               "concrete block",
                                               "bamboo/plywood")),
    house_window_regroup = factor(house_window_regroup,
                                  levels = c("glass/curtain",
                                             "bamboo/wood/open")),
    nTotal_people_regroup = factor(nTotal_people_regroup,
                                   levels = c("1-3 (low)",
                                              "4-6 (moderate)",
                                              ">6 (high)")),
    nTotal_child_5yo_andBelow_regroup = factor(nTotal_child_5yo_andBelow_regroup,
                                               levels = c("0",
                                                          "1-2 children",
                                                          "3-7 children")),
    nTotal_child_5yo_andBelow_sleep_regroup = factor(nTotal_child_5yo_andBelow_sleep_regroup,
                                                     levels = c("0",
                                                                "1-3 children")),
    
    illness_past24h_difficulty_compiled= factor(illness_past24h_difficulty_compiled,
                                                levels= c("no",
                                                          "≥ 1 respiratory illness")),
    vaccination_hibpentavalent_dc_n_regroup = factor(vaccination_hibpentavalent_dc_n_regroup,
                                                     levels = c("0 not yet",
                                                                "vaccinated")),
    vaccination_pcv13_dc_n_regroup = factor(vaccination_pcv13_dc_n_regroup,
                                            levels = c("0 not yet",
                                                       "1-2 mandatory",
                                                       "3-4 booster")),
    healthcareVisit_last_3mo = factor(healthcareVisit_last_3mo,
                                      levels = c("no", "yes")),
    hospitalised_last_3mo = factor(hospitalised_last_3mo,
                                   levels = c("no", "yes")),
    antibiotic_past3days = factor(antibiotic_past3days,
                                  levels = c("no", "yes"))
  ) %>% 
  dplyr::select(
    -age_month,
    -age_year,
    -tribe
  ) %>% 
  dplyr::left_join(
    read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
      dplyr::select(
        specimen_id, final_pneumo_decision,
        serotype_final_decision,
        serotype_classification_PCV13_final_decision
      )
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::rename(
    final_pneumo_decision_all = final_pneumo_decision,
    serotype_classification_PCV13_all = serotype_classification_PCV13_final_decision
  ) %>% 
  glimpse()

epi_cpos <- df_epi_coded_eng %>% 
  dplyr::select(
    # -specimen_id,
    -consent,
    # -vaccination_pcv13_dc_n_regroup, # avoid collinearity
    -vaccination_hibpentavalent_dc_n_regroup, # extreme imbalance in non-vaccinated
    -illness_past3days_fever_regroup,
    -illness_past24h_cough,
    -illness_past24h_runny_nose,
    -illness_past24h_difficulty_breathing
  ) %>% 
  dplyr::left_join(
    read.table("outputs/result_poppunk/qfile_filtered_19to23.txt") %>% 
      dplyr::mutate(specimen_id = V1,
                    workPoppunk_qc = "pass_qc") %>% 
      dplyr::select(specimen_id, workPoppunk_qc)
    ,
    by = "specimen_id"
    
  ) %>%
  
  # dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  dplyr::rename(label = specimen_id) %>%
  dplyr::mutate(serotype_final_decision = factor(serotype_final_decision,
                                                 levels = c(
                                                   # VT
                                                   # "3", "6A/6B", "6A/6B/6C/6D", "serogroup 6",
                                                   # "14", "17F", "18C/18B", "19A", "19F", "23F",
                                                   "1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   # NVT
                                                   # "7C", "10A", "10B", "11A/11D", "13", "15A", "15B/15C",
                                                   # "16F", "19B", "20", "23A", "23B", "24F/24B", "25F/25A/38",
                                                   # "28F/28A", "31", "34", "35A/35C/42", "35B/35D", "35F",
                                                   # "37", "39", "mixed serogroups",
                                                   "serogroup 6", "6C", "7C",
                                                   "10", "10A", "10B", "11A", "13",
                                                   "15A", "15B", "15C","15B/15C", "16F",
                                                   "17F", "18A", "18B", "19B", "20", "20B",
                                                   "21", "23A", "23B", "23B1",
                                                   "24F", "24B/C/F", "24B/24C/24F", "serogroup 24",
                                                   "25B", "25F",
                                                   "28A", "31", "33B", "33G",
                                                   "34", "35A", "35B", "35C", "35F", "37",
                                                   "37F", "38", "39",
                                                   "NT")),
                
                vaccination_status_area = case_when(
                  area == "Lombok" |
                    area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
                  TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
                ),
  ) %>% 
  # factorise all character-based on QC
  dplyr::mutate(
    # # using binomial I only need to factor y
    # final_pneumo_decision = factor(final_pneumo_decision,
    #                                levels = c("negative", "positive"))
    
    # I use poisson instead
    final_pneumo_decision_passQC = ifelse(workPoppunk_qc == "pass_qc", 1, 0),
    
    # redefine serotype_classification_PCV13_final_decision post QC
    serotype_classification_PCV13_postQC = case_when(
      final_pneumo_decision_all == "negative" ~ "negative",
      (final_pneumo_decision_all == "positive" & 
        is.na(workPoppunk_qc)) ~ "unclassified (QC failure)",
      TRUE ~ serotype_classification_PCV13_all
    ),
    
    # redefine VT & NVT using bool
    serotype_classification_PCV13_vt = ifelse(
      serotype_classification_PCV13_postQC == "VT", 1, 0
    ),
    serotype_classification_PCV13_nvt = ifelse(
      serotype_classification_PCV13_postQC == "NVT", 1, 0
    ),
    serotype_classification_PCV13_nt = ifelse(
      serotype_classification_PCV13_postQC == "NT", 1, 0
    ),
    serotype_classification_PCV13_unclassified = ifelse(
      serotype_classification_PCV13_postQC == "unclassified (QC failure)", 1, 0
    ),
  ) %>% 
  glimpse()

# test descriptive using table summary
additional_desc <- epi_cpos %>%
  dplyr::select(
    vaccination_pcv13_dc_n_regroup,
    final_pneumo_decision_all,
    # final_pneumo_decision_passQC,
    # serotype_classification_PCV13_postQC, # would rather use individual values
    serotype_classification_PCV13_vt,
    serotype_classification_PCV13_nvt,
    serotype_classification_PCV13_nt,
    serotype_classification_PCV13_unclassified) %>% 
  gtsummary::tbl_summary(
    by = vaccination_pcv13_dc_n_regroup,
    label = list(
      final_pneumo_decision_all ~ "Overall S. pneumoniae positive",
      serotype_classification_PCV13_vt  ~ "PCV13+6C Vaccine type",
      serotype_classification_PCV13_nvt ~ "PCV13+6C Non-vaccine type",
      serotype_classification_PCV13_nt ~ "Nontypeable",
      serotype_classification_PCV13_unclassified ~ "Positive but unclassified (QC failure)"
    ),
    statistic = gtsummary::all_categorical() ~ "{n} ({p}%)"
  ) %>%
  gtsummary::add_p(test = everything() ~ "chisq.test") %>%
  gtsummary::add_overall() %>%
  gtsummary::modify_header(label ~ "**Outcome**") %>%
  gtsummary::modify_caption("**Table S. pneumoniae by individual PCV13 vaccination status**") %>%
  gtsummary::bold_labels()

additional_desc

# additional_desc %>%
#   gtsummary::as_flex_table() %>%
#   flextable::save_as_docx(path = "outputs/out_1carriage_additional_PCV13_VT.docx")

additional_desc %>%
  as_tibble() %>%
  mutate(across(everything(), ~gsub("\u00A0", " ", .))) %>%
  readr::write_csv("outputs/out_1carriage_additional_PCV13_VT.csv")


# univariable logistic regression for crude RR (VT) ############################
# I modified global/fun.R to include robust SE

# redefine epi_cpos for RR
epi_cpos <- epi_cpos %>% 
  dplyr::select(
    -label,
    -final_pneumo_decision_all,
    -serotype_final_decision,
    -serotype_classification_PCV13_all,
    -workPoppunk_qc,
    -vaccination_status_area,
    -final_pneumo_decision_passQC,
    -serotype_classification_PCV13_postQC,
    
    -serotype_classification_PCV13_nvt,
    -serotype_classification_PCV13_nt,
    -serotype_classification_PCV13_unclassified
  )


rr_univar_all <- generate_univar_pois_report(df_input = epi_cpos %>% 
                                               dplyr::select(where(~ all(!is.na(.)))),
                                             binary_disease = "serotype_classification_PCV13_vt",
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

# prepare formulae for multivariable logistic regression
# GGally::ggpairs(epi_cpos[, c("area", "vaccination_pcv13_dc_n_regroup")])
# strong collinearity between model & vaccination status, I test it to 2 model

# with vacc_area
get_columns_area <- epi_cpos %>%
  dplyr::select(vaccination_pcv13_dc_n_regroup,
                age_year_3groups,
                # area,
                
  ) %>%
  colnames()
formula_a <- as.formula(paste("serotype_classification_PCV13_vt", "~",
                              paste(get_columns_area, collapse = " + ")))
mod_a <- glm(formula_a, family = poisson(link="log"), data = epi_cpos)
# car::vif(mod_a)
robust_se_a <- sandwich::vcovHC(mod_a, type = "HC0",
                                cluster = epi_cpos$area)
model_a <- lmtest::coeftest(mod_a, vcov = robust_se_a)

# with area
get_columns_vacc <- epi_cpos %>%
  # dplyr::select(-serotype_classification_PCV13_vt
  dplyr::select(vaccination_pcv13_dc_n_regroup,
                age_year_3groups,
                area,
                
  ) %>%
  colnames()
formula_v <- as.formula(paste("serotype_classification_PCV13_vt", "~",
                              paste(get_columns_vacc, collapse = " + ")))
mod_v <- glm(formula_v, family = poisson(link="log"), data = epi_cpos)
# car::vif(mod_v)
robust_se_v <- sandwich::vcovHC(mod_v, type = "HC0",
                                cluster = epi_cpos$area)
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
# detect <- glm(formula, family = poisson(link="log"), data = epi_cpos,
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
combined_unimultivar_carr <- dplyr::full_join(
  rr_univar_model_report, rr_multivar_model_report
  ,
  by = c("value" = "multivar_value")
) %>% 
  dplyr::mutate(
    value = stringr::str_remove(value, variable),
    value = ifelse(is.na(value), "(Intercept multivar)", value)
  ) %>% 
  glimpse()

rr_carr <- combined_unimultivar_carr %>% 
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
# overall, all is fine, but Minahasa is the oddball
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

write.csv(combined_unimultivar_carr %>%
            dplyr::mutate(variable = recode(variable, !!!var_map),
                          arrangement = as.numeric(
                            recode(variable, !!!var_arrangement))
            ) %>%
            dplyr::select(arrangement, variable, value,
                          crude_robust_RR_report, crude_robust_significance,
                          multivar_robust_RR_report, multivar_robust_significance
            ) %>%
            arrange(arrangement)
          ,
          "outputs/out_1carriage_additional_PCV13_VT_2RR.csv",
          row.names = F)

# additional multivar per-area
write.csv(rr_multivar_model_report_v %>%
            dplyr::select(multivar_value,
                          multivar_robust_RR_report, multivar_robust_significance,
                          multivar_AIC, multivar_loglik
            )
          ,
          "outputs/out_1carriage_additional_PCV13_VT_2RR_area.csv",
          row.names = F)
