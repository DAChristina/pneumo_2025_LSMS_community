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
        specimen_id, final_pneumo_decision
      )
    ,
    by = "specimen_id"
  ) %>% 
  glimpse()

ptest_matrix_all <- sumstats::generate_or_chisq_report(df_input = df_epi_coded_eng %>% 
                                                         dplyr::select(-specimen_id,
                                                                       -consent),
                                                       binary_disease = "area")

ptest_matrix_table_report <- sumstats::chisq_fisher_to_2D_table(result = ptest_matrix_all) %>%
  glimpse()

# compile all assessment
df_assessed <- df_epi_coded_eng %>% 
  dplyr::mutate(across(everything(), as.character)) %>% 
  tidyr::pivot_longer(cols = everything(),
                      names_to = "variable", values_to = "value") %>% 
  dplyr::group_by(variable, value) %>% 
  dplyr::summarise(count_all = n(), .groups = "drop") %>% 
  dplyr::mutate(
    percent = round(count_all/nrow(df_epi_coded_eng)*100, 1),
    report_assessed_all = paste0(count_all, " (", percent, "%)")
  ) %>% 
  dplyr::select(-count_all,
                -percent) %>% 
  # view() %>%
  glimpse()

# compile all assessment per-area
df_assessed_area <- df_epi_coded_eng %>% 
  dplyr::mutate(across(-area, as.character)) %>%  
  tidyr::pivot_longer(cols = -area,
                      names_to = "variable", values_to = "value") %>% 
  dplyr::group_by(variable, value, area) %>% 
  dplyr::summarise(count_assessed_perArea = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_coded_eng %>% 
      dplyr::group_by(area) %>% 
      dplyr::summarise(count_area = n(), .groups = "drop")
    ,
    by = "area"
  ) %>% 
  dplyr::mutate(
    percent = round(count_assessed_perArea/count_area*100, 1),
    report_assessed_perArea = paste0(count_assessed_perArea, " (", percent, "%)")
  ) %>% 
  dplyr::select(-count_assessed_perArea,
                -count_area,
                -percent) %>% 
  tidyr::pivot_wider(names_from = area, 
                     values_from = report_assessed_perArea) %>% 
  dplyr::rename(
    report_assessed_Lombok = Lombok,
    report_assessed_Sumbawa = Sumbawa,
    report_assessed_Minahasa = Minahasa,
    report_assessed_Sorong = Sorong,
    
  ) %>% 
  # view() %>%
  glimpse()

compile_assessed_all <- dplyr::left_join(
  df_assessed, df_assessed_area,
  by = c("variable", "value")
) %>% 
  # view() %>% 
  glimpse()

# compiled pneumo positive
df_compiled_positivePneumo <- dplyr::left_join(
  df_epi_coded_eng %>% 
    dplyr::mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value) %>% 
    dplyr::summarise(count_all = n(), .groups = "drop")
  ,
  df_epi_coded_eng %>% 
    dplyr::mutate(across(-final_pneumo_decision, as.character)) %>%
    tidyr::pivot_longer(cols = -final_pneumo_decision, 
                        names_to = "variable", 
                        values_to = "value"
                        ) %>% 
    dplyr::group_by(variable, value, final_pneumo_decision) %>% 
    dplyr::summarise(count_positivePneumo = n(), .groups = "drop") %>% 
    dplyr::filter(final_pneumo_decision == "positive") %>% 
    dplyr::select(-final_pneumo_decision)
  ,
  by = c("variable", "value")
) %>% 
  dplyr::mutate(
    count_positivePneumo = case_when(
      is.na(count_positivePneumo) ~ 0,
      TRUE ~ count_positivePneumo
    ),
    percent = round(count_positivePneumo/count_all*100, 1),
    report_positive_all = paste0(count_positivePneumo, " (", percent, "%)")
  ) %>% 
  dplyr::select(-count_all,
                -count_positivePneumo,
                -percent) %>% 
  # view() %>% 
  glimpse()

# compiled pneumo positive per-area
df_area_positivePneumo <- dplyr::left_join(
  df_epi_coded_eng %>% 
    dplyr::mutate(across(-area, as.character)) %>%
    tidyr::pivot_longer(cols = -area,
                        names_to = "variable",
                        values_to = "value") %>% 
    dplyr::group_by(variable, value, area) %>% 
    dplyr::summarise(count_perArea = n(), .groups = "drop")
  ,
  df_epi_coded_eng %>% 
    dplyr::mutate(across(-c(area, final_pneumo_decision), as.character)) %>%  
    tidyr::pivot_longer(cols = -c(area, final_pneumo_decision),
                        names_to = "variable", values_to = "value") %>% 
    dplyr::group_by(variable, value, area, final_pneumo_decision) %>% 
    dplyr::summarise(count_positivePneumo = n(), .groups = "drop") %>% 
    dplyr::filter(final_pneumo_decision == "positive") %>% 
    dplyr::select(-final_pneumo_decision)
  ,
  by = c("variable", "value", "area")
) %>% 
  dplyr::mutate(
    count_positivePneumo = case_when(
      is.na(count_positivePneumo) ~ 0,
      TRUE ~ count_positivePneumo
    ),
    percent = round(count_positivePneumo/count_perArea*100, 1),
    report_positive_perArea = paste0(count_positivePneumo, " (", percent, "%)")
  ) %>% 
  dplyr::select(-count_perArea,
                -count_positivePneumo,
                -percent) %>% 
  tidyr::pivot_wider(names_from = area, 
                     values_from = report_positive_perArea) %>% 
  dplyr::rename(
    report_positive_Lombok = Lombok,
    report_positive_Sumbawa = Sumbawa,
    report_positive_Minahasa = Minahasa,
    report_positive_Sorong = Sorong
  ) %>% 
  # view() %>% 
  glimpse()

compile_positive_all <- dplyr::left_join(
  df_compiled_positivePneumo, df_area_positivePneumo,
  by = c("variable", "value")
) %>% 
  # view() %>% 
  glimpse()

compile_all_report <- dplyr::left_join(
  compile_assessed_all, compile_positive_all,
  by = c("variable", "value")
) %>% 
  # re-arranged
  dplyr::select(variable, value,
                report_assessed_Lombok, report_positive_Lombok,
                report_assessed_Sumbawa, report_positive_Sumbawa,
                report_assessed_Minahasa, report_positive_Minahasa,
                report_assessed_Sorong, report_positive_Sorong,
                report_assessed_all, report_positive_all) %>% 
  # view() %>% 
  glimpse()

# combine report
compile_all_report_with_pValues <- dplyr::left_join(
  compile_all_report,
  ptest_matrix_table_report,
  by = c("variable"),
  relationship = "many-to-many"
) %>% 
  # dplyr::filter(estimate != 1) %>% # rese amat
  # dplyr::left_join(
  #   ptest_matrix_table_report,
  #   by = c("variable")
  # ) %>% 
  dplyr::distinct(variable, value, .keep_all = T) %>% 
  dplyr::filter(!variable %in% c("specimen_id",
                                 "consent",
                                 "area")
  ) %>% 
  dplyr::mutate(
    variable = recode(variable, !!!var_map),
    arrangement = as.numeric(
      recode(variable, !!!var_arrangement))
  ) %>% 
  arrange(arrangement) %>% 
  # view() %>%
  glimpse()

write.csv(compile_all_report_with_pValues %>% 
            dplyr::select(-arrangement),
          "outputs/out_1carriage_1all_tables.csv",
          row.names = F)
