library(tidyverse)
source("global/fun.R")

if (!dir.exists("inputs/supplementary")) {
  dir.create("inputs/supplementary")
}

# Data cleaning process for epiData ############################################
# NAs & "unknown" lookup
read.csv("inputs/epiData.csv") %>%
  dplyr::filter(consent == "yes") %>% 
  dplyr::select(!contains("_last_3mo_"), # too many NAs (>40%)
                -antibiotic_past1mo # not reliable
                ) %>% 
  dplyr::summarise(across(everything(), 
                          ~ sum(.x == "unknown" | is.na(.x), na.rm = TRUE),
                          .names = "n_{.col}")
                   ) %>%
  # transpose
  tidyr::pivot_longer(everything(),
                      names_to = "variable", values_to = "NAs"
                      ) %>%
  dplyr::mutate(variable = str_remove(variable, "^n_")
                ) %>% 
  dplyr::filter(NAs != 0) %>%
  glimpse()

# variable availability
save_var_avail <- read.csv("inputs/epiData.csv") %>%
  dplyr::filter(consent == "yes") %>% 
  dplyr::select(!contains("_last_3mo_")) %>% # too many NAs (>40%)
  dplyr::group_by(area) %>%
  dplyr::summarise(across(everything(), 
                   ~ sum(.x != "unknown" & !is.na(.x), na.rm = TRUE),
                   .names = "n_{.col}")) %>%
  dplyr::ungroup() %>% 
  # transpose
  tidyr::pivot_longer(-area,
                      names_to = "variable", values_to = "n"
                      ) %>%
  dplyr::transmute(Variable = str_remove(variable, "^n_"),
                   Variable = recode(Variable,
                                     !!!var_map
                                     ),
                   Area = factor(str_replace(area, "^[a-z]", str_to_upper),
                                 levels = c("Lombok", "Sumbawa",
                                            "Minahasa", "Sorong")
                                 ),
                   n = n
                   
                ) %>%
  dplyr::filter(Variable != "not used") %>% 
  dplyr::arrange(Variable) %>%
  glimpse()

write.csv(save_var_avail,
          "inputs/supplementary/epiData_variable_availability.csv",
          row.names = F)

# I change epiData from Indonesian to English ##################################
df_epi_coded_eng <- read.csv("inputs/epiData.csv") %>% 
  dplyr::mutate(
    tribe = tolower(tribe)
  ) %>% 
  dplyr::transmute(
    specimen_id = specimen_id,
    consent = consent,
    age_month = age_month,
    age_year = age_year,
    age_year_3groups = age_year_3groups,
    area = str_replace(area, "^[a-z]", str_to_upper),
    area = ifelse(area == "Manado", "Minahasa", area),
    sex = case_when(
      sex == "laki-laki" ~ "male",
      TRUE ~ "female"
    ),
    tribe = tribe,
    contact_kindergarten = contact_kindergarten,
    contact_otherChildren = contact_otherChildren,
    contact_cigarettes = contact_cigarettes,
    contact_cooking_fuel = case_when(
      contact_cooking_fuel == "lpg/gas alam" ~ "natural gas",
      contact_cooking_fuel == "kayu" ~ "wood",
      contact_cooking_fuel == "minyak tanah" ~ "kerosene",
      TRUE ~ contact_cooking_fuel
      ),
    contact_cooking_place = case_when(
      contact_cooking_place == "di dalam rumah" ~ "indoor",
      contact_cooking_place == "di luar rumah" ~ "outdoor",
      ),
    
    breastFeed_compiled = case_when(
      breastMilk_given == "yes" & 
        breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "no" & 
        breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "yes" & 
        breastMilk_still_being_given == "no" ~ "ever breastfeed",
      breastMilk_given == "no" & 
        breastMilk_still_being_given == "no" ~ "never breastfeed"
    ),
    house_roof_regroup = case_when(
      house_roof %in% c("batako", "beton", "genteng logam",
                        "daun palem", "jerami", "kayu") ~ "others", # too little value size (< 10)
      house_roof == "asbes" ~ "asbestos",
      house_roof == "genteng" ~ "clay tile",
      house_roof == "seng" ~ "metal sheet",
      TRUE ~ house_roof # spandek is spandek
    ),
    house_building_regroup = case_when(
      house_building %in% c("batu", "batu bata", 
                            "batu   bata") ~ "brick",
      house_building %in% c("anyaman bambu", "bambu", 
                            "kayu", "triplek") ~ "bamboo/plywood",
      house_building == "batako" ~ "concrete block",
      TRUE ~ house_building
    ),
    house_window_regroup = case_when(
      house_window %in% c("bambu", "kayu", 
                          "tidak ada/terbuka") ~ "bamboo/wood/open",
      house_window == "kaca/tirai" ~ "glass/curtain",
      TRUE ~ house_window
    ),
    nTotal_people_regroup = case_when(
      is.na(nTotal_people) | 
        nTotal_people < 4 ~ "1-3 (low)",
      nTotal_people >= 4 & 
        nTotal_people < 7 ~ "4-6 (moderate)",
      nTotal_people >= 7 ~ ">6 (high)"
    ),
    nTotal_child_5yo_andBelow_regroup = case_when(
      nTotal_child_5yo_andBelow == 0 ~ "0",
      nTotal_child_5yo_andBelow == 1 | 
        nTotal_child_5yo_andBelow == 2 ~ "1-2 children",
      nTotal_child_5yo_andBelow >= 3 ~ "3-7 children"
    ),
    nTotal_child_5yo_andBelow_sleep_regroup = case_when(
      nTotal_child_5yo_andBelow_sleep == 0 ~ "0",
      nTotal_child_5yo_andBelow_sleep >= 1 ~ "1-3 children"
    ),
    illness_past3days_fever_regroup = case_when(
      illness_past3days_fever == "unknown" ~ "no",
      TRUE ~ illness_past3days_fever
    ),
    illness_past3days_fever_nDays_regroup = illness_past3days_fever_nDays,
    illness_past24h_cough = case_when(
      is.na(illness_past24h_cough) ~ "no",
      TRUE ~ illness_past24h_cough
    ),
    illness_past24h_runny_nose = case_when(
      is.na(illness_past24h_runny_nose) ~ "no",
      TRUE ~ illness_past24h_runny_nose
    ),
    illness_past24h_difficulty_breathing = case_when(
      is.na(illness_past24h_difficulty_breathing) ~ "no",
      TRUE ~ illness_past24h_difficulty_breathing
    ),
    illness_past24h_difficulty_compiled = case_when(
      illness_past24h_cough == "no" & 
        illness_past24h_runny_nose == "no" & 
        illness_past24h_difficulty_breathing == "no" ~ "no",
      TRUE ~ "≥ 1 respiratory illness",
    ),
    vaccination_hibpentavalent_dc_n_regroup = case_when(
      vaccination_hibpentavalent_dc_n == 0 ~ "0 not yet",
      vaccination_hibpentavalent_dc_n >= 1 ~ "vaccinated"
        # vaccination_hibpentavalent_dc_n <= 3 ~ "1-3 mandatory",
      # vaccination_hibpentavalent_dc_n >= 4 ~ "4 booster"
    ),
    vaccination_pcv13_dc_n_regroup = case_when(
      vaccination_pcv13_dc_n == 0 ~ "0 not yet",
      vaccination_pcv13_dc_n >= 1 & 
        vaccination_pcv13_dc_n <= 2 ~ "1-2 mandatory",
      vaccination_pcv13_dc_n >= 3 ~ "3-4 booster"
    ),
    # mode imputation because < 1% missing values (n_max = 12)
    healthcareVisit_last_3mo = healthcareVisit_last_3mo,
    hospitalised_last_3mo = case_when(
      hospitalised_last_3mo == "" | 
        hospitalised_last_3mo == "unknown" ~ "no",
      TRUE ~ hospitalised_last_3mo
      ),
    antibiotic_past3days = case_when(
      antibiotic_past3days == "unknown" ~ "no",
      TRUE ~ antibiotic_past3days
      ),
  ) %>% 
  glimpse()


write.csv(df_epi_coded_eng, "inputs/epiData_eng.csv", row.names = F)

