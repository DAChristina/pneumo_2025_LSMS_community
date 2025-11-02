library(tidyverse)

# Data cleaning process for workLab ############################################
# Generate available fasta list first!
# urgent data preparation for WGS checking

# newest (cleaned) data for labWork (29 October 2025, ver. 6)
# I manually combined the data (negatives were in hidden columns)
read_pos <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver6.xlsx") %>% 
  janitor::clean_names() %>% 
  detoxdats::lowerval_except(., exclude = "specimen_id") %>%
  dplyr::mutate(
    specimen_id = gsub("[ -]", "_", specimen_id), # instead of using " |-"),
    labWork_status = "validated by RFS"
  ) %>%
  glimpse()


# test all data from ver. 6
sheet_all <- readxl::excel_sheets(
  "raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver6.xlsx") %>% 
  glimpse()
sheet_names <- sheet_all[!tolower(sheet_all) %in% "combined"]

bind_all_labWork_data <- data.frame()
for(s in sheet_names){
  read_all <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver6.xlsx",
                                sheet = s) %>% 
    janitor::clean_names() %>% 
    detoxdats::lowerval_except(., exclude = "specimen_id") %>%
    dplyr::mutate(
      specimen_id = gsub("[ -]", "_", specimen_id), # instead of using " |-")
      ) %>% 
    dplyr::select(2:12)
  
  bind_all_labWork_data <- dplyr::bind_rows(bind_all_labWork_data, read_all)
  bind_all_labWork_data
}

# bind all data to see non-validated samples
labWork_all <- dplyr::left_join(
  bind_all_labWork_data
  ,
  read_pos %>% 
    dplyr::select(specimen_id, labWork_status)
  ,
  by = "specimen_id"
) %>% 
  dplyr::mutate(across(where(is.character), ~na_if(.x, "n/a")),
                # validated by DCs
                labWork_status = ifelse(is.na(labWork_status) &
                                          (
                                            # wgs_result == "dna concentration insufficient" |
                                             stringr::str_detect(wgs_result, "streptococcus_pneumoniae_")),
                                        "validated by DC", labWork_status),
                ) %>% 
  # dplyr::filter(is.na(labWork_status) #&
  #                 ) %>%
  distinct(specimen_id, .keep_all = T) %>% 
  view() %>% 
  glimpse()

# personal notes:
# 18 = DNA concentration insufficient
# 









lab_data <- bind_NP_data %>% 
  dplyr::filter(!is.na(id)) %>% 
  dplyr::transmute(
    labWork_id = gsub("[ -]", "_", id), # instead of using " |-"),
    labWork_culture = labWork_culture,
    labWork_checkArea = temporary_area
  ) %>% 
  dplyr::mutate(across(where(is.character), tolower)) %>% 
  glimpse()















df_final_pneumo <- dplyr::bind_rows(
  df_workLab_lombok_sumbawa
  ,
  df_workLab_manado_sorong
  ) %>%
  # cleanup genData first
  dplyr::left_join(
    read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
      dplyr::select(specimen_id, workWGS_species_pw)
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      workWGS_species_pw == "Streptococcus pneumoniae" ~ "positive",
      # optochin == "s" | optochin == "?" ~ "positive", # "s" can be negative based on WGS result
      workLab_optochin == "?" ~ "positive",
      workLab_optochin == "r" ~ "negative", # correct notes result based on optochin & culture result
      workLab_culture_result == "neg" ~ "negative",
      workLab_culture_suspect == "yes" & 
        workLab_culture_result == "pos" ~ "positive",
      # workLab_culture_suspect == "yes" & workLab_culture_result == "pos" ~ "negative",  # OR "Failed_to_be_extracted"?
      # workLab_culture_suspect == "yes" & workWGS_species_pw != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      TRUE ~ workLab_culture_result
    ),
    final_pneumo_decision = case_when(
      final_pneumo_decision == "neg" ~ "negative",
      final_pneumo_decision == "pos" ~ "negative", # negative optochin
      is.na(final_pneumo_decision) ~ "negative",
      workLab_optochin == "s" ~ "positive",
      TRUE ~ final_pneumo_decision  # ensure other values remain unchanged
    )
  ) %>%
  # view() %>% 
  glimpse()

# test area
df_final_pneumo %>% 
  group_by(area, workFasta_file_check) %>% 
  summarise(count = n()) %>% 
  # view() %>% 
  glimpse()

# test group
df_final_pneumo %>% 
  dplyr::group_by(workLab_culture_suspect, workLab_optochin,
                  workLab_culture_result,
                  workLab_culture_result_final, workLab_culture_notes,
                  # workBLAST_lytA_predicted_species,
                  workWGS_species_pw,
                  final_pneumo_decision
  ) %>% 
  dplyr::summarise(count = n()) %>% 
  # view() %>%
  glimpse()

table(df_final_pneumo$final_pneumo_decision)
table(df_final_pneumo$area)
write.csv(df_final_pneumo,
          "inputs/workLab_data.csv",
          row.names = F)

# filter data to available fasta files for manado & sorong:
filtered_data_to_be_shared <- df_final_pneumo %>% 
  dplyr::filter(area %in% c("manado", "sorong"),
                # !is.na(wgs_shipment_date) | final_pneumo_decision == "positive"
                final_pneumo_decision == "positive"
  ) %>% 
  distinct(specimen_id, .keep_all = T) %>% 
  # view() %>% 
  glimpse()

table(filtered_data_to_be_shared$area)
table(filtered_data_to_be_shared$workFasta_file_check)

write.csv(filtered_data_to_be_shared,
          "raw_data/current_fasta_files_manado_sorong_DC_shared_on_20250901.csv",
          row.names = F)


# old supposed to be final checkpoints for all of 4 areas:
write.csv(df_workLab, "inputs/workLab_data_manado_sorong.csv", row.names = F)

# Generate report for fasta files not accepted by DC (yet)
# fasta_missing_report <- df_workLab %>% 
#   dplyr::filter(workWGS_species == "Streptococcus pneumoniae" & is.na(workFasta_file_check)) %>% 
#   view() %>% 
#   glimpse()
# 
# write.csv(fasta_missing_report, "report/temporary_missing_fasta_list_in_DC.csv")
# write.table(fasta_missing_report %>% 
#               dplyr::select(specimen_id),
#             "report/temporary_missing_fasta_list_in_DC.txt",
#             row.names = F, quote = F)

