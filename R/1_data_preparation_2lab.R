library(tidyverse)

# Data cleaning process for workLab ############################################
# Generate available fasta list first!
# urgent data preparation for WGS checking

df_workLab_manado_sorong <- dplyr::left_join(
  read.csv("inputs/workLab_data_manado_sorong.csv")
  ,
  read.csv("inputs/test_fasta_names_manado_sorong.csv",
           col.names = "workFasta_name_with_extension") %>% 
    dplyr::mutate(workFasta_file_check = "Accepted_by_DC",
                  specimen_id = gsub("Streptococcus_pneumoniae_", "", workFasta_name_with_extension),
                  specimen_id = gsub(".fasta", "", specimen_id),)
  ,
  by = "specimen_id"
) %>% 
  dplyr::mutate(
    wgs_shipment_date = as.character(wgs_shipment_date),
    workWGS_success_failed = as.character(workWGS_success_failed)
  ) %>% 
  glimpse()
# required: workFasta_file_check & workFasta_name_with_extension

df_workLab_lombok_sumbawa <- read.csv("inputs/workLab_data_lombok_sumbawa.csv") %>% 
  dplyr::mutate(
    wgs_shipment_date = as.character(wgs_shipment_date),
    workWGS_success_failed = as.character(workWGS_success_failed)
  ) %>% 
  glimpse()

df_final_pneumo <- dplyr::bind_rows(
  df_workLab_lombok_sumbawa
  ,
  df_workLab_manado_sorong
) %>% 
  dplyr::mutate(
    final_pneumo_decision = case_when(
      # workWGS_species_pw == "streptococcus pneumoniae" | workWGS_MLST_dc_species == "spneumoniae" ~ "positive",
      # optochin == "s" | optochin == "?" ~ "positive", # "s" can be negative based on WGS result
      workLab_optochin == "?" ~ "positive",
      workLab_optochin == "r" ~ "negative", # correct notes result based on optochin & culture result
      workLab_culture_result == "neg" ~ "negative",
      workLab_culture_suspect == "yes" & workLab_culture_result == "pos" ~ "positive",
      # workLab_culture_suspect == "yes" & workLab_culture_result == "pos" ~ "negative",  # OR "Failed_to_be_extracted"?
      # workLab_culture_suspect == "yes" & workWGS_species_pw != "streptococcus pneumoniae" ~ "negative",  # OR "Failed_to_be_extracted"?
      TRUE ~ workLab_culture_result
    ),
    final_pneumo_decision = case_when(
      final_pneumo_decision == "neg" ~ "negative",
      is.na(final_pneumo_decision) ~ "negative",
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
                  # workWGS_MLST_dc_species, workWGS_species_pw,
                  final_pneumo_decision
  ) %>% 
  dplyr::summarise(count = n()) %>% 
  view() %>%
  glimpse()


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

