library(tidyverse)

# Data cleaning process for workLab ############################################
# Generate available fasta list first!

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
                                sheet = "combined") %>% 
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
  dplyr::left_join(
    read.csv("inputs/blastData_all.csv") %>% 
      dplyr::select(file_name, workBLAST_species_decision)
    ,
    by = c("specimen_id" = "file_name")
  ) %>% 
  dplyr::mutate(across(where(is.character), ~na_if(.x, "n/a")),
                
                # validated by DCs
                labWork_status = case_when(
                  is.na(labWork_status) &
                    !is.na(workBLAST_species_decision)
                  ~ "validated by DC",
                  
                  # based on data validation 13 October 2025
                  is.na(labWork_status) & 
                    (s_pneumoniae_suspect_culture_colony == "yes" &
                       s_pneumoniae_culture_result == "pos" &
                       stringr::str_detect(notes, "green") &
                       optochin == "s")|
                    (wgs_shipment_date == "dna concentration insufficient" |
                       wgs_result_12 == "dna concentration insufficient" |
                       wgs_result == "dna concentration insufficient")
                  ~ "positive but low DNA concentration, failed recultivation, validated by RFS",
                  
                  (labWork_status == "validated by RFS") &
                    (workBLAST_species_decision == "pneumo, need verification")
                  ~ "positive, validated by DC & RFS",
                  
                  is.na(labWork_status) & 
                    (s_pneumoniae_suspect_culture_colony == "no" |
                    s_pneumoniae_culture_result == "neg" |
                    stringr::str_detect(notes, "white|no") |
                      optochin == "r")
                  ~ "negative, validated by DC & RFS",
                  
                  TRUE ~ labWork_status
                  )
                ) %>% 
  # revalidate all positive samples with BLAST result
  dplyr::mutate(
    labWork_status = case_when(
      (labWork_status == "validated by DC" |
         labWork_status == "validated by RFS") &
        (workBLAST_species_decision == "pneumo" |
           workBLAST_species_decision == "non-capsulated pneumo")
      ~ "positive, validated by DC & RFS",

      # based on data validation 13-14 October 2025
      (labWork_status == "validated by RFS") &
        (wgs_result == "dna concentration insufficient (failed for wgs)") |
        (s_pneumoniae_suspect_culture_colony == "yes" &
           s_pneumoniae_culture_result == "pos" &
           stringr::str_detect(notes, "green") &
           optochin == "s" &
           is.na(workBLAST_species_decision))
      ~ "positive but low DNA concentration, failed recultivation, validated by RFS",

      (labWork_status == "validated by RFS" |
         labWork_status == "validated by DC") &
        (workBLAST_species_decision == "not pneumo")
      ~ "positive on lab, but not pneumo by BLAST, validated by DC & RFS",

      TRUE ~ labWork_status
      )
    )%>% 
    
    # revalidate ongoing WGS at BRIN
    dplyr::mutate(
      labWork_status = case_when(
        (labWork_status == "positive but low DNA concentration, failed recultivation, validated by RFS") &
          (wgs_result == "on going wgs at brin")
        ~ "positive but on going wgs at brin, validated by RFS",
        TRUE ~ labWork_status
      )
  ) %>%
  
  # determine final pneumo positivity based on cultivation, optochin & BLAST
  dplyr::mutate(
    final_pneumo_decision = case_when(
      labWork_status == "negative, validated by DC & RFS" |
        labWork_status == "positive on lab, but not pneumo by BLAST, validated by DC & RFS"
      ~ "negative",
      TRUE ~ "positive"
    )
  ) %>%
  distinct(specimen_id, .keep_all = T) %>% 
  # view() %>%
  glimpse()

# extract weird samples
labWork_all %>% 
  dplyr::filter(
    is.na(labWork_status)
  ) %>% 
  # view() %>% 
  glimpse()

# test labWork_status
labWork_all %>% 
  dplyr::filter(
    !labWork_status %in% c("positive, validated by DC & RFS",
                        "negative, validated by DC & RFS",
                        "positive but low DNA concentration, failed recultivation, validated by RFS",
                        "positive but on going wgs at brin, validated by RFS",
                        "positive on lab, but not pneumo by BLAST, validated by DC & RFS"
                        )
  ) %>% 
  # view() %>%
  glimpse()

table(labWork_all$labWork_status, useNA = "always")

# test final_pneumo_decision
table(labWork_all$final_pneumo_decision, useNA = "always")


write.csv(labWork_all, "inputs/workLab_data.csv", row.names = F)
