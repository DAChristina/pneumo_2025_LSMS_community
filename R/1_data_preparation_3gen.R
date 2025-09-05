library(tidyverse)

# Data cleaning process for genData ############################################
# Apparently "No Isolat" is based on the first line inside <cat *.fasta>.
# I extract "No Isolat" on terminal; see 0_temporary_script

# BE CAREFUL while inspecting Data WGS_Lombok.xlsx;
# 49 "No Isolat" with joined <location ID> & <participat ID>
# I manually edit these inconsistencies

df_epi_manado <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver5.xlsx",
                                    sheet = "Manado") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "manado",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
# across(everything(), as.character),
# across(everything(), tolower)) %>% 
# dplyr::select(-contains("koding"),-contains("Ya="))

# Detect duplicated IDs
df_epi_manado_duplicated_ids <- df_epi_manado %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()

write.csv(df_epi_manado, "raw_data/temporary_df_epi_manado.csv",
          row.names = F)

df_epi_sorong <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver4.xlsx",
                                    sheet = "Papua") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub("-", "_", SPECIMEN_ID),
                area = "sorong",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
# across(everything(), as.character),
# across(everything(), tolower)) %>% 
# dplyr::select(-contains("koding"),-contains("Ya="))

# Detect duplicated IDs
df_epi_sorong_duplicated_ids <- df_epi_sorong %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()

write.csv(df_epi_sorong, "raw_data/temporary_df_epi_sorong.csv",
          row.names = F)

setdiff(names(df_epi_manado), names(df_epi_sorong))
setdiff(names(df_epi_sorong), names(df_epi_manado))


# just temporary extract specimen_id and area

df_gen_all <- dplyr::left_join(
  read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv") %>% 
    dplyr::select(specimen_id, area) %>% 
    dplyr::mutate(workFasta_name = paste0("Streptococcus_pneumoniae_", specimen_id))
  ,  
  # I manually edit 49 naming inconsistencies and analyse LBK_137 from contigs
  readxl::read_excel("raw_data/Data WGS S. pneumoniae.xlsx") %>% 
    dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
    dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
    dplyr::rename_all(~ paste0("workWGS_", .)) %>% 
    dplyr::rename(
      workWGS_species_pw = workWGS_organism_name,
      workWGS_MLST_pw_ST = workWGS_sequence_type,
      workWGS_MLST_pw_aroe = workWGS_aroe,
      workWGS_MLST_pw_gdh = workWGS_gdh,
      workWGS_MLST_pw_gki = workWGS_gki,
      workWGS_MLST_pw_recp = workWGS_recp,
      workWGS_MLST_pw_spi = workWGS_spi,
      workWGS_MLST_pw_xpt = workWGS_xpt,
      workWGS_MLST_pw_ddl = workWGS_ddl,
      
      workWGS_AMR_pbp1a = workWGS_pbp1a,
      workWGS_AMR_pbp2b = workWGS_pbp2b,
      workWGS_AMR_pbp2x = workWGS_pbp2x,
      workWGS_AMR_chloramphenicol = workWGS_chloramphenicol,
      workWGS_AMR_clindamycin = workWGS_clindamycin,
      workWGS_AMR_erythromycin = workWGS_erythromycin,
      workWGS_AMR_fluoroquinolones = workWGS_fluoroquinolones,
      workWGS_AMR_kanamycin = workWGS_kanamycin,
      workWGS_AMR_linezolid = workWGS_linezolid,
      workWGS_AMR_tetracycline = workWGS_tetracycline,
      workWGS_AMR_trimethoprim = workWGS_trimethoprim,
      workWGS_AMR_sulfamethoxazole = workWGS_sulfamethoxazole,
      workWGS_AMR_cotrimoxazole = workWGS_cotrimoxazole,
      workWGS_AMR_amoxicillin = workWGS_amoxicillin,
      workWGS_AMR_ceftriaxone = workWGS_ceftriaxone,
      workWGS_AMR_cefotaxime = workWGS_cefotaxime,
      workWGS_AMR_cefuroxime = workWGS_cefuroxime,
      workWGS_AMR_meropenem = workWGS_meropenem,
      workWGS_AMR_penicillin = workWGS_penicillin
    ) %>% 
    dplyr::mutate(
      workWGS_serotype_regroup = case_when(
        workWGS_serotype == "03" ~ "3",
        workWGS_serotype == "06A" ~ "6A",
        workWGS_serotype == "06B" ~ "6B",
        workWGS_serotype == "06C" ~ "6C",
        workWGS_serotype == "6E(6B)" ~ "serogroup 6",
        workWGS_serotype == "07C" ~ "7C",
        workWGS_serotype %in% c("alternative_aliB_NT", "untypable", "Untypable") ~ "untypeable",
        TRUE ~ workWGS_serotype
      )
    ) %>%
    # annoying inconsistencies AMR values
    dplyr::mutate(
      across(
        .cols = contains("_AMR_"),
        .fns = ~ .x %>%
          tolower() %>%
          str_replace_all("[_()\\t;\\- ]", "") %>%
          str_trim()
      )
    ) %>% 
    dplyr::mutate(across(
      .cols = contains("AMR"),
      .fns = ~ case_when(
        str_detect(.x, "catpc194") ~ "R (cat_pC194)",
        str_detect(.x, "ermb") ~ "R (ermB)",
        str_detect(.x, "inua") ~ "R (inuA)",
        str_detect(.x, "mefa10") ~ "R (mefA_10)",
        
        str_detect(.x, "parcd83n") ~ "R (parC_D83N)",
        str_detect(.x, "parcs79f") ~ "R (parC_S79F)",
        str_detect(.x, "parcs79y") ~ "R (parC_S79Y)",
        
        str_detect(.x, "tetk") ~ "R (tetK)",
        str_detect(.x, "tetm1") ~ "R (tetM_1)",
        str_detect(.x, "tetm12") ~ "R (tetM_12)",
        str_detect(.x, "tetm13") ~ "R (tetM_13)",
        str_detect(.x, "tetm2") ~ "R (tetM_2)",
        str_detect(.x, "tetm4") ~ "R (tetM_4)",
        str_detect(.x, "tetm8") ~ "R (tetM_8)",
        
        str_detect(.x, "folai100|folai100l") ~ "R (folA_I100L)",
        str_detect(.x, "folpaainsert5770") ~ "R (folP_57-70)",
        str_detect(.x, "folpaainsert5771") ~ "R (folP_57-71)",
        TRUE ~ .x
      ))
    ) %>% 
    dplyr::mutate(across(
      .cols = contains("_AMR_"),
      .fns = ~ case_when(
        # relabel
        str_to_lower(.x) == "sensitive" ~ "S",
        str_to_lower(.x) == "resistant" ~ "R",
        str_to_lower(.x) == "intermediate" ~ "I",
        str_to_lower(.x) %in% c("none", "nf", "nfnf", "-", "", "null", "null/null", "nf/nf", "nfnull/null") ~ "NF",
        
        # compound mappings
        str_to_lower(.x) == "sensitive/intermediate" ~ "S/I",
        str_to_lower(.x) == "sensitive/resistant" ~ "S/R",
        str_to_lower(.x) == "intermediate/resistant" ~ "I/R",
        str_to_lower(.x) == "sensitive/sensitive" ~ "S/S",
        
        # prefix mutation
        # str_detect(.x, "\\(") & !str_detect(.x, "^R \\(") ~ paste0("R ", .x),
        TRUE ~ .x
      ))
    ) %>% 
    # grouping
    dplyr::mutate(
      workWGS_AMR_ceftriaxone_nonMeningitis = case_when(
        is.na(workWGS_AMR_ceftriaxone) ~ NA_character_,
        workWGS_AMR_ceftriaxone == "NF" ~ "NF",
        TRUE ~ sub("/.*", "", workWGS_AMR_ceftriaxone)
      ),
      workWGS_AMR_ceftriaxone_meningitis = case_when(
        is.na(workWGS_AMR_ceftriaxone) ~ NA_character_,
        workWGS_AMR_ceftriaxone == "NF" ~ "NF",
        TRUE ~ sub(".*/", "", workWGS_AMR_ceftriaxone)
      ),
      
      workWGS_AMR_cefotaxime_nonMeningitis = case_when(
        is.na(workWGS_AMR_cefotaxime) ~ NA_character_,
        workWGS_AMR_cefotaxime == "NF" ~ "NF",
        TRUE ~ sub("/.*", "", workWGS_AMR_cefotaxime)
      ),
      workWGS_AMR_cefotaxime_meningitis = case_when(
        is.na(workWGS_AMR_cefotaxime) ~ NA_character_,
        workWGS_AMR_cefotaxime == "NF" ~ "NF",
        TRUE ~ sub(".*/", "", workWGS_AMR_cefotaxime)
      ),
      
      workWGS_AMR_penicillin_nonMeningitis = case_when(
        is.na(workWGS_AMR_penicillin) ~ NA_character_,
        workWGS_AMR_penicillin == "NF" ~ "NF",
        TRUE ~ sub("/.*", "", workWGS_AMR_penicillin)
      ),
      workWGS_AMR_penicillin_meningitis = case_when(
        is.na(workWGS_AMR_penicillin) ~ NA_character_,
        workWGS_AMR_penicillin == "NF" ~ "NF",
        TRUE ~ sub(".*/", "", workWGS_AMR_penicillin)
      ),
      
      workWGS_AMR_class_cephalosporins = case_when(
        workWGS_AMR_ceftriaxone_nonMeningitis == "R" | workWGS_AMR_cefotaxime_nonMeningitis == "R" | workWGS_AMR_cefuroxime == "R" ~ "R",
        workWGS_AMR_ceftriaxone_nonMeningitis == "I" | workWGS_AMR_cefotaxime_nonMeningitis == "I" | workWGS_AMR_cefuroxime == "I" ~ "I",
        workWGS_AMR_ceftriaxone_nonMeningitis == "S" | workWGS_AMR_cefotaxime_nonMeningitis == "S" | workWGS_AMR_cefuroxime == "S" ~ "S",
        TRUE ~ "NF"
      ),
      workWGS_AMR_class_penicillins = case_when(
        workWGS_AMR_penicillin_nonMeningitis == "R" | workWGS_AMR_amoxicillin == "R" ~ "R",
        workWGS_AMR_penicillin_nonMeningitis == "I" | workWGS_AMR_amoxicillin == "I" ~ "I",
        workWGS_AMR_penicillin_nonMeningitis == "S" | workWGS_AMR_amoxicillin == "S" ~ "S",
        TRUE ~ "NF"
      ),
      workWGS_AMR_class_antifolates = case_when( # technically including workWGS_AMR_cotrimoxazole
        workWGS_AMR_trimethoprim == "R (folA_I100L)" & workWGS_AMR_sulfamethoxazole == "R (folP_57-70)" ~ "R (folA_I100L & folP_57-70)",
        workWGS_AMR_trimethoprim == "NF" & workWGS_AMR_sulfamethoxazole == "R (folP_57-70)" ~ "R (folP_57-70)",
        workWGS_AMR_trimethoprim == "R (folA_I100L)" & workWGS_AMR_sulfamethoxazole == "NF" ~ "R (folA_I100L)",
        TRUE ~ "NF"
      ),
      # define MDR flag
      workWGS_AMR_logic_class_amphenicols = str_detect(workWGS_AMR_chloramphenicol, "^R"),
      workWGS_AMR_logic_class_lincosamides = str_detect(workWGS_AMR_clindamycin, "^R"),
      workWGS_AMR_logic_class_macrolides = str_detect(workWGS_AMR_erythromycin, "^R"),
      workWGS_AMR_logic_class_quinolones = str_detect(workWGS_AMR_fluoroquinolones, "^R"),
      workWGS_AMR_logic_class_aminoglycosides = str_detect(workWGS_AMR_kanamycin, "^R"),
      workWGS_AMR_logic_class_oxazolidinones = str_detect(workWGS_AMR_linezolid, "^R"),
      workWGS_AMR_logic_class_tetracyclines = str_detect(workWGS_AMR_tetracycline, "^R"),
      workWGS_AMR_logic_class_carbapenems = str_detect(workWGS_AMR_meropenem, "^R"),
      
      workWGS_AMR_logic_class_cephalosporins = str_detect(workWGS_AMR_class_cephalosporins, "^R"),
      workWGS_AMR_logic_class_penicillins = str_detect(workWGS_AMR_class_penicillins, "^R"),    
      workWGS_AMR_logic_class_antifolates = str_detect(workWGS_AMR_class_antifolates, "^R"),
      
      workWGS_AMR_logic_class_counts = rowSums(across(starts_with("workWGS_AMR_logic_class_")), na.rm = TRUE),
      workWGS_AMR_MDR_flag = case_when(
        workWGS_AMR_logic_class_counts >= 3 ~ "MDR",
        workWGS_AMR_logic_class_counts >= 0 ~ "non-MDR",
        TRUE ~ NA_character_
      )
    )
  ,
  join_by("workFasta_name" == "workWGS_dc_id")
) %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      workWGS_serotype == "03" ~ "3",
      workWGS_serotype == "06A" ~ "6A",
      workWGS_serotype == "6E(6B)" ~ "6B",
      workWGS_serotype == "untypable" ~ "nontypeable",
      workWGS_serotype == "10X" ~ "nontypeable",
      workWGS_serotype == "Swiss_NT" ~ "nontypeable",
      workWGS_serotype == "alternative_aliB_NT" ~ "nontypeable",
      workWGS_serotype == "NCC1_pspK_NESp" ~ "nontypeable",
      workWGS_serotype == "serogroup 24" ~ "24F",
      workWGS_serotype == "24B/24C/24F" ~ "24B/C/F",
      TRUE ~ workWGS_serotype
    )
    # serotype_final_decision = case_when(
    #   serotype_final_decision == "35A/35C/42" ~ "35C",
    #   serotype_final_decision == "serogroup 6" ~ "6B",
    #   serotype_final_decision == "serogroup 24" ~ "24F",
    #   serotype_final_decision == "10X" ~ "nontypeable",
    #   serotype_final_decision == "Swiss_NT" ~ "nontypeable",
    #   workWGS_serotype == "untypable" ~ "nontypeable",
    #   workWGS_serotype == "21" ~ "21",
    #   serotype_final_decision == "15B/15C" ~ "15C",
    #   TRUE ~ serotype_final_decision
    # )
  ) %>% 
  dplyr::mutate(
    serotype_classification_PCV13_final_decision = case_when(
      serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                     "6A", "6B", "9V", "14", "18C",
                                     "19A", "19F", "23F") ~ "VT",
      serotype_final_decision == "nontypeable" ~ "nontypeable",
      TRUE ~ "NVT"
    ),
    serotype_classification_PCV15_final_decision = case_when(
      serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                     "6A", "6B", "9V", "14", "18C",
                                     "19A", "19F", "23F",
                                     "22F", "33F") ~ "VT",
      serotype_final_decision == "nontypeable" ~ "nontypeable",
      TRUE ~ "NVT"
    )
  ) %>% 
  glimpse()

# test serotype list for factors
test_serotypes <- df_gen_all %>% 
  dplyr::select(workWGS_serotype,
                serotype_final_decision,
                serotype_classification_PCV13_final_decision) %>% 
  # view() %>% 
  glimpse()

write.csv(df_gen_all, "inputs/genData_all.csv", row.names = F)


