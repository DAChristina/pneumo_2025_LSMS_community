library(tidyverse)

# Data cleaning process for genData ############################################
# BE CAREFUL while inspecting Data WGS_Lombok.xlsx;
# 49 "No Isolat" with joined <location ID> & <participat ID>
# I manually edit these inconsistencies

# extract specimen_id and area #################################################
# combine previously compiled PW data
# I manually edit 49 naming inconsistencies and duplicate values

df_gen_all <- dplyr::right_join(
  read.csv("inputs/epiData_eng.csv") %>% 
    dplyr::select(specimen_id, area) %>% 
    dplyr::mutate(workFasta_name = paste0("Streptococcus_pneumoniae_", specimen_id))
  ,  
  dplyr::bind_rows(
    read.csv("inputs/final_df_genData_compiled.csv", check.names = FALSE)
    ,
    dplyr::bind_rows(
      # currently sequenced data (31/10/2025)
      dplyr::bind_rows(
        read.csv("raw_data/results_SOQ.csv") %>% 
          dplyr::mutate(across(everything(), as.character))
        ,
        read.csv("raw_data/results.csv") %>%
          dplyr::filter(!stringr::str_detect(Sample_ID, "_SWB_")) %>%  # SWB is hospital data
          dplyr::mutate(across(everything(), as.character))
      ) %>% 
        dplyr::transmute(
          No = NA_integer_,
          `Process Date` = as.character("30/10/2025"),
          `No Isolat` = NA_character_,
          dc_id = paste0("Streptococcus_penumoniae_", Sample_ID),
          `Organism name` = ifelse(S.Pneumo_. >= 50, "Streptococcus pneumoniae",
                                   "others"),
          `Genome length` = as.double(Assembly_Length),
          `GC content` = NA_character_,
          Serotype = Serotype,
          `Sequence Type` = ST,
          `GPSC Strain` = GPSC,
          aroE = aroE,
          gdh = gdh,
          gki = gki,
          recP = recP,
          spi = spi,
          xpt = xpt,
          ddl = ddl,
          PBP1a = pbp1a,
          PBP2b = pbp2b,
          PBP2x = pbp2x,
          Chloramphenicol = CHL_Res,
          Clindamycin = CLI_Res,
          Erythromycin = ERY_Res,
          Fluoroquinolones = FQ_Res,
          Kanamycin = KAN_Res,
          Linezolid = NA_character_,
          Tetracycline = TET_Res,
          Trimethoprim = TMP_Res,
          Sulfamethoxazole = SMX_Res,
          `Co-Trimoxazole` = COT_Res,
          Amoxicillin = AMO_Res,
          Ceftriaxone = CFT_Res.Non.meningital.,
          Cefotaxime = TAX_Res.Non.meningital.,
          Cefuroxime = CFX_Res,
          Meropenem = MER_Res,
          Penicillin = PEN_Res.Non.meningital.
        )
    )
  ) %>% 
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
        
        # newly generated data from GPS pipeline
        str_detect(.x, "resistantcatq") ~ "R (cat_q)",
        str_detect(.x, "rmefa") ~ "R (mefA_10)",
        str_detect(.x, "rtetm5") ~ "R (tetM_5)",
        str_detect(.x, "rtetm") ~ "R (tetM)",
        str_detect(.x, "resistanttet32") ~ "R (tet_32)",
        str_detect(.x, "rfolp5770") ~ "R (folP_57-70)",
        TRUE ~ .x
      ))
    ) %>% 
    dplyr::mutate(across(
      .cols = contains("_AMR_"),
      .fns = ~ case_when(
        # relabel
        str_to_lower(.x) == "sensitive" |
          str_to_lower(.x) == "s" ~ "S",
        str_to_lower(.x) == "resistant" |
          str_to_lower(.x) == "r" ~ "R",
        str_to_lower(.x) == "intermediate"  |
          str_to_lower(.x) == "i" ~ "I",
        str_to_lower(.x) %in% c("none", "nf", "nfnf", "-", "", "null", "null/null", "nf/nf", "nfnull/null") ~ "NF",
        
        # compound mappings
        str_to_lower(.x) == "sensitive/intermediate" |
          str_to_lower(.x) == "s/i"~ "S/I",
        str_to_lower(.x) == "sensitive/resistant"  |
          str_to_lower(.x) == "s/r"~ "S/R",
        str_to_lower(.x) == "intermediate/resistant"  |
          str_to_lower(.x) == "i/r"~ "I/R",
        str_to_lower(.x) == "sensitive/sensitive"  |
          str_to_lower(.x) == "s/s"~ "S/S",
        
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
      workWGS_AMR_logic_class_chloramphenicol = str_detect(workWGS_AMR_chloramphenicol, "^R"),
      workWGS_AMR_logic_class_clindamycin = str_detect(workWGS_AMR_clindamycin, "^R"),
      workWGS_AMR_logic_class_erythromycin = str_detect(workWGS_AMR_erythromycin, "^R"),
      workWGS_AMR_logic_class_fluoroquinolones = str_detect(workWGS_AMR_fluoroquinolones, "^R"),
      workWGS_AMR_logic_class_kanamycin = str_detect(workWGS_AMR_kanamycin, "^R"),
      workWGS_AMR_logic_class_linezolid = str_detect(workWGS_AMR_linezolid, "^R"),
      workWGS_AMR_logic_class_tetracycline = str_detect(workWGS_AMR_tetracycline, "^R"),
      workWGS_AMR_logic_class_meropenem = str_detect(workWGS_AMR_meropenem, "^R"),
      
      workWGS_AMR_logic_class_cephalosporins = str_detect(workWGS_AMR_class_cephalosporins, "^R"),
      workWGS_AMR_logic_class_penicillins = str_detect(workWGS_AMR_class_penicillins, "^R"),    
      workWGS_AMR_logic_class_antifolates = str_detect(workWGS_AMR_class_antifolates, "^R"),
      
      workWGS_AMR_logic_class_counts = rowSums(across(starts_with("workWGS_AMR_logic_class_")), na.rm = TRUE),
      workWGS_AMR_MDR_flag = case_when(
        workWGS_AMR_logic_class_counts >= 3 ~ "MDR",
        workWGS_AMR_logic_class_counts >= 0 ~ "non-MDR",
        # workWGS_AMR_logic_class_counts == 0 ~ "non-AMR",
        TRUE ~ NA_character_
      )
    )
  ,
  join_by("workFasta_name" == "workWGS_dc_id")
) %>% 
  dplyr::filter(!is.na(area)) %>% 
  dplyr::mutate(
    serotype_final_decision = case_when( # final decision by capsule/serology
      workWGS_serotype == "03" ~ "3",
      workWGS_serotype == "06A" |
        workWGS_serotype == "06A(06A-III)" ~ "6A",
      workWGS_serotype == "06B" ~ "6B",
      workWGS_serotype == "6E(6B)" ~ "6B", # 6E with 6B-like capsule
      workWGS_serotype == "06C" ~ "6C",
      workWGS_serotype == "06C" ~ "6C",
      workWGS_serotype == "07C" ~ "7C",
      workWGS_serotype == "10X" ~ "33G", # validated with newest version in Pathogenwatch
      # workWGS_serotype == "15B/15C" ~ "15B/C",
      workWGS_serotype == "19F(19AF)" ~ "19A", # note: hybrid 19F with 19A-like capsule https://pmc.ncbi.nlm.nih.gov/articles/PMC4547413/
      workWGS_serotype == "untypable" | 
        workWGS_serotype == "untypeable" |
        workWGS_serotype == "Untypable" |
        workWGS_serotype == "_" |
        workWGS_serotype == "Swiss_NT" |
        workWGS_serotype == "alternative_aliB_NT" |
        workWGS_serotype == "NCC1_pspK_NESp" |
        workWGS_serotype == "NCC1_pspK_non_encapsulated" ~ "NT",
      workWGS_serotype == "24B/24C/24F" ~ "serogroup 24", # 24C: new capsule https://pmc.ncbi.nlm.nih.gov/articles/PMC8218768/
      is.na(workWGS_serotype) ~ NA,
      TRUE ~ workWGS_serotype
    )
  ) %>% 
  dplyr::mutate(
    serotype_classification_PCV13_final_decision = case_when(
      serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                     "6A", "6B", "9V", "14", "18C",
                                     "19A", "19F", "23F") ~ "VT",
      serotype_final_decision == "NT" ~ "NT",
      is.na(serotype_final_decision) ~ NA,
      TRUE ~ "NVT"
    ),
    serotype_classification_PCV15_final_decision = case_when(
      serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                     "6A", "6B", "9V", "14", "18C",
                                     "19A", "19F", "23F",
                                     "22F", "33F") ~ "VT",
      serotype_final_decision == "NT" ~ "NT",
      is.na(serotype_final_decision) ~ NA,
      TRUE ~ "NVT"
    )
  ) %>% 
  # test duplicates
  # group_by(workFasta_name) %>% 
  # summarise(count = n()) %>%
  # filter(count > 1) %>%
  distinct(specimen_id, .keep_all = T) %>% 
  
  # check all available contigs & missing WGS files
  dplyr::full_join(
    read.csv("raw_data/all_fasta_contigs_compiled.csv", header = F) %>% 
      dplyr::rename(workContigs_name = V1) %>% 
      dplyr::mutate(specimen_id = stringr::str_remove(workContigs_name,
                                                      ".fasta"),
                    workContigs_report = "available with DC")
    ,
    by = "specimen_id"
  ) %>% 
  # check all available alignments
  dplyr::full_join(
    read.csv("raw_data/all_fasta_alignments_compiled.csv", header = F) %>% 
      dplyr::rename(workAlignments_name = V1) %>% 
      dplyr::mutate(specimen_id = stringr::str_remove(workAlignments_name,
                                                      ".fasta"),
                    workAlignments_report = "available with DC")
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::mutate(
    workContigs_report = case_when(
      is.na(workContigs_report) &
        workAlignments_report == "available with DC"
      ~ "but alignments available",
      TRUE ~ workContigs_report
    )
  ) %>% 
  dplyr::select(-area) %>% 
  # rejoined by area
  dplyr::left_join(
    read.csv("inputs/epiData_eng.csv") %>% 
      dplyr::select(specimen_id, area)
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::left_join(
    read.csv("inputs/blastData_all.csv")
    ,
    by = c("specimen_id" = "file_name")
  ) %>%
  
  # combine CC data based on goeBURST output (SLV)
  dplyr::left_join(
    read.csv("outputs/phyloviz_goeBURST_output_table.csv") %>%
      dplyr::mutate(ST = as.character(ST),
                    workWGS_CC = as.character(CC)
      )
    ,
    by = c("workWGS_MLST_pw_ST" = "ST")
  ) %>%
  dplyr::mutate(
    workWGS_CC = ifelse(is.na(workWGS_CC), "not assigned", workWGS_CC)
  ) %>% 
  
  # combine with new cluster
  dplyr::left_join(
    read.csv("outputs/result_poppunk/reclassify_cluster_db_v12/reclassify_cluster_db_v12_external_clusters.csv")
    ,
    by = c("specimen_id" = "sample")
  ) %>%
  
  # combine with Salma's work
  dplyr::left_join(
    readxl::read_excel("raw_data/Result_NT_GPS.xlsx") %>%
      dplyr::transmute(
        sample = Sample_ID,
        GPSC_salma = ifelse(GPSC == "NA" | GPSC == "_", NA_character_, GPSC)
      ) %>% 
      dplyr::distinct(sample, .keep_all = TRUE) # weird duplicated ID
    ,
    by = c("specimen_id" = "sample")
  ) %>%
  
  dplyr::mutate(
    CC = ifelse(is.na(CC), "not assigned", CC),
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned", as.character(GPSC),
                                 workWGS_gpsc_strain),
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned" | is.na(workWGS_gpsc_strain), as.character(GPSC_salma),
                                 workWGS_gpsc_strain),
    
    # readjust GPSC name based on the latest version
    workWGS_gpsc_strain = case_when(
      workWGS_gpsc_strain == "Not assigned" | is.na(workWGS_gpsc_strain) ~ "not assigned",
      workWGS_gpsc_strain == "904;9" ~ "9;904",
      workWGS_gpsc_strain == "994;18" ~ "18;994",
      TRUE ~ workWGS_gpsc_strain
    ),
  ) %>%
  dplyr::select(
    -CC,
    -GPSC,
    -GPSC_salma
  ) %>% 
  glimpse()

# check unavailable contigs IDs (fasta alignment available)
df_gen_all %>% 
  dplyr::filter(is.na(workContigs_report)) %>% 
  dplyr::select(-contains("AMR")) %>% 
  # view() %>% 
  glimpse()

# check species final decision
df_gen_all %>% 
  dplyr::select(specimen_id,
                workWGS_species_pw,
                workBLAST_species_decision,
                workWGS_serotype,
                serotype_final_decision
  ) %>% 
  # view() %>%
  glimpse()


# test serotype list for factors ###############################################
df_gen_all %>% 
  filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::select(workWGS_serotype,
                serotype_final_decision,
                serotype_classification_PCV13_final_decision) %>% 
  # view() %>% 
  glimpse()

# sanity check for area and species
df_gen_all %>% 
  # filter(!is.na(workWGS_species_pw)) %>% 
  group_by(area) %>% 
  summarise(count = n()) %>% 
  glimpse()

df_gen_all %>%
  filter(workWGS_species_pw == "Streptococcus pneumoniae") %>%
  group_by(area) %>%
  summarise(count = n()) %>%
  glimpse()

df_gen_all %>% 
  group_by(workWGS_species_pw) %>% 
  summarise(count_sp = n()) %>% 
  # view() %>% 
  glimpse()

# store genData_old.csv & rewrite genData_all.csv 
write.csv(df_gen_all, "inputs/genData_all_adjusted_gpsc.csv", row.names = F)


# combine epiData with genData
df_epi_gen_pneumo <- dplyr::left_join(
  read.csv("inputs/epiData_eng.csv")
  ,
  read.csv("inputs/workLab_data.csv") %>% 
    dplyr::select(specimen_id,
                  labWork_status,
                  workBLAST_species_decision,
                  final_pneumo_decision)
  ,
  by = "specimen_id"
) %>% 
  dplyr::left_join(
    read.csv("inputs/genData_all_adjusted_gpsc.csv") %>% 
      dplyr::select(-area,
                    -workBLAST_species_decision
      )
    ,
    by = "specimen_id"
  ) %>% 
  dplyr::distinct(specimen_id, .keep_all = T) %>% # check duplicated IDs in genData
  glimpse()

write.csv(df_epi_gen_pneumo,
          "inputs/genData_pneumo_with_epiData_with_final_pneumo_decision_adjusted_gpsc.csv",
          row.names = F)
