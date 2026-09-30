library(tidyverse)

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision_adjusted_gpsc.csv") %>% 
  dplyr::right_join(
    read.table("outputs/result_poppunk/qfile_filtered_19to23.txt") %>% 
      dplyr::mutate(specimen_id = V1,
                    workPoppunk_qc = "pass_qc") %>% 
      dplyr::select(specimen_id, workPoppunk_qc)
    ,
    by = "specimen_id"
    
  ) %>% 
  dplyr::filter(workPoppunk_qc == "pass_qc") %>%
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
                # test individual serotype
                # serotype_final_decision = ifelse(serotype_final_decision == "NT", "NT", "others"),
                
                workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned", "not assigned",
                                             workWGS_gpsc_strain),
                vaccination_status_area = case_when(
                  area == "Lombok" |
                    area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
                  TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
                ),
                
                # reset logic label for AMR-MDR viz
                workWGS_AMR_logic_class_chloramphenicol = case_when(
                  workWGS_AMR_chloramphenicol != "NF" ~ stringr::str_extract(workWGS_AMR_chloramphenicol,
                                                                             "(?<=R \\().*(?=\\))"),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_chloramphenicol = case_when(
                  workWGS_AMR_logic_class_chloramphenicol == "cat_pC194" ~ "cat (pC194)",
                  workWGS_AMR_logic_class_chloramphenicol == "cat_q" ~ "catQ",
                  TRUE ~ workWGS_AMR_logic_class_chloramphenicol
                ),
                workWGS_AMR_logic_class_chloramphenicol = factor(workWGS_AMR_logic_class_chloramphenicol),
                
                workWGS_AMR_logic_class_clindamycin = case_when(
                  workWGS_AMR_clindamycin != "NF" ~ stringr::str_extract(workWGS_AMR_clindamycin,
                                                                         "(?<=R \\().*(?=\\))"),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_clindamycin = factor(workWGS_AMR_logic_class_clindamycin),
                
                workWGS_AMR_logic_class_erythromycin = case_when(
                  workWGS_AMR_erythromycin != "NF" ~ stringr::str_extract(workWGS_AMR_erythromycin,
                                                                          "(?<=R \\().*(?=\\))"
                  ),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_erythromycin = case_when(
                  workWGS_AMR_logic_class_erythromycin == "mefA_10" ~ "mefA",
                  TRUE ~ workWGS_AMR_logic_class_erythromycin
                ),
                workWGS_AMR_logic_class_erythromycin = factor(workWGS_AMR_logic_class_erythromycin),
                
                workWGS_AMR_logic_class_fluoroquinolones = case_when(
                  workWGS_AMR_fluoroquinolones != "NF" ~ stringr::str_extract(workWGS_AMR_fluoroquinolones,
                                                                              "(?<=R \\().*(?=\\))"
                  ),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_fluoroquinolones = case_when(
                  workWGS_AMR_logic_class_fluoroquinolones == "parC_S79Y" |
                    workWGS_AMR_logic_class_fluoroquinolones == "parC_D83N" ~ "parC",
                  TRUE ~ workWGS_AMR_logic_class_fluoroquinolones
                ),
                workWGS_AMR_logic_class_fluoroquinolones = factor(workWGS_AMR_logic_class_fluoroquinolones),
                
                workWGS_AMR_logic_class_tetracycline = case_when(
                  workWGS_AMR_tetracycline != "NF" ~ stringr::str_extract(workWGS_AMR_tetracycline,
                                                                          "(?<=R \\().*(?=\\))"
                  ),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_tetracycline = case_when(
                  stringr::str_detect(workWGS_AMR_logic_class_tetracycline, "tetM") ~ "tet(M)",
                  stringr::str_detect(workWGS_AMR_logic_class_tetracycline, "tet_") ~ "tet",
                  workWGS_AMR_logic_class_tetracycline == "tetK" ~ "tet(K)",
                  TRUE ~ workWGS_AMR_logic_class_tetracycline
                ),
                workWGS_AMR_logic_class_tetracycline = factor(workWGS_AMR_logic_class_tetracycline),
                
                workWGS_AMR_logic_class_antifolates = case_when(
                  workWGS_AMR_class_antifolates != "NF" ~ stringr::str_extract(workWGS_AMR_class_antifolates,
                                                                               "(?<=R \\().*(?=\\))"
                  ),
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_antifolates = case_when(
                  workWGS_AMR_logic_class_antifolates == "folA_I100L" ~ "folA",
                  workWGS_AMR_logic_class_antifolates == "folP_57-70" ~ "folP",
                  workWGS_AMR_logic_class_antifolates == "folA_I100L & folP_57-70" ~ "folA & folP",
                  TRUE ~ workWGS_AMR_logic_class_antifolates
                ),
                workWGS_AMR_logic_class_antifolates = factor(workWGS_AMR_logic_class_antifolates),
                
                # SIR format
                workWGS_AMR_logic_class_cephalosporins = case_when(
                  workWGS_AMR_logic_class_cephalosporins ~ " Resistance",
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_cephalosporins = factor(workWGS_AMR_logic_class_cephalosporins, levels = c(" Not found", " Resistance")),
                
                workWGS_AMR_logic_class_penicillins = case_when(
                  workWGS_AMR_logic_class_penicillins ~ " Resistance",
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_penicillins = factor(workWGS_AMR_logic_class_penicillins, levels = c(" Not found", " Resistance")),
                
                workWGS_AMR_logic_class_meropenem = case_when(
                  workWGS_AMR_logic_class_meropenem ~ " Resistance",
                  TRUE ~ " Not found"
                ),
                workWGS_AMR_logic_class_meropenem = factor(workWGS_AMR_logic_class_meropenem, levels = c(" Not found", " Resistance")),
                
                workWGS_AMR_MDR_flag = ifelse(workWGS_AMR_MDR_flag == "non-MDR", " Not found", " MDR"),
                workWGS_AMR_MDR_flag = factor(workWGS_AMR_MDR_flag,
                                              levels = c(" Not found", " MDR")),
                
  ) %>% 
  glimpse()

min_n_gpsc <- 10
top_st <- 5
top_sero <- 5

meta <- df_epi_gen_pneumo %>%
  dplyr::rename(
    ST = workWGS_MLST_pw_ST,
    GPSC = workWGS_gpsc_strain,
    Serotype = serotype_final_decision,
    CC = workWGS_CC
  ) %>% 
  mutate(across(c(Serotype, ST, CC, GPSC), as.character)) %>%
  mutate(
    ST = coalesce(na_if(ST, ""), "Unassigned"),
    CC = coalesce(na_if(CC, ""), "Singleton/Unassigned"),
    Serotype = coalesce(na_if(Serotype, ""), "Unknown")
  ) %>%
  filter(!is.na(GPSC), GPSC != "")

top_label <- function(x, n = 3) {
  tab <- sort(table(x), decreasing = TRUE)
  tab <- head(tab, n)
  paste0(names(tab), " (", tab, ")", collapse = ", ")
}

dominant <- function(x) names(sort(table(x), decreasing = TRUE))[1]

gpsc_summary <- meta %>%
  group_by(GPSC) %>%
  summarise(
    n_isolates = n(),
    n_STs = n_distinct(ST),
    dominant_ST = dominant(ST),
    dom_ST_pct = round(100 * max(table(ST)) / n(), 1),
    dominant_CC = dominant(CC),
    main_STs = top_label(ST, top_st),
    main_CCs = top_label(CC, 2),
    serotypes = top_label(Serotype, top_sero),
    .groups = "drop"
  ) %>%
  arrange(desc(n_isolates))

write.csv(gpsc_summary,
          "outputs/genome_supp_gpsc_summary_table.csv",
          row.names = FALSE)

# Full ST composition within each GPSC
st_by_gpsc <- meta %>%
  count(GPSC, ST, CC, name = "n") %>%
  group_by(GPSC) %>%
  mutate(pct_of_gpsc = round(100 * n / sum(n), 1)) %>%
  ungroup() %>%
  arrange(GPSC, desc(n))

write.csv(st_by_gpsc,
          "outputs/genome_supp_st_composition_by_gpsc.csv",
          row.names = FALSE)