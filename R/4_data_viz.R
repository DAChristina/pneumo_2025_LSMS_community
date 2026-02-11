library(tidyverse)
library(ggtree)
library(ggtreeExtra)
source("global/fun.R")

df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  # dplyr::left_join(
  #   read.csv("inputs/genData_pneumo_panvita_long.csv") %>% 
  #     dplyr::select(Strains,
  #                   contains("gene_present_absent_"))
  #   ,
  #   by = c("workFasta_name" = "Strains")
  # ) %>% 
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
                                                   "nontypeable")),
                workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned", "not assigned",
                                             workWGS_gpsc_strain),
                vaccination_status_area = case_when(
                  area == "Lombok" |
                    area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
                  TRUE ~ "Pre-implemented area (Manado & Sorong)"
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
tre_pp <- ape::read.tree("outputs/result_poppunk/rapidnj_no_GPSC/rapidnj_no_GPSC_core_NJ.tree")
tre_raxml <- ape::read.tree("outputs/result_raxml_from_panaroo/RAxML_bestTree.1_output_tree")

# focused on raxml tree: rearrange label coz' ggtree link label to row_names
df_epi_gen_pneumo <- 
  dplyr::left_join(
    data.frame(tre_raxml$tip.label),
    df_epi_gen_pneumo,
    by = c("tre_raxml.tip.label" = "label")
  )

rownames(df_epi_gen_pneumo) <- tre_raxml$tip.label

# test node
ggtree(tre_raxml) + 
  geom_tiplab(size = 2) +
  geom_label2(aes(subset=!isTip, label=node),
              size=2, color="darkred", alpha=0.5)

# analyse weird subtree:
subtree <- ape::extract.clade(tre_raxml,
                              node = 1122) 
# ggtree(subtree) + 
#   geom_tiplab(size = 2) +
#   geom_label2(aes(subset=!isTip, label=node), size=3, color="darkred", alpha=0.5)
df_subtree <- data.frame(
  selected_id = subtree$tip.label
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(tre_raxml.tip.label,
                    serotype_final_decision,
                    workWGS_gpsc_strain,
                    workWGS_AMR_MDR_flag)
    ,
    by = c("selected_id" = "tre_raxml.tip.label")
  ) %>%
  # view() %>% 
  glimpse()

# basic
show_pp <- ggtree(tre_pp,
                  layout = "fan",
                  open.angle=30,
                  size=0.75,
                  # aes(colour=Clade)
                  ) %<+% 
  df_epi_gen_pneumo +
  # geom_tiplab(size = 2) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  geom_hilight(node=367, fill="pink", alpha=0.5)
# show_pp

show_raxml <- ggtree(tre_raxml,
                  layout = "fan",
                  open.angle=30,
                  size=0.25,
                  # aes(colour=Clade)
) %<+% 
  df_epi_gen_pneumo +
  # geom_tiplab(size = 2) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # 19F (1123) & 6B (1168) --> all = 1122
  # geom_hilight(node=1122, fill="pink", alpha=0.5) +
  geom_hilight(node=1123, fill="orange", alpha=0.5) +
  geom_hilight(node=1168, fill="purple", alpha=0.5) +
  
  # 23F GPSC14
  geom_hilight(node=638, fill="red", alpha=0.5)
show_raxml

# gen tree #####################################################################
tree_gen_raxml <- show_raxml %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.02,
    offset=0.05
  ) +
  scale_fill_manual(
    name="PCV13 serotype coverage",
    values=c(col_map),
    breaks = c("VT", "NVT", "nontypeable"),
    labels = c("VT", "NVT", "nontypeable"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3, order=1)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # serotype
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_final_decision),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Serotype",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 7, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # GPSC
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_gpsc_strain), #gpsc_dominant_filter),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "GPSC",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 7, order = 3)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # age groups
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$age_year_3groups),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Age groups",
    values=c(col_map),
    labels=c("<1", "1-2", "3-5"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 3, order = 4)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # area (vaccination status)
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$vaccination_status_area),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_manual(
    name="Vaccination status area",
    values=c(col_map),
    labels=c("PCV13-implemented area (Lombok & Sumbawa)",
             "Pre-implemented area (Manado & Sorong)"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 1, order = 5)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  )

png("pictures/phylo_raxml_1epiTree.png",
    width = 25, height = 20, units = "cm", res = 800)
tree_gen_raxml
dev.off()

# AMR tree ver1 ################################################################
tree_amr_raxml <- show_raxml %<+%
  df_epi_gen_pneumo +
  # vaccine classification
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_classification_PCV13_final_decision),
    width=0.02,
    offset=0.05
  ) +
  scale_fill_manual(
    name="PCV13 serotype coverage",
    values=c(col_map),
    breaks = c("VT", "NVT", "untypeable"),
    labels = c("VT", "NVT", "untypeable"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol=3,
                       order=1)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # serotype
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$serotype_final_decision),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    name = "Serotype",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 5, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # chloramphenicol
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_chloramphenicol),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Chloramphenicol",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 2)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # clindamycin
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_clindamycin),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Clindamycin",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 3)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # erythromycin
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_erythromycin),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Erythromycin",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 4)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # fluorofluoroquinolones
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom=geom_tile,
  #   mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_fluoroquinolones),
  #   width=0.02,
  #   offset=0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Fluorofluoroquinolones",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 5)
  # ) +
  # theme(
  #   legend.title=element_text(size=12), 
  #   legend.text=element_text(size=9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # tetracycline
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_tetracycline),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Tetracycline",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 6)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # antifolates
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom=geom_tile,
    mapping=aes(fill=df_epi_gen_pneumo$workWGS_AMR_logic_class_antifolates),
    width=0.02,
    offset=0.1
  ) +
  scale_fill_viridis_d(
    # name = "Antifolates",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 7)
  ) +
  theme(
    legend.title=element_text(size=12), 
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # meropenem
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_carbapenems),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Meropenem",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 14)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # penicillins
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_penicillins),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   # name = "Penicillins",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 3, order = 15)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # cephalosporins
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_logic_class_cephalosporins),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    # name = "Cephalosporins",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 16)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  # MDR flag
  ggnewscale::new_scale_fill() +
  ggtreeExtra::geom_fruit(
    geom = geom_tile,
    mapping = aes(fill = df_epi_gen_pneumo$workWGS_AMR_MDR_flag),
    width = 0.02,
    offset = 0.1
  ) +
  scale_fill_viridis_d(
    name = "MDR Flag",
    option = "C",
    direction = -1,
    guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
                         ncol = 3, order = 17)
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) +
  theme(
    legend.title = element_text(size = 0),
    legend.text = element_text(size = 9),
    legend.spacing.y = unit(0.02, "cm")
  ) #+
  # # Adherence (pavA)
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_pavA_Adherence),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "Adherence (pavA)",
  #   option = "C",
  #   direction = 1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 2, order = 18)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # # Adherence (cbpA/pspC)
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_cbpA.pspC_Adherence),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "Adherence (cbpA/pspC)",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 2, order = 19)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # # Exoenzyme (lytA)
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_lytA_Exoenzyme),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "Exoenzyme (lytA)",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 2, order = 20)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # # Exotoxin (ply)
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_ply_Exotoxin),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "Exotoxin (ply)",
  #   option = "C",
  #   direction = 1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 2, order = 21)
  # ) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # ) +
  # # Immune modulation (pspA)
  # ggnewscale::new_scale_fill() +
  # ggtreeExtra::geom_fruit(
  #   geom = geom_tile,
  #   mapping = aes(fill = df_epi_gen_pneumo$gene_present_absent_pspA_Immune.modulation),
  #   width = 0.02,
  #   offset = 0.1
  # ) +
  # scale_fill_viridis_d(
  #   name = "Immune modulation (pspA)",
  #   option = "C",
  #   direction = -1,
  #   guide = guide_legend(keywidth = 0.3, keyheight = 0.3,
  #                        ncol = 2, order = 22)
  # ) +
  # # geom_axis_text(angle=-45, hjust=0, size=1.5) +
  # theme(
  #   legend.title = element_text(size = 0),
  #   legend.text = element_text(size = 9),
  #   legend.spacing.y = unit(0.02, "cm")
  # )

# png("pictures/phylo_raxml_2AMR_ver1.png",
#     width = 30, height = 25, units = "cm", res = 800)
# tree_amr_raxml
# dev.off()

# AMR tree ver2 ################################################################

filtered_df <- df_epi_gen_pneumo %>% 
  dplyr::select(
    # serotype_classification_PCV13_final_decision,
    # serotype_final_decision,
    # tre_raxml.tip.label,
    contains("workWGS_AMR_logic_class_"),
    -workWGS_AMR_logic_class_counts,
    -workWGS_AMR_logic_class_kanamycin, # 0 resistance
    -workWGS_AMR_logic_class_linezolid, # 0 resistance
    workWGS_AMR_MDR_flag,
    # contains("gene_present_absent_")
  ) %>% 
  dplyr::rename_with(
    ~ sub("workWGS_AMR_logic_class_|workWGS_AMR_|gene_present_absent_", "", .)
  ) %>% 
  # rearrange columns
  dplyr::select(
    fluoroquinolones,
    meropenem, penicillins, cephalosporins,
    clindamycin, chloramphenicol,
    # the big three
    erythromycin, tetracycline, antifolates,
    MDR_flag
  ) %>%
  glimpse()

rownames(filtered_df) <- tre_raxml$tip.label

all_labels <- unique(unlist(lapply(filtered_df, as.character)))
manual <- c(" Not found" = "palegoldenrod")
others <- setdiff(all_labels, names(manual))

auto_col <- scales::hue_pal()(length(others))
names(auto_col) <- others
final_col <- c(manual, auto_col)

# factor_levels <- c("FolP", 
#                    "FolP", 
#                    "Tet(M)", 
#                    "cat",
#                    "mefA",
#                    " Not found", 
#                    "NA")
# 
# filtered_df <- filtered_df %>%
#   dplyr::mutate(across(everything(), ~factor(.x, levels = factor_levels)))

png("pictures/phylo_raxml_2AMR_ver2.png",
    width = 25, height = 15, units = "cm", res = 800)
library(ggnewscale)
p2 <- tree_gen_raxml + ggnewscale::new_scale_fill()
ggtree::gheatmap(p2, filtered_df,
                 offset=0.07, width=1, font.size=2, 
                 colnames_angle=-45, hjust=0) +
  scale_fill_manual(values = final_col,
                    na.value = "white",
                    drop = F,
                    # na.translate = FALSE,
                    name = "Antimicrobial resistance genes",
                    guide = guide_legend(ncol = 7)
                    
  ) +
  # readjust legends
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )
dev.off()



