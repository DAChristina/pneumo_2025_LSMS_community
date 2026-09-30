library(tidyverse)
library(ggtree)
library(ggtreeExtra)
source("global/fun.R")

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

# analyse subtree:
subtree1 <- ape::extract.clade(tre_raxml,
                              node = 902)

subtree2 <- c(
  ape::extract.clade(tre_raxml, 638),
  ape::extract.clade(tre_raxml, 1168),
  ape::extract.clade(tre_raxml, 1123),
  ape::extract.clade(tre_raxml, 794),
  ape::extract.clade(tre_raxml, 902)
)

df_subtree <- dplyr::bind_rows(
  data.frame(node = 638,
             selected_id  = ape::extract.clade(tre_raxml, 638)$tip.label),
  data.frame(node = 1168,
             selected_id  = ape::extract.clade(tre_raxml, 1168)$tip.label),
  data.frame(node = 1123,
             selected_id  = ape::extract.clade(tre_raxml, 1123)$tip.label),
  data.frame(node = 794,
             selected_id  = ape::extract.clade(tre_raxml, 794)$tip.label),
  data.frame(node = 902,
             selected_id  = ape::extract.clade(tre_raxml, 902)$tip.label)
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(tre_raxml.tip.label,
                    serotype_final_decision,
                    workWGS_gpsc_strain,
                    workWGS_MLST_pw_ST,
                    workWGS_AMR_MDR_flag)
    ,
    by = c("selected_id" = "tre_raxml.tip.label")
  ) %>%
  # view() %>%
  glimpse()

# proportion calculation per node
prop_calc2 <- df_subtree %>% 
  dplyr::group_by(node,
                  serotype_final_decision,
                  workWGS_gpsc_strain,
                  # workWGS_MLST_pw_ST
                  ) %>% 
  dplyr::summarise(n_all = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_subtree %>% 
      dplyr::group_by(node) %>% 
      dplyr::summarise(n_node = n(), .groups = "drop")
    ,
    by = "node"
  ) %>% 
  dplyr::mutate(
    percent = round(n_all/n_node*100, 2),
    report = paste0(n_all, "/", n_node, " (", percent, "%)")
  ) %>% 
  arrange(desc(percent)) %>% 
  glimpse()

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
  # 23F GPSC14
  geom_hilight(node=638, fill="#A63603", alpha=0.5) +
  geom_cladelab(
    data = data.frame(
      node = 638,
      name = "23F\n(GPSC14-ST242)\ndominant"
    ),
    mapping = aes(
      node = node,
      label = name
    ),
    align = TRUE,
    offset = .23,
    offset.text = .045,
    hjust = "center",
    barsize = .2,
    fontsize = 3,
    angle = "auto",
    horizontal = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.margin = grid::unit(c(-15, -15, -15, -15), "mm")
  ) +
  # 19F (1123) & 6B (1168) --> all = 1122
  # geom_hilight(node=1122, fill="pink", alpha=0.5) +
  geom_hilight(node=1168, fill="#FF7F00", alpha=0.5) +
  geom_cladelab(
    data = data.frame(
      node = 1168,
      name = "6B (GPSC185)\ndominant"
    ),
    mapping = aes(
      node = node,
      label = name
    ),
    align = TRUE,
    offset = .23,
    offset.text = .025,
    hjust = "center",
    barsize = .2,
    fontsize = 3,
    angle = "auto",
    horizontal = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.margin = grid::unit(c(-15, -15, -15, -15), "mm")
  ) +
  geom_hilight(node=1123, fill="#E31A1C", alpha=0.5) +
  geom_cladelab(
    data = data.frame(
      node = 1123,
      name = "19F (GPSC1)\ndominant"
    ),
    mapping = aes(
      node = node,
      label = name
    ),
    align = TRUE,
    offset = .23,
    offset.text = .025,
    hjust = "center",
    barsize = .2,
    fontsize = 3,
    angle = "auto",
    horizontal = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.margin = grid::unit(c(-15, -15, -15, -15), "mm")
  ) +
  # 11A
  geom_hilight(node=794, fill="#1F78B4", alpha=0.5) +
  geom_cladelab(
    data = data.frame(
      node = 794,
      name = "11A\n(GPSC642-ST6191)\ndominant"
    ),
    mapping = aes(
      node = node,
      label = name
    ),
    align = TRUE,
    offset = .23,
    offset.text = .045,
    hjust = "center",
    barsize = .2,
    fontsize = 3,
    angle = "auto",
    horizontal = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.margin = grid::unit(c(-15, -15, -15, -15), "mm")
  ) +
  # serogroup 15 (15B & 15C)
  geom_hilight(node=902, fill="#33A02C", alpha=0.5) +
  geom_cladelab(
    data = data.frame(
      node = 902,
      name = "15B & 15C\n(GPSC11-ST193)\ndominant"
    ),
    mapping = aes(
      node = node,
      label = name
    ),
    align = TRUE,
    offset = .23,
    offset.text = .045,
    hjust = "center",
    barsize = .2,
    fontsize = 3,
    angle = "auto",
    horizontal = FALSE
  ) +
  theme(
    legend.position = "none",
    plot.margin = grid::unit(c(-15, -15, -15, -15), "mm")
  )
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
    breaks = c("VT", "NVT", "NT"),
    labels = c("VT", "NVT", "NT"),
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
             "Pre-implemented area (Minahasa & Sorong)"),
    guide=guide_legend(keywidth=0.3, keyheight=0.3,
                       ncol = 1, order = 5)
  ) +
  theme(
    legend.title=element_text(size=12),
    legend.text=element_text(size=9),
    legend.spacing.y = unit(0.02, "cm")
  )

# png("pictures/phylo_raxml_1epiTree.png",
#     width = 30, height = 20, units = "cm", res = 800)
tree_gen_raxml
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


png("pictures/phylo_raxml_2AMR_ver2.png",
    width = 30, height = 15, units = "cm", res = 800)
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
                    guide = guide_legend(ncol = 7),
                    labels = function(x) parse(text = parse_italiced_legends(x))
                    
  ) +
  # readjust legends
  theme(
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    legend.key.size = unit(0.4, "cm")
  )
dev.off()



