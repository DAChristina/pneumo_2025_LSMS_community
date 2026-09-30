library(tidyverse)
source("global/fun.R")


# epi vaccination coverage #####################################################
df_vaccCoverage <- read.csv("inputs/epiData_eng.csv") %>% 
  dplyr::filter(consent == "yes") %>% 
  dplyr::group_by(area, vaccination_pcv13_dc_n_regroup) %>% 
  dplyr::summarise(count_nvac = n()) %>%
  dplyr::left_join(
    read.csv("inputs/epiData_eng.csv") %>% 
      dplyr::group_by(area) %>% 
      dplyr::summarise(count_area = n())
    ,
    by = "area"
  ) %>% 
  dplyr::mutate(percentage = round(count_nvac / count_area * 100, 1),
                area = factor(area,
                              levels = c("Lombok", "Sumbawa",
                                         "Minahasa", "Sorong"))
                ) %>%
  dplyr::arrange(desc(percentage)) %>% 
  # dplyr::transmute(
  #   group = vaccination_pcv13_dc_n_regroup,
  #   report = paste0(count_nvac, "/", count_area,  " (", percentage, "%)")
  # ) %>%
  glimpse() %>% 
  ggplot(.,
         aes(x = vaccination_pcv13_dc_n_regroup,
             y = percentage,
             fill = area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 3,
            angle = 0,
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "PCV13 Vaccination", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = "none", # legend.position = c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")

# png(file = "pictures/epiAnalyses_vaccination_coverage_grouped.png",
#     width = 29, height = 20, unit = "cm", res = 600)
# df_vaccCoverage
# dev.off()

# data viz just only for serotype & AMR ########################################
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
  dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      serotype_final_decision == "mixed serotypes/serogroups" ~ "mixed serogroups",
      TRUE ~ serotype_final_decision
    ),
    serotype_final_decision = factor(serotype_final_decision,
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
    serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "NT")),
    serotype_classification_PCV15_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "NT"))
  ) %>%
  glimpse()

table(df_epi_gen_pneumo$area)

# test classification df
test_classification <- df_epi_gen_pneumo %>% 
  dplyr::select(workWGS_serotype,
                serotype_final_decision,
                serotype_classification_PCV13_final_decision) %>% 
  # view() %>% 
  glimpse()


df_serotype_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_final_decision) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision)
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::distinct() %>% 
  # view() %>% 
  glimpse()

df_serotype_classification_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_classification_PCV13_final_decision) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

df_serotype_classification_perArea_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(serotype_classification_PCV13_final_decision, area) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

# compiled VT percentage per area ##############################################
df_compiled_VT_percentage <- dplyr::left_join(
  df_epi_gen_pneumo %>% 
    dplyr::group_by(area, serotype_classification_PCV13_final_decision) %>% 
    dplyr::summarise(count_vacc = n(), .groups = "drop") %>% 
    dplyr::mutate(
      count_vacc = case_when(
        is.na(count_vacc) ~ 0,
        TRUE ~ count_vacc
      )
    )
  ,
  df_epi_gen_pneumo %>% 
    dplyr::group_by(area) %>% 
    dplyr::summarise(count_area = n(), .groups = "drop") %>% 
    dplyr::mutate(
      count_area = case_when(
        is.na(count_area) ~ 0,
        TRUE ~ count_area
      )
    )
  ,
  by = "area"
) %>% 
  dplyr::mutate(
    percent = round(count_vacc/count_area*100, 1),
    report_vaccination = paste0(count_vacc, " (", percent, "%)"),
    # vaccination_pcv13_dc_n_regroup = case_when(
    #   is.na(vaccination_pcv13_dc_n_regroup) ~ "0 not yet",
    #   T ~ vaccination_pcv13_dc_n_regroup
    # ),
    area = factor(area,
                  levels = c("Lombok", "Sumbawa", "Minahasa", "Sorong"))
  ) %>% 
  # view() %>% 
  glimpse() %>% 
  ggplot(., aes(x = serotype_classification_PCV13_final_decision,
                                        y = percent,
                                        fill = area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            vjust = -0.5, size = 3,
            angle = 0,
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # facet_grid(~ area,
  #            scales = "free_x",
  #            space = "free_x"
  # ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "PCV13 Vaccine Group", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = "right", # c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "centre"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
        ) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")

# png(file = "pictures/genData_vaccCoverage_and_serotypes_vaccineGroup.png",
#     width = 29, height = 10, unit = "cm", res = 600)
combined_vaccCoverage_and_serotypes <- cowplot::plot_grid(
  df_vaccCoverage, df_compiled_VT_percentage,
  nrow = 1,
  labels = c("A", "B"),
  rel_widths = c(0.8, 1))
# dev.off()


# serotypes per-area ###########################################################
ser1 <- ggplot(df_serotype_summary %>% 
                 dplyr::mutate(percentage_label = ifelse(percentage < 3, NA,
                                                         paste0(round(percentage, 1), "%")))
               ,
               aes(x = serotype_final_decision, y = percentage,
                   fill = serotype_classification_PCV13_final_decision)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage_label),
            angle = 30,
            vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 11)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  scale_fill_manual(values = c(col_map)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "right", # c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "centre"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))
ser1

# Compute percentage by age_year_3groups
df_serotype_age_year_3groups_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(age_year_3groups, serotype_final_decision) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(age_year_3groups) %>% 
  # percentage is calculated from count_serotype per age_year_3groups
  dplyr::mutate(percentage = count / sum(count) * 100,
                age_year_3groups = factor(age_year_3groups,
                                  levels = c("1", "2", "3", "4", "5")),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F") ~ "VT",
                  serotype_final_decision == "NT" ~ "NT",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV15_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F",
                                                 "22F", "33F") ~ "VT",
                  serotype_final_decision == "NT" ~ "NT",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_classification_PCV13_final_decision == "NT" ~ " ",
                  TRUE ~ serotype_classification_PCV13_final_decision
                ),
                serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("VT", "NVT", " ")),
  )

# ser_age <- ggplot(df_serotype_age_year_3groups_summary, aes(x = serotype_final_decision,
#                                                     y = percentage,
#                                                     fill = age_year_3groups)) +
#   geom_bar(stat = "identity", position = position_dodge()) +
#   # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   facet_grid(~ serotype_classification_PCV13_final_decision,
#              scales = "free_x",
#              space = "free_x"
#   ) +
#   scale_fill_manual(values = c(col_map)) +
#   labs(x = "Category", y = "Percentage", 
#        # title = "All Serotypes"
#   ) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
#         legend.position = c(0.02, 0.75),
#         legend.direction = "vertical",
#         legend.justification = c("left", "top"),
#         legend.background = element_rect(fill = NA, color = NA),
#         legend.title = element_blank(),
#         legend.margin = margin(t = -50),
#         legend.spacing.y = unit(-0.3, "cm")) # +
# # facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
# ser_age


# Compute percentage by area
df_serotype_area_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(area, serotype_final_decision) %>%
  dplyr::summarise(count = n(), .groups = "drop") %>%
  dplyr::group_by(area) %>% 
  # percentage is calculated from count_serotype per area
  dplyr::mutate(percentage = count / sum(count) * 100,
                area = factor(area,
                              levels = c("Lombok", "Sumbawa", "Minahasa", "Sorong")),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F") ~ "VT",
                  serotype_final_decision == "NT" ~ "NT",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV15_final_decision = case_when(
                  serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                 "6A", "6B", "9V", "14", "18C",
                                                 "19A", "19F", "23F",
                                                 "22F", "33F") ~ "VT",
                  serotype_final_decision == "NT" ~ "NT",
                  TRUE ~ "NVT"
                ),
                serotype_classification_PCV13_final_decision = case_when(
                  serotype_classification_PCV13_final_decision == "NT" ~ " ",
                  TRUE ~ serotype_classification_PCV13_final_decision
                ),
                serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                      levels = c("VT", "NVT", " ")),
  )

ser2 <- ggplot(df_serotype_area_summary, aes(x = serotype_final_decision,
                                             y = percentage,
                                             fill = area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "Category", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "right", # c(0.02, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "centre"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) # +
# facet_wrap(~ serotype_classification_PCV13_final_decision, nrow = 1, scales = "free_x")
ser2

combined_serotypes <- cowplot::plot_grid(
  ser1, ser2,
  nrow = 2,
  labels = c("C", "D"))


png(file = "pictures/genData_combined_vaccCoverage_and_serotypes.png",
    width = 29, height = 29, unit = "cm", res = 600)
cowplot::plot_grid(combined_vaccCoverage_and_serotypes, combined_serotypes,
                   nrow = 2,
                   rel_heights = c(0.5, 1)
                   )
dev.off()



# additional visualisation of serotype percentage per area & age ###############
# additional visualisation of serotype percentage per area & age
# levels = c("Lombok", "Sumbawa", "Minahasa", "Sorong"))
area <- unique(df_epi_gen_pneumo$area)
plotStore_area <- list()

for(a in area){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::filter(area == a) %>% 
    dplyr::group_by(age_year_3groups, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year_3groups) %>% 
    # percentage is calculated from count_serotype per area
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year_3groups = factor(age_year_3groups,
                                    levels = c("< 1 year old",
                                               "1-2 years old",
                                               "3-5 years old")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "NT" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year_3groups)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_area[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_area.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_area[[1]],
                   plotStore_area[[3]],
                   plotStore_area[[4]],
                   plotStore_area[[2]],
                   nrow = 4)
dev.off()

# additional visualisation of serotype percentage per PCV13-implemented area & age
# levels = c("PCV13-implemented area", "Not yet implemented area"))
vaccination_status_area <- c("PCV13-implemented area (Lombok & Sumbawa)",
                             "Pre-implemented area (Minahasa & Sorong)")
plotStore_vaccArea <- list()

for(a in vaccination_status_area){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::mutate(vaccination_status_area = case_when(
      area == "Lombok" |
        area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
      TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_area == a) %>% 
    dplyr::group_by(age_year_3groups, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year_3groups) %>% 
    # percentage is calculated from count_serotype per area
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year_3groups = factor(age_year_3groups,
                                            levels = c("< 1 year old",
                                                       "1-2 years old",
                                                       "3-5 years old")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "NT" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year_3groups)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vaccArea[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_vaccArea.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vaccArea,
                   nrow = 2)
dev.off()


# additional visualisation of serotype percentage per area & age (year) ########
# additional visualisation of serotype percentage per area & age
# levels = c("Lombok", "Sumbawa", "Minahasa", "Sorong"))
area <- unique(df_epi_gen_pneumo$area)
plotStore_area <- list()

for(a in area){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::filter(area == a) %>% 
    dplyr::group_by(age_year, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year) %>% 
    # percentage is calculated from count_serotype per area
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year = factor(age_year,
                                    levels = c("0", "1", "2", "3", "4", "5")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "NT" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_area[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_area_year.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_area[[1]],
                   plotStore_area[[3]],
                   plotStore_area[[4]],
                   plotStore_area[[2]],
                   nrow = 4)
dev.off()

# additional visualisation of serotype percentage per PCV13-implemented area & age
# levels = c("PCV13-implemented area", "Not yet implemented area"))
vaccination_status_area <- c("PCV13-implemented area (Lombok & Sumbawa)",
                             "Pre-implemented area (Minahasa & Sorong)")
plotStore_vaccArea <- list()

for(a in vaccination_status_area){
  plot <- df_epi_gen_pneumo %>% 
    dplyr::mutate(vaccination_status_area = case_when(
      area == "Lombok" |
        area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
      TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_area == a) %>% 
    dplyr::group_by(age_year, serotype_final_decision) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    dplyr::group_by(age_year) %>% 
    # percentage is calculated from count_serotype per area
    dplyr::mutate(percentage = count / sum(count) * 100,
                  age_year = factor(age_year,
                                    levels = c("0", "1", "2", "3", "4", "5")),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV15_final_decision = case_when(
                    serotype_final_decision %in% c("1", "3", "4", "5", "7F",
                                                   "6A", "6B", "9V", "14", "18C",
                                                   "19A", "19F", "23F",
                                                   "22F", "33F") ~ "VT",
                    serotype_final_decision == "NT" ~ "NT",
                    TRUE ~ "NVT"
                  ),
                  serotype_classification_PCV13_final_decision = case_when(
                    serotype_classification_PCV13_final_decision == "NT" ~ " ",
                    TRUE ~ serotype_classification_PCV13_final_decision
                  ),
                  serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                        levels = c("VT", "NVT", " ")),
    ) %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = percentage,
               fill = age_year)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    # geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, size = 3) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = "Category", y = "Percentage", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    labs(x = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          # legend.position = "none",
          legend.direction = "vertical",
          # legend.justification = "bottom",
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vaccArea[[a]] <- plot
}

png(file = "pictures/genData_serotypes_classification_filterPneumo_vaccArea_year.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vaccArea,
                   nrow = 2)
dev.off()





# additional visualisation of GPSC percentage per serotype per vaccarea ########
serobigsix_all <- df_epi_gen_pneumo %>% 
  dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                               "6A", "6B",
                                               "11A", "13",
                                               "15B", "15C",
                                               "NT")
  ) %>% 
  dplyr::group_by(serotype_final_decision,
                  workWGS_gpsc_strain) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                                   "6A", "6B",
                                                   "11A", "13",
                                                   "15B", "15C",
                                                   "NT")
      ) %>% 
      dplyr::group_by(serotype_final_decision
      ) %>% 
      dplyr::summarise(n_all = n(), .groups = "drop")
    ,
    by = c("serotype_final_decision")
  ) %>% 
  dplyr::mutate(
    percent = round(n/n_all*100, 1),
    report = paste0(n, "/", n_all,  " (", percent, "%)"),
    # adjust some values for viz
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "not assigned",
                                 "Not\nassigned",
                                 workWGS_gpsc_strain),
    label = ifelse(percent <= 25, " ", paste0(round(percent, 1), "%")),
    serotype_final_decision = ifelse(serotype_final_decision != "NT",
                                     paste0("Serotype ",
                                            serotype_final_decision),
                                     "NT"),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       "Serotype 6A", "Serotype 6B",
                                       "Serotype 19F", "Serotype 23F",
                                       
                                       # NVT
                                       "Serotype 11A", "Serotype 13",
                                       "Serotype 15B", "Serotype 15C",
                                       
                                       # NT
                                       "NT"
                                     )),
  ) %>% 
  dplyr::arrange(serotype_final_decision,
                 desc(percent),
  ) %>% 
  glimpse() %>% 
  ggplot(.,
         aes(x = workWGS_gpsc_strain,
             y = percent,
             fill = serotype_final_decision)) +
  geom_bar(stat = "identity",
           position = "stack") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 110)) +
  # scale_fill_manual(values = c(col_map)) +
  scale_fill_viridis_d() +
  labs(x = "GPSC", y = "Percentage"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.text  = element_text(size = 11),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )

serobigsix <- df_epi_gen_pneumo %>% 
  dplyr::mutate(
    # add vaccination status according to area
    vacc_area = case_when(
      area == "Lombok" | 
        area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
      TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
    ),
    # vacc_area = factor(vacc_area,
    #                    levels = c("no", "vaccinated")),
    
  ) %>% 
  dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                               "6A", "6B",
                                               "11A", "13",
                                               "15B", "15C",
                                               "NT")
  ) %>% 
  dplyr::group_by(vacc_area,
                  serotype_final_decision,
                  workWGS_gpsc_strain) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::mutate(
        # add vaccination status according to area
        vacc_area = case_when(
          area == "Lombok" | 
            area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
          TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
        ),
        # vacc_area = factor(vacc_area,
        #                    levels = c("no", "vaccinated")),
      ) %>% 
      dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                                   "6A", "6B",
                                                   "11A", "13",
                                                   "15B", "15C",
                                                   "NT")
      ) %>% 
      dplyr::group_by(vacc_area,
                      serotype_final_decision
      ) %>% 
      dplyr::summarise(n_all = n(), .groups = "drop")
    ,
    by = c("vacc_area", "serotype_final_decision")
  ) %>% 
  dplyr::mutate(
    percent = round(n/n_all*100, 1),
    report = paste0(n, "/", n_all,  " (", percent, "%)"),
    # adjust some values for viz
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "not assigned",
                                 "Not\nassigned",
                                 workWGS_gpsc_strain),
    label = ifelse(percent <= 25, " ", paste0(round(percent, 1), "%")),
    serotype_final_decision = ifelse(serotype_final_decision != "NT",
                                     paste0("Serotype ",
                                            serotype_final_decision),
                                     "NT"),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       "Serotype 6A", "Serotype 6B",
                                       "Serotype 19F", "Serotype 23F",
                                       
                                       # NVT
                                       "Serotype 11A", "Serotype 13",
                                       "Serotype 15B", "Serotype 15C",
                                       
                                       # NT
                                       "NT"
                                     )),
    
  ) %>% 
  dplyr::arrange(serotype_final_decision,
                 desc(percent),
  ) %>% 
  glimpse() %>% 
  ggplot(.,
         aes(x = workWGS_gpsc_strain,
             y = percent,
             fill = vacc_area)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = label),
            vjust = 0.5, size = 3,
            angle = 90, hjust = 0,
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 130)) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = "GPSC", y = "Percentage"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9.5),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.text  = element_text(size = 11),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  ) +
  facet_wrap(~ serotype_final_decision,
             ncol = 4,
             scales = "free_x")


png(file = "pictures/genData_gpsc_serobigsix_vaccArea.png",
    width = 25, height = 29, unit = "cm", res = 600)
cowplot::plot_grid(serobigsix_all, serobigsix,
                   nrow = 2,
                   rel_heights = c(0.5, 1),
                   labels = c("A", "B")
)
dev.off()

serobigsix_filter <- serobigsix %>% 
  dplyr::group_by(serotype_final_decision) %>%
  dplyr::slice_max(order_by = percent, n = 3) %>%
  ungroup() %>%
  dplyr::mutate(
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not\nassigned",
                                 "Not assigned",
                                 workWGS_gpsc_strain),
  ) %>% 
  glimpse()


serobigsix_st <- df_epi_gen_pneumo %>% 
  dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                               "6A", "6B",
                                               "11A", "13",
                                               "15B", "15C",
                                               "NT"),
                workWGS_gpsc_strain %in% unique(serobigsix_filter$workWGS_gpsc_strain)
  ) %>% 
  dplyr::mutate(
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned",
                                 "NA",
                                 workWGS_gpsc_strain),
    workWGS_MLST_pw_ST = ifelse(grepl("\\*", workWGS_MLST_pw_ST), "NA",
                                workWGS_MLST_pw_ST),
    gpsc_st = paste0(workWGS_gpsc_strain, "-", workWGS_MLST_pw_ST),
    gpsc_st = ifelse(gpsc_st == "NA-NA", "NA", gpsc_st)
  ) %>% 
  dplyr::group_by(serotype_final_decision,
                  gpsc_st) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::filter(serotype_final_decision %in% c("19F", "23F",
                                                   "6A", "6B",
                                                   "11A", "13",
                                                   "15B", "15C",
                                                   "NT"),
                    workWGS_gpsc_strain %in% unique(serobigsix_filter$workWGS_gpsc_strain)
      ) %>% 
      dplyr::mutate(
        workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned",
                                     "NA",
                                     workWGS_gpsc_strain),
        workWGS_MLST_pw_ST = ifelse(grepl("\\*", workWGS_MLST_pw_ST), "NA",
                                    workWGS_MLST_pw_ST),
        gpsc_st = paste0(workWGS_gpsc_strain, "-", workWGS_MLST_pw_ST),
        gpsc_st = ifelse(gpsc_st == "NA-NA", "NA", gpsc_st)
      ) %>% 
      dplyr::group_by(serotype_final_decision
      ) %>% 
      dplyr::summarise(n_all = n(), .groups = "drop")
    ,
    by = c("serotype_final_decision")
  ) %>% 
  dplyr::mutate(
    percent = round(n/n_all*100, 1),
    report = paste0(n, "/", n_all,  " (", percent, "%)"),
    # adjust some values for viz
    label = ifelse(percent <= 25 | percent == 100.0, " ", paste0(round(percent, 1), "%")),
    serotype_final_decision = ifelse(serotype_final_decision != "NT",
                                     paste0("Serotype ",
                                            serotype_final_decision),
                                     "NT"),
    serotype_final_decision = factor(serotype_final_decision,
                                     levels = c(
                                       # VT
                                       "Serotype 6A", "Serotype 6B",
                                       "Serotype 19F", "Serotype 23F",
                                       
                                       # NVT
                                       "Serotype 11A", "Serotype 13",
                                       "Serotype 15B", "Serotype 15C",
                                       
                                       # NT
                                       "NT"
                                     )),
  ) %>% 
  dplyr::arrange(serotype_final_decision,
                 desc(percent),
  ) %>% 
  glimpse() %>% 
  ggplot(.,
         aes(x = gpsc_st,
             y = percent,
             fill = serotype_final_decision)) +
  geom_bar(stat = "identity",
           position = "stack") +
  # geom_text(aes(label = label),
  #           vjust = -0.5, size = 3,
  #           angle = 0,
  #           check_overlap = TRUE,
  #           position = position_stack(vjust = 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     breaks = c(0, 25, 50, 75, 100),
                     limits = c(0, 100)) +
  scale_fill_viridis_d() +
  labs(x = "GPSC-ST", y = "Percentage"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.text  = element_text(size = 11),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  ) #+
  # facet_wrap(~ vacc_area,
  #            nrow = 2,
  #            scales = "free_x")

# png(file = "pictures/genData_gpsc-st_serobigsix_vaccArea.png",
#     width = 40, height = 20, unit = "cm", res = 600)
serobigsix_st
# dev.off()

png(file = "pictures/genData_gpsc_serobigsixst_vaccArea.png",
    width = 23, height = 29, unit = "cm", res = 600)
cowplot::plot_grid(serobigsix_st, serobigsix,
                   nrow = 2,
                   rel_heights = c(0.5, 1),
                   labels = c("A", "B")
)
dev.off()



# AMR analyses & viz ###########################################################
# load df_epi_gen_pneumo first
# AMR and virulence factors use pass qc samples (n = 314)
df_epi_gen_pneumo <- read.csv("inputs/genData_pneumo_with_epiData_with_final_pneumo_decision.csv") %>% 
  # dplyr::filter(workPoppunk_qc == "pass_qc") %>%
  dplyr::filter(workWGS_species_pw == "Streptococcus pneumoniae") %>% 
  dplyr::mutate(
    serotype_final_decision = case_when(
      serotype_final_decision == "mixed serotypes/serogroups" ~ "mixed serogroups",
      TRUE ~ serotype_final_decision
    ),
    serotype_final_decision = factor(serotype_final_decision,
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
    serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "NT")),
    serotype_classification_PCV15_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                          levels = c("VT", "NVT", "NT"))
  ) %>%
  glimpse()

# cumulative AMR per-PCV13 vaccine type
amr_pcv13_long <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_classification_PCV13_final_decision,
                contains("workWGS_AMR_logic_class"),
                -workWGS_AMR_logic_class_counts
  ) %>% 
  tidyr::pivot_longer(
    cols = contains("workWGS_AMR_logic_class"),
    names_to = "class",
    values_to = "logic"
  ) %>% 
  dplyr::group_by(serotype_classification_PCV13_final_decision, class, logic) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_classification_PCV13_final_decision,
                    contains("workWGS_AMR_logic_class"),
                    -workWGS_AMR_logic_class_counts
      ) %>% 
      tidyr::pivot_longer(
        cols = contains("workWGS_AMR_logic_class"),
        names_to = "class",
        values_to = "logic"
      ) %>% 
      dplyr::group_by(serotype_classification_PCV13_final_decision, class) %>%
      dplyr::summarise(count_class = n(),
                       .groups = "drop")
    ,
    by = c("serotype_classification_PCV13_final_decision", "class")
  ) %>% 
  dplyr::mutate(
    percent = count/count_class*100
  ) %>% 
  dplyr::filter(
    logic == "TRUE"
  ) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(
    class = gsub("workWGS_AMR_logic_class_", "", class)
  ) %>% 
  # view() %>% 
  glimpse()

amr_grouped <- amr_pcv13_long %>% 
  # dplyr::filter(logic == "TRUE") %>% 
  dplyr::group_by(class, logic) %>% 
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup() %>% 
  glimpse()

amr1 <- ggplot(amr_pcv13_long, aes(x = serotype_classification_PCV13_final_decision,
                                   y = percent,
                                   fill = class)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  # geom_text(aes(label = paste0(round(percentage, 2), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.3) +
  # scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(legend.position = "none")
# theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
#       legend.position = "bottom", # c(0.02, 0.75),
#       legend.direction = "horizontal",
#       legend.justification = c("centre", "top"),
#       legend.background = element_rect(fill = NA, color = NA),
#       legend.title = element_blank(),
#       legend.margin = margin(t = -10),
#       legend.spacing.y = unit(-0.3, "cm"))


# cumulative AMR per-serotype
amr_ser_long <- df_epi_gen_pneumo %>% 
  dplyr::select(serotype_final_decision,
                contains("workWGS_AMR_logic_class"),
                -workWGS_AMR_logic_class_counts
  ) %>% 
  tidyr::pivot_longer(
    cols = contains("workWGS_AMR_logic_class"),
    names_to = "class",
    values_to = "logic"
  ) %>% 
  dplyr::group_by(serotype_final_decision, class, logic) %>%
  dplyr::summarise(count = n(),
                   .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    contains("workWGS_AMR_logic_class"),
                    -workWGS_AMR_logic_class_counts
      ) %>% 
      tidyr::pivot_longer(
        cols = contains("workWGS_AMR_logic_class"),
        names_to = "class",
        values_to = "logic"
      ) %>% 
      dplyr::group_by(serotype_final_decision, class) %>%
      dplyr::summarise(count_class = n(),
                       .groups = "drop")
    ,
    by = c("serotype_final_decision", "class")
  ) %>% 
  dplyr::mutate(
    percent = count/count_class*100
  ) %>% 
  dplyr::filter(
    logic == "TRUE"
  ) %>% 
  # dplyr::group_by(serotype_final_decision, class) %>%
  # dplyr::summarise(percent = count/sum(count)*100) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>% 
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "NT" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
    ,
    relationship = "many-to-many"
  ) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(
    class = gsub("workWGS_AMR_logic_class_", "", class)
  ) %>% 
  # view() %>% 
  glimpse()

amr2 <- ggplot(amr_ser_long, aes(x = serotype_final_decision,
                                 y = percent,
                                 fill = class)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  # geom_text(aes(label = paste0(round(percentage, 2), "%")),
  #           position = position_dodge(width = 0.9),
  #           vjust = -0.3) +
  # scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Percentage", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom", # c(0.02, 0.75),
        legend.direction = "horizontal",
        legend.justification = c("centre", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -25),
        legend.spacing.y = unit(-0.3, "cm"))


png(file = "pictures/genData_amr_filterPneumo.png",
    width = 29, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(amr1, amr2,
                   nrow = 2,
                   labels = c("A", "B"))
dev.off()



# numeric AMR counts ###########################################################
df_amr_counts_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(workWGS_AMR_logic_class_counts, serotype_final_decision) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::group_by(serotype_final_decision) %>%
      dplyr::summarise(count_serotype = n())
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::mutate(
    percentage = ifelse(workWGS_AMR_logic_class_counts == "MDR",
                        count/count_serotype*100,
                        NA_real_),
    y_pos = count_serotype + 2
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>%
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "NT" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
  ) %>%
  dplyr::mutate(
    workWGS_AMR_logic_class_counts = as.character(workWGS_AMR_logic_class_counts),
    workWGS_AMR_logic_class_counts = factor(workWGS_AMR_logic_class_counts,
                                            levels = c("7", "6", "5", "4", "3",
                                                       "2", "1", "0"))
  ) %>% 
  dplyr::distinct() %>% 
  # view() %>%
  glimpse()

# test plot
mdr1 <- ggplot(df_amr_counts_summary, aes(x = serotype_final_decision,
                                          y = count,
                                          fill = workWGS_AMR_logic_class_counts)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Count", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.22, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm")) +
  guides(fill = guide_legend(ncol = 2))

df_amr_counts_classification_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(workWGS_AMR_logic_class_counts) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()

df_amr_counts_classification_perArea_summary <- df_epi_gen_pneumo %>% 
  dplyr::count(workWGS_AMR_logic_class_counts, area) %>%
  dplyr::mutate(percentage = n / sum(n) * 100) %>% 
  glimpse()



# MDR flag
df_amr_mdr_summary <- df_epi_gen_pneumo %>% 
  dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
  dplyr::summarise(count = n()) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
      dplyr::group_by(serotype_final_decision) %>%
      dplyr::summarise(count_serotype = n())
    ,
    by = "serotype_final_decision"
  ) %>% 
  dplyr::mutate(
    percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                        count/count_serotype*100,
                        NA_real_),
    y_pos = count_serotype + 3
  ) %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>%
      dplyr::select(serotype_final_decision,
                    serotype_classification_PCV13_final_decision) %>%
      dplyr::mutate(
        # slightly change classifications
        serotype_classification_PCV13_final_decision = case_when(
          serotype_classification_PCV13_final_decision == "NT" ~ " ",
          TRUE ~ serotype_classification_PCV13_final_decision
        ),
        serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                              levels = c("VT", "NVT", " "))
      )
    ,
    by = "serotype_final_decision"
  ) %>%
  dplyr::distinct() %>% 
  # view() %>%
  glimpse()

# test plot
mdr2 <- ggplot(df_amr_mdr_summary, aes(x = serotype_final_decision,
                                       y = count,
                                       fill = workWGS_AMR_MDR_flag)) +
  # geom_line(size = 1.5) +
  geom_bar(stat = "identity", position = position_stack()) +
  # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  facet_grid(~ serotype_classification_PCV13_final_decision,
             scales = "free_x",
             space = "free_x"
  ) +
  geom_text(aes(x = serotype_final_decision,
                y = y_pos,
                label = ifelse(is.na(percentage),
                               NA,
                               paste0(round(percentage, 1), "%"))),
            # position = position_stack(vjust = 1.3),
            angle = 90,
            check_overlap = TRUE
  ) +
  scale_fill_manual(values = c(col_map)) +
  labs(x = " ", y = "Count", 
       # title = "All Serotypes"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = c(0.22, 0.75),
        legend.direction = "vertical",
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.margin = margin(t = -50),
        legend.spacing.y = unit(-0.3, "cm"))

png(file = "pictures/genData_mdr_filterPneumo.png",
    width = 29, height = 25, unit = "cm", res = 600)
cowplot::plot_grid(mdr1, mdr2,
                   nrow = 2,
                   rel_heights = c(1, 1.3),
                   labels = c("A", "B"))
dev.off()


# deep dive of MDR in area #####################################################
area <- unique(df_epi_gen_pneumo$area)
plotStore_area <- list()

for(a in area){
  plot <- df_epi_gen_pneumo %>%
    dplyr::filter(area == a) %>% 
    dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
    dplyr::summarise(count = n()) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>% 
        dplyr::filter(area == a) %>% 
        dplyr::group_by(serotype_final_decision) %>%
        dplyr::summarise(count_serotype = n())
      ,
      by = "serotype_final_decision"
    ) %>% 
    dplyr::mutate(
      percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                          count/count_serotype*100,
                          NA_real_),
      y_pos = count_serotype
    ) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>%
        dplyr::select(serotype_final_decision,
                      serotype_classification_PCV13_final_decision) %>%
        dplyr::mutate(
          # slightly change classifications
          serotype_classification_PCV13_final_decision = case_when(
            serotype_classification_PCV13_final_decision == "NT" ~ " ",
            TRUE ~ serotype_classification_PCV13_final_decision
          ),
          serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                levels = c("VT", "NVT", " "))
        )
      ,
      by = "serotype_final_decision"
    ) %>%
    dplyr::distinct() %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = count,
               fill = workWGS_AMR_MDR_flag)) +
    # geom_line(size = 1.5) +
    geom_bar(stat = "identity", position = position_stack()) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    geom_text(aes(x = serotype_final_decision,
                  y = y_pos,
                  label = ifelse(is.na(percentage),
                                 NA,
                                 paste0(round(percentage, 1), "%"))),
              # position = position_stack(vjust = 1.3),
              angle = 90,
              # vjust = -3,
              check_overlap = TRUE
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = " ", y = "Count", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.position = "none",
          legend.direction = "vertical",
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_area[[a]] <- plot
}


png(file = "pictures/genData_mdr_filterPneumo_area.png",
    width = 29, height = 46, unit = "cm", res = 600)
cowplot::plot_grid(plotStore_area[[1]],
                   plotStore_area[[3]],
                   plotStore_area[[4]],
                   plotStore_area[[2]],
                   nrow = 4)
dev.off()


# deep dive of MDR in PCV13-implemented area ###################################
vaccination_status_area <- c("PCV13-implemented area (Lombok & Sumbawa)",
                             "Pre-implemented area (Minahasa & Sorong)")
plotStore_vaccArea <- list()

for(a in vaccination_status_area){
  plot <- df_epi_gen_pneumo %>%
    dplyr::mutate(vaccination_status_area = case_when(
      area == "Lombok" |
        area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
      TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
    )
    ) %>% 
    dplyr::filter(vaccination_status_area == a) %>% 
    dplyr::group_by(workWGS_AMR_MDR_flag, serotype_final_decision) %>%
    dplyr::summarise(count = n()) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>% 
        dplyr::mutate(vaccination_status_area = case_when(
          area == "Lombok" |
            area == "Sumbawa" ~ "PCV13-implemented area (Lombok & Sumbawa)",
          TRUE ~ "Pre-implemented area (Minahasa & Sorong)"
        )
        ) %>% 
        dplyr::filter(vaccination_status_area == a) %>% 
        dplyr::group_by(serotype_final_decision) %>%
        dplyr::summarise(count_serotype = n())
      ,
      by = "serotype_final_decision"
    ) %>% 
    dplyr::mutate(
      percentage = ifelse(workWGS_AMR_MDR_flag == "MDR",
                          count/count_serotype*100,
                          NA_real_),
      y_pos = count_serotype
    ) %>% 
    dplyr::left_join(
      df_epi_gen_pneumo %>%
        dplyr::select(serotype_final_decision,
                      serotype_classification_PCV13_final_decision) %>%
        dplyr::mutate(
          # slightly change classifications
          serotype_classification_PCV13_final_decision = case_when(
            serotype_classification_PCV13_final_decision == "NT" ~ " ",
            TRUE ~ serotype_classification_PCV13_final_decision
          ),
          serotype_classification_PCV13_final_decision = factor(serotype_classification_PCV13_final_decision,
                                                                levels = c("VT", "NVT", " "))
        )
      ,
      by = "serotype_final_decision"
    ) %>%
    dplyr::distinct() %>% 
    ggplot(.,
           aes(x = serotype_final_decision,
               y = count,
               fill = workWGS_AMR_MDR_flag)) +
    # geom_line(size = 1.5) +
    geom_bar(stat = "identity", position = position_stack()) +
    # scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    facet_grid(~ serotype_classification_PCV13_final_decision,
               scales = "free_x",
               space = "free_x"
    ) +
    geom_text(aes(x = serotype_final_decision,
                  y = y_pos,
                  label = ifelse(is.na(percentage),
                                 NA,
                                 paste0(round(percentage, 1), "%"))),
              # position = position_stack(vjust = 1.3),
              angle = 90,
              # vjust = -3,
              check_overlap = TRUE
    ) +
    scale_fill_manual(values = c(col_map)) +
    labs(x = " ", y = "Count", 
         # title = "All Serotypes"
    ) +
    theme_bw() +
    ggtitle(a) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.position = "none",
          legend.direction = "vertical",
          legend.justification = c("left", "top"),
          legend.background = element_rect(fill = NA, color = NA),
          legend.title = element_blank(),
          legend.margin = margin(t = -50),
          legend.spacing.y = unit(-0.3, "cm"))
  
  # redefine plots
  plotStore_vaccArea[[a]] <- plot
}

png(file = "pictures/genData_mdr_filterPneumo_vaccArea.png",
    width = 29, height = 23, unit = "cm", res = 600)
cowplot::plot_grid(plotlist = plotStore_vaccArea,
                   nrow = 2)
dev.off()
