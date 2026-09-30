serogpsc_group <- df_epi_gen_pneumo %>% 
  dplyr::group_by(workWGS_gpsc_strain,
                  serotype_final_decision
                  ) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  # dplyr::group_by(workWGS_gpsc_strain) %>%
  # dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::filter(n > 1) %>%
  arrange(desc(n)) %>%
  dplyr::mutate(
    percent = round(n/606*100, 1)
  ) %>% 
  glimpse()



serogpsc_st <- df_epi_gen_pneumo %>% 
  dplyr::filter(workWGS_gpsc_strain %in% unique(serogpsc_group$workWGS_gpsc_strain)) %>% 
  # dplyr::mutate(
  #   gpsc_st = paste0()
  # ) %>% 
  dplyr::group_by(serotype_final_decision,
                  workWGS_gpsc_strain) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
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
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned",
                                 "Not\nassigned",
                                 workWGS_gpsc_strain),
    label = ifelse(percent <= 25, " ", paste0(round(percent, 1), "%")),
    serotype_final_decision = ifelse(serotype_final_decision != "NT",
                                     paste0("Serotype ",
                                            serotype_final_decision),
                                     "NT"),
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
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.text  = element_text(size = 11),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )


serogpsc_more <- df_epi_gen_pneumo %>% 
  dplyr::filter(workWGS_gpsc_strain %in% unique(serogpsc_group$workWGS_gpsc_strain)) %>% 
  dplyr::group_by(serotype_final_decision,
                  workWGS_gpsc_strain) %>% 
  dplyr::summarise(n = n(), .groups = "drop") %>% 
  dplyr::left_join(
    df_epi_gen_pneumo %>% 
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
    workWGS_gpsc_strain = ifelse(workWGS_gpsc_strain == "Not assigned",
                                 "Not\nassigned",
                                 workWGS_gpsc_strain),
    label = ifelse(percent <= 25, " ", paste0(round(percent, 1), "%")),
    serotype_final_decision = ifelse(serotype_final_decision != "NT",
                                     paste0("Serotype ",
                                            serotype_final_decision),
                                     "NT"),
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
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal",
        # legend.justification = c("left", "top"),
        legend.background = element_rect(fill = NA, color = NA),
        legend.title = element_blank(),
        legend.text  = element_text(size = 11),
        # legend.margin = margin(t = -50),
        # legend.spacing.y = unit(-0.3, "cm")
  )

