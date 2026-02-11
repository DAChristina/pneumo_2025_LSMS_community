# need to be loaded:
# 1. metadata
# 2. blastx result

library(tidyverse)

stats <- read.csv("inputs/epiData_eng.csv") %>% 
  dplyr::select(specimen_id, area) %>% 
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
  dplyr::transmute(
    specimen_id = specimen_id,
    area = area,
    workContigs_report = case_when(
      is.na(workContigs_report) &
        workAlignments_report == "available with DC"
      ~ "but alignments available",
      TRUE ~ workContigs_report
    )
  ) %>% 
  glimpse()


# lytA piaB SP2020 blast result ################################################
blast1 <- read.table("outputs/result_blast/tblastn_tabular_lytA_piaB_SP2020.txt",
                     header = F, sep = "\t") %>% 
  stats::setNames(c("file_name", "qseqid", "sseqid",
                    "pident", "length", "qlen",
                    "sstart", "send",
                    "qcovs", "qcovhsp",
                    "evalue", "bitscore")) %>% 
  dplyr::left_join(
    stats
    ,
    by = c("file_name" = "specimen_id")
  ) %>%
  dplyr::mutate(
    qseqid = case_when(
      qseqid == "WP_000405234.1" ~ "lytA",
      qseqid == "WP_001180357.1" ~ "piaB",
      qseqid == "WP_000105270.1" ~ "SP2020",
      TRUE ~ qseqid
    ),
    threshold = case_when( 
      (qseqid == "lytA" & 
        evalue <= 1e-20 & 
        pident >= 95 & 
        qcovhsp >= 90) | 
      (qseqid == "piaB" & 
         evalue <= 1e-20 & 
         pident >= 85 & 
         qcovhsp >= 80) | 
      (qseqid == "SP2020" & 
         evalue <= 1e-20 & 
         pident >= 95 & 
         qcovhsp >= 90) 
    ~ "pass",
    TRUE ~ NA
    )
  ) %>%
  # dplyr::filter(
  #   threshold == "pass"
  # ) %>%
  dplyr::distinct(file_name, qseqid, .keep_all = T) %>%
  dplyr::arrange(file_name) %>%
  tidyr::pivot_wider(
    id_cols = c("file_name"),
    names_from = "qseqid",
    values_from = "threshold"
  ) %>% 
  dplyr::mutate(
    workBLAST_species_decision = case_when(
      (lytA == "pass" | SP2020 == "pass")  & 
        piaB == "pass" 
      ~ "pneumo",
      (lytA == "pass" & SP2020 == "pass")  & 
        is.na(piaB) 
      ~ "non-capsulated pneumo",
      is.na(lytA) & is.na(SP2020) & 
        piaB == "pass"
      ~ "pneumo, need verification",
      is.na(lytA) & is.na(SP2020) & 
        is.na(piaB)
      ~ "not pneumo",
      
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::rename(
    workBLAST_lytA = lytA,
    workBLAST_piaB = piaB,
    workBLAST_SP2020 = SP2020,
  ) %>% 
  glimpse()

# sanity check
# unique(blast1$qseqid)
# 
# test <- blast1 %>% 
#   dplyr::filter(qseqid == "LytA")
# hist(test$pident)
# hist(test$qcovs)
# hist(test$qcovhsp)


write.csv(blast1, "inputs/blastData_all.csv", row.names = F)


# test solo sample
read.table("outputs/result_blast/tblastn_tabular_lytA_piaB_SP2020.txt",
           header = F, sep = "\t") %>% 
  stats::setNames(c("file_name", "qseqid", "sseqid",
                    "pident", "length", "qlen",
                    "sstart", "send",
                    "qcovs", "qcovhsp",
                    "evalue", "bitscore")) %>% 
  dplyr::left_join(
    stats
    ,
    by = c("file_name" = "specimen_id")
  ) %>%
  dplyr::mutate(
    qseqid = case_when(
      qseqid == "WP_000405234.1" ~ "lytA",
      qseqid == "WP_001180357.1" ~ "piaB",
      qseqid == "WP_000105270.1" ~ "SP2020",
      TRUE ~ qseqid
    ),
    threshold = case_when( 
      (qseqid == "lytA" & 
         evalue <= 1e-20 & 
         pident >= 95 & 
         qcovhsp >= 90) | 
        (qseqid == "piaB" & 
           evalue <= 1e-20 & 
           pident >= 85 & 
           qcovhsp >= 80) | 
        (qseqid == "SP2020" & 
           evalue <= 1e-20 & 
           pident >= 95 & 
           qcovhsp >= 90) 
      ~ "pass",
      TRUE ~ NA
    )
  ) %>%
  dplyr::filter(
    file_name == "SWQ_365"
  ) %>%
  view() %>% 
  glimpse()


# a slight modification for all wgs data report ################################
report_wgs_data <- blast1 %>% 
  dplyr::transmute(
    file_name = paste0("Streptococcus_pneumoniae_", file_name),
    priority = case_when(
      workBLAST_species_decision == "pneumo" |
        workBLAST_species_decision == "non-capsulated pneumo"
      ~ "prioritas (positif pneumo by lytA&SP2020)",
      TRUE ~ "bukan pneumo"
    )
  ) %>% 
  # dplyr::select()
  glimpse()

write.csv(report_wgs_data, "inputs/list_all_WGS_data_in_DC.csv", row.names = F)


