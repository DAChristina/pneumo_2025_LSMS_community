library(tidyverse)

# Data cleaning process for epiData ############################################
df_epi_lombok <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver5.xlsx",
                                    sheet = "Lombok") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Lombok",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
# across(everything(), as.character),
# across(everything(), tolower)) %>% 
# dplyr::select(-contains("koding"),-contains("Ya="))

# Detect duplicated IDs
df_epi_lombok_duplicated_ids <- df_epi_lombok %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  view()

write.csv(df_epi_lombok, "raw_data/temporary_df_epi_lombok.csv",
          row.names = F)

df_epi_sumbawa <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver5.xlsx",
                                     sheet = "Sumbawa") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub("-", "_", SPECIMEN_ID),
                area = "Sumbawa",
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)))
# across(everything(), as.character),
# across(everything(), tolower)) %>% 
# dplyr::select(-contains("koding"),-contains("Ya="))

# Detect duplicated IDs
df_epi_sumbawa_duplicated_ids <- df_epi_sumbawa %>% 
  dplyr::count(SPECIMEN_ID) %>% 
  dplyr::mutate(category = case_when(
    n == 2 ~ "Duplicated",
    n == 3 ~ "Triplicated",
    n == 4 ~ "Quadruplicated",
    n > 4 ~ "More than Quadruplicated"
  )) %>% 
  dplyr::filter(SPECIMEN_ID > 1) %>% 
  # view() %>% 
  glimpse()

write.csv(df_epi_sumbawa, "raw_data/temporary_df_epi_sumbawa.csv",
          row.names = F)

setdiff(names(df_epi_lombok), names(df_epi_sumbawa))
setdiff(names(df_epi_sumbawa), names(df_epi_lombok))


df_epi_manado <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver5.xlsx",
                                 sheet = "Manado") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub(" ", "_", SPECIMEN_ID),
                area = "Manado",
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
  dplyr::filter(n > 1) %>% 
  # view() %>% 
  glimpse()

write.csv(df_epi_manado, "raw_data/temporary_df_epi_manado.csv",
          row.names = F)

df_epi_sorong <- readxl::read_excel("raw_data/DATABASE PENELITIAN PNEUMOKOKUS (Manado, Lombok, Sorong, Sumbawa)_ver5.xlsx",
                                    sheet = "Papua") %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(SPECIMEN_ID = gsub("-", "_", SPECIMEN_ID),
                area = "Sorong",
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
  dplyr::filter(n > 1) %>% 
  # view() %>% 
  glimpse()

write.csv(df_epi_sorong, "raw_data/temporary_df_epi_sorong.csv",
          row.names = F)

setdiff(names(df_epi_manado), names(df_epi_sorong))
setdiff(names(df_epi_sorong), names(df_epi_manado))

# In the end, I manually merge Manado & Sorong dfs Column differences occur with
# various values including shifted columns Do not trust coded columns & the "how
# many vaccination" columns (they were manually coded). I can't trust n
# vaccination columns because there are date columns available; I manually
# corrected n vaccination calculations.

# I manually inspect NA values based on data types (numeric & categorical) and
# many aspects from different columns. < 2% NAs filled with median/modes. 2 NAs
# in "tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut"
# is filled with median/mode = 3 1 NA in "jika_ya_berapa_jika_0_isi_"
# (healthcare visit); "0" ommited and filled with median/mode = 1 because child
# is considered ill (batuk)

# Cleaned data is stored in temporary_df_epi_lombok_sumbawa_manual_combine_row.csv:
df_epi_merged <- read.csv("raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row.csv") %>% 
  dplyr::select(-contains(c("kode_", "Kode_", "koding_","Koding_"))) %>%
  dplyr::rename_with(~ tolower(gsub("[^[:alnum:]_]", "", .x))) %>% 
  dplyr::rename_all(~stringr::str_replace_all(., " ", "_")) %>% 
  dplyr::mutate(specimen_id = gsub(" ", "_", specimen_id),
                across(where(is.character), ~ na_if(., "N/A")),
                across(where(is.character), ~ if_else(. == "-", "tidak", .)),
                across(everything(), ~ ifelse(. == "ya", "yes", 
                                              ifelse(. == "tidak", "no",
                                                     ifelse(. == "tidak tahu", "unknown", .)))))

# Test weird unique value
lapply(df_epi_merged, unique)
df_epi_merged_summarise <- df_epi_merged %>% 
  dplyr::summarise(across(everything(), ~ list(table(.)))) %>% 
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  # view() %>% 
  glimpse()

# I conducted manual data cleaning for inputted values
# then, pick some interesting columns to be analysed
df_epi_clean <- df_epi_merged %>% 
  dplyr::select(specimen_id, s_pneumoniae_suspect_culture_colony,
                optochin, s_pneumoniae_culture_result, wgs_result11, wgs_result12,
                serotype_wgs, # will be modified to VTs and NVTs
                age_month, # will be modified soon and classified according to some ageGroups
                area,
                jenis_kelamin, suku, # based on the coded value, samawa == sumbawa (10)
                apakah_anak_tersebut_pernah_diberi_asi,
                jika_ya_apakah_anak_tersebut_masih_diberi_asi,
                tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut, # I change "-" to NA, then filled with median/mode = 3
                kecuali_anak_tersebut_berapa_anak_berusia_5_tahun_yang_tinggal_serumah_dengan_anak_tersebut,
                jumlah_anak_berusia_1_tahun_yang_tinggal_serumah, # <1 I change "-", or "tidak" to 0
                jumlah_anak_berusia_antara_1_sampai_dengan_2_tahun_yang_tinggal_serumah, # 1<2, I change "-", or "tidak" to 0
                jumlah_anak_berusia_24_tahun_yang_tinggal_serumah, # 2-4 I change "-", or "tidak" to 0; 2 IDs with "1, 5 tahun" is changed to "1"
                kecuali_anak_tersebut_berapa_anak_yang_berusia_5_tahun_yang_tidur_dalam_1_kamar_dengan_anak_tersebut, # <5 I change "-", or "tidak" to 0
                apakah_anak_pergi_ke_sekolah_taman_kanakkanak_playgroup_pendidikan_anak_usia_dini_ppa_pendidikan_pengembangan_anak_sekolah_minggu_atau_tempat_penitipan_anak_dengan_peserta_lebih_dari_5_orang_anak_lain,
                apakah_anak_tersebut_menghabiskan_setidaknya_1_hari_dalam_seminggu_bergaulberdekatan_dengan_anak_lain_yang_berusia_5_tahun_yang_tidak_tinggal_serumah_dengan_anak_tersebut,
                apakah_di_dalam_rumah_ada_yang_merokok_di_depan_anak,
                
                atap_rumah_terbuat_dari, # is "gaiteng" a typographical error of "genteng" (5) or beton (3)
                bangunan_rumah_terbuat_dari,
                tipe_jendela_rumah__tertutup_dengan,
                sumber_bahan_bakar_untuk_memasak,
                dimana_biasanya_anda_memasak,
                apakah_anak_tersebut_pernah_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini,
                berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit, # what is "H" and "="? I change those to "0"
                sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap, # I corrected many variations of "pneumonia" such as "pneuminia" and "pneumoni"
                
                # specified to illness
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
                apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
                
                tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
                jika_ya_berapa_jika_tidak_isi_, # 1 NA filled with median/mode = 1; child is considered ill (batuk)
                alasan_anak_mengunjungi_fasilitas_kesehatan,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
                apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
                apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
                jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_, # review needed, too many categorical values; I change "-" as 0
                
                # illness in past 24h
                # dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami, # review needed
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_batuk,
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_hidung_ingusan,
                dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_kesulitan_bernapas,
                
                # antibiotic usage
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
                apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
                
                # vaccination
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib, # I can't trust the calculations, I inspect n from dates
                sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib_dc,
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd, # I can't trust the calculations, I inspect n from dates
                sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd_dc
                ) %>% 
  dplyr::mutate(
    # generate age classification
    age_year = case_when(
      age_month < 13 ~ 1,
      age_month >= 13 & age_month < 25 ~ 2,
      age_month >= 25 & age_month < 37 ~ 3,
      age_month >= 37 & age_month < 49 ~ 4,
      age_month >= 49 & age_month < 61 ~ 5
    ),
    age_year_2groups = case_when(
      age_month < 13 ~ "1 and below",
      age_month >= 13 ~ "more than 1"
    ),
    age_year_3groups = case_when(
      age_month < 13 ~ "< 1 year old",
      age_month >= 13 & age_month < 25 ~ "1-2 years old",
      age_month >= 25 & age_month < 61 ~ "3-5 years old",
    ),
    # generate VTs and NVTs according to PCV13
    serotype_wgs = toupper(serotype_wgs),
    serotype_classification_PCV13 = case_when(
      serotype_wgs %in% c("1", "3", "4", "5", "6A", "6B", "7F", 
                          "9V", "14", "18C", "19A", "19F", "23F") ~ "VT",
      serotype_wgs == "UNTYPABLE" ~ "UNTYPABLE",
      !is.na(serotype_wgs) ~ "NVT",
      TRUE ~ NA_character_
    )
  ) %>% 
  # rename epiData
  dplyr::rename(
    sex = jenis_kelamin,
    tribe = suku,
    workLab_culture_suspect = s_pneumoniae_suspect_culture_colony,
    workLab_culture_result = s_pneumoniae_culture_result,
    workLab_optochin = optochin,
    workWGS_success_failed = wgs_result11,
    workWGS_species = wgs_result12,
    workWGS_serotype = serotype_wgs,
    workWGS_serotype_classification_PCV13 = serotype_classification_PCV13,
    breastMilk_given = apakah_anak_tersebut_pernah_diberi_asi,
    breastMilk_still_being_given = jika_ya_apakah_anak_tersebut_masih_diberi_asi,
    nTotal_people = tidak_termasuk_anak_tersebut_berapa_orang_yang_tinggal_1_rumah_dengan_anak_tersebut,
    nTotal_child_5yo_andBelow = kecuali_anak_tersebut_berapa_anak_berusia_5_tahun_yang_tinggal_serumah_dengan_anak_tersebut,
    n_child_1yo_andBelow = jumlah_anak_berusia_1_tahun_yang_tinggal_serumah,
    n_child_1to2yo = jumlah_anak_berusia_antara_1_sampai_dengan_2_tahun_yang_tinggal_serumah,
    n_child_2to4yo = jumlah_anak_berusia_24_tahun_yang_tinggal_serumah,
    nTotal_child_5yo_andBelow_sleep = kecuali_anak_tersebut_berapa_anak_yang_berusia_5_tahun_yang_tidur_dalam_1_kamar_dengan_anak_tersebut,
    contact_kindergarten = apakah_anak_pergi_ke_sekolah_taman_kanakkanak_playgroup_pendidikan_anak_usia_dini_ppa_pendidikan_pengembangan_anak_sekolah_minggu_atau_tempat_penitipan_anak_dengan_peserta_lebih_dari_5_orang_anak_lain,
    contact_otherChildren = apakah_anak_tersebut_menghabiskan_setidaknya_1_hari_dalam_seminggu_bergaulberdekatan_dengan_anak_lain_yang_berusia_5_tahun_yang_tidak_tinggal_serumah_dengan_anak_tersebut,
    contact_cigarettes = apakah_di_dalam_rumah_ada_yang_merokok_di_depan_anak,
    house_roof = atap_rumah_terbuat_dari,
    house_building = bangunan_rumah_terbuat_dari,
    house_window = tipe_jendela_rumah__tertutup_dengan,
    contact_cooking_fuel = sumber_bahan_bakar_untuk_memasak,
    contact_cooking_place = dimana_biasanya_anda_memasak,
    hospitalised_last_3mo = apakah_anak_tersebut_pernah_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini,
    hospitalised_last_3mo_n = berapa_kali_anak_tersebut_dirawat_inap_dalam_3_bulan_terakhir_ini_____kali_dirawat_di_rumah_sakit,
    hospitalised_last_3mo_illness = sakit_di_derita_anak_yang_mengharuskan_anak_di_rawat_inap,
    hospitalised_last_3mo_illness_pneumonia = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_pneumonia,
    hospitalised_last_3mo_illness_diarrhoea = apakah_sakit_diderita_anak_tersebut_yang_mengharuskan_anak_dirawat_inap_di_rumah_sakit_dalam_3_bulan_terakhir_ini_diare,
    healthcareVisit_last_3mo = tidak_termasuk_rawat_inap_di_rumah_sakit_apakah_anak_tersebut_mengunjungi_fasilitas_kesehatan_klinik_rumah_sakit_puskesmas_dalam_3_bulan_terakhir_ini,
    healthcareVisit_last_3mo_n = jika_ya_berapa_jika_tidak_isi_,
    healthcareVisit_last_3mo_reason = alasan_anak_mengunjungi_fasilitas_kesehatan,
    healthcareVisit_last_3mo_reason_cough = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_batuk,
    healthcareVisit_last_3mo_reason_diarrhoea = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_diare,
    healthcareVisit_last_3mo_reason_rash = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_ruam,
    healthcareVisit_last_3mo_reason_others = apakah_alasan_anak_tersebut_mengunjungi_fasilitas_kesehatan_tersebut_lainlain,
    illness_past3days_fever = apakah_anak_sedangpernah_mengalami_demam_dalam_3_hari_terakhir_ini,
    illness_past3days_fever_nDays = jika_ya_berapa_hari_anak_tersebut_mengalami_demam_jika_tidak_isi_,
    # illness_past24h = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami,
    illness_past24h_cough = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_batuk,
    illness_past24h_runny_nose = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_hidung_ingusan,
    illness_past24h_difficulty_breathing = dalam_waktu_24_jam_terakhir_ini_apakah_anak_tersebut_mengalami_kesulitan_bernapas,
    antibiotic_past3days = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_3_hari_terakhir_ini,
    antibiotic_past1mo = apakah_anak_tersebut_pernah_diberi_obat_antibiotik_1_bulan_terakhir_ini,
    vaccination_hibpentavalent_n = sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib,
    vaccination_hibpentavalent_dc_n = sudah_berapa_kali_anak_anda_diberi_vaksin_haemophilus_influenzae_hibpentavalent_dtphbhib_dc,
    vaccination_pcv13_n = sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd,
    vaccination_pcv13_dc_n = sudah_berapa_kali_anak_anda_diberi_pneumococcal_conjugate_vaccine_13_pcv13_vaksin_pneumokokus_vaksin_pneumokokus_konjugasi_vaksin_pneumokokus_ipd_dc
    
  ) %>% 
  # generate re-grouping imbalanced columns
  dplyr::mutate(
    breastFeed_compiled = case_when(
      breastMilk_given == "yes" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "yes" & breastMilk_still_being_given == "no" ~ "ever breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "no" ~ "never breastfeed"
    ),
    house_roof_regroup = case_when(
      house_roof %in% c("batako", "beton", "genteng logam") ~ "others",
      TRUE ~ house_roof
    ),
    house_building_regroup = case_when(
      house_building %in% c("batu", "batu bata", "batu  bata") ~ "batu bata",
      house_building %in% c("anyaman bambu", "bambu", "kayu", "triplek") ~ "bambu/triplek",
      TRUE ~ house_building
    ),
    house_window_regroup = case_when(
      house_window %in% c("bambu", "kayu", "tidak ada/terbuka") ~ "bambu/kayu/terbuka",
      TRUE ~ house_window
    ),
    nTotal_people_regroup = case_when(
      is.na(nTotal_people) | nTotal_people < 4 ~ "1-3 (low)",
      nTotal_people >= 4 & nTotal_people < 7 ~ "4-6 (moderate)",
      nTotal_people >= 7 ~ ">6 (high)"
    ),
    nTotal_child_5yo_andBelow_regroup = case_when(
      nTotal_child_5yo_andBelow == 0 ~ "0",
      nTotal_child_5yo_andBelow >= 0 ~ "1-4 children"
    ),
    nTotal_child_5yo_andBelow_sleep_regroup = case_when(
      nTotal_child_5yo_andBelow_sleep == 0 ~ "0",
      nTotal_child_5yo_andBelow_sleep >= 0 ~ "1-3 children"
    ),
    illness_past3days_fever_regroup = case_when(
      illness_past3days_fever == "unknown" ~ "no",
      TRUE ~ illness_past3days_fever
    ),
    # illness_past3days_fever_nDays_regroup = case_when(
    #   illness_past3days_fever_nDays == "no" ~ "0",
    #   illness_past3days_fever_nDays == "yes" ~ "2", # mode
    #   TRUE ~ illness_past3days_fever_nDays
    # ),
    illness_past3days_fever_nDays_regroup = illness_past3days_fever_nDays,
    illness_past24h_cough = case_when(
      is.na(illness_past24h_cough) ~ "no",
      TRUE ~ illness_past24h_cough
    ),
    illness_past24h_runny_nose = case_when(
      is.na(illness_past24h_runny_nose) ~ "no",
      TRUE ~ illness_past24h_runny_nose
    ),
    illness_past24h_difficulty_breathing = case_when(
      is.na(illness_past24h_difficulty_breathing) ~ "no",
      TRUE ~ illness_past24h_difficulty_breathing
    ),
    illness_past24h_difficulty_compiled = case_when(
      illness_past24h_cough == "no" & illness_past24h_runny_nose == "no" & illness_past24h_difficulty_breathing == "no" ~ "no",
      TRUE ~ "≥ 1 respiratory illness",
    ),
    vaccination_hibpentavalent_dc_n_regroup = case_when(
      vaccination_hibpentavalent_dc_n < 4 ~ "1-3 mandatory",
      vaccination_hibpentavalent_dc_n >= 4 ~ "4 booster"
    ),
    vaccination_pcv13_dc_n_regroup = case_when(
      vaccination_pcv13_dc_n == 0 ~ "0 not yet",
      vaccination_pcv13_dc_n < 3 ~ "1-2 mandatory",
      vaccination_pcv13_dc_n >= 3 ~ "3-4 booster"
    )
  ) #%>% 
  # combine to available fasta
  # dplyr::left_join(read.table("raw_data/test_available_fasta_renamed.txt", header = FALSE) %>% 
  #                    dplyr::mutate(workFasta_check = "Accepted_by_DC",
  #                                  workFasta_name = gsub(".fasta", "", V1),
  #                                  specimen_id = gsub("Streptococcus_pneumoniae_", "", workFasta_name),
  #                                  workFasta_name_with_extension = V1) %>% 
  #                    dplyr::select(-V1)
  #                  ,
  #                  by = "specimen_id"
  # )


column_names <- setdiff(names(df_epi_clean), "workLab_culture_result")
for (column in column_names){
  df_summary <- df_epi_clean %>% 
    dplyr::group_by(!!sym(column), workLab_culture_result) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>% 
    glimpse()
}

write.csv(df_epi_clean, "raw_data/temporary_df_epi_lombok_sumbawa_manual_combine_row_cleaned.csv", row.names = F)

df_epi_coded <- df_epi_clean %>% 
  dplyr::select(-contains("work"))

# check columns with NA
cols_with_na <- colnames(df_epi_coded)[colSums(is.na(df_epi_coded)) > 0]
cols_with_na_sums <- df_epi_coded %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  # transpose
  tidyr::pivot_longer(everything(),
                      names_to = "columns", values_to = "NAs") %>% 
  dplyr::filter(!str_detect(columns, "work"),
                NAs != 0)
view(cols_with_na_sums)

get_mmm <- function(x) {
  mea_v <- mean(x, na.rm = TRUE)
  med_v <- median(x, na.rm = TRUE)
  
  uniq_x <- unique(na.omit(x))
  mod_v <- uniq_x[which.max(tabulate(match(x, uniq_x)))]
  
  return(list(mean = mea_v,
              median = med_v,
              mode = mod_v))
  }

# Crucial columns with NA:
# nTotal_people (1)
get_mmm(df_epi_coded$nTotal_people)
# healthcareVisit_last_3mo_n (2)
filtered_0 <- df_epi_coded %>% 
  dplyr::filter(healthcareVisit_last_3mo_n != 0)
get_mmm(filtered_0$healthcareVisit_last_3mo_n)

write.csv(df_epi_coded, "inputs/epiData.csv", row.names = F)


# Further data cleaning for values #############################################
# I change Indonesian to English
df_epi_coded_eng <- read.csv("inputs/epiData.csv") %>% 
  dplyr::mutate(
    tribe = tolower(tribe)
  ) %>% 
  dplyr::transmute(
    specimen_id = specimen_id,
    age_month = age_month,
    age_year = age_year,
    age_year_3groups = age_year_3groups,
    area = area,
    sex = case_when(
      sex == "laki-laki" ~ "male",
      TRUE ~ "female"
    ),
    tribe = tribe,
    contact_kindergarten = contact_kindergarten,
    contact_otherChildren = contact_otherChildren,
    contact_cigarettes = contact_cigarettes,
    contact_cooking_fuel = case_when(
      contact_cooking_fuel == "lpg/gas alam" ~ "natural gas",
      contact_cooking_fuel == "kayu" ~ "wood",
      contact_cooking_fuel == "minyak tanah" ~ "kerosene",
      TRUE ~ contact_cooking_fuel
      ),
    contact_cooking_place = case_when(
      contact_cooking_place == "di dalam rumah" ~ "indoor",
      contact_cooking_place == "di luar rumah" ~ "outdoor",
      ),
    
    breastFeed_compiled = case_when(
      breastMilk_given == "yes" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "yes" ~ "currently breastfeed",
      breastMilk_given == "yes" & breastMilk_still_being_given == "no" ~ "ever breastfeed",
      breastMilk_given == "no" & breastMilk_still_being_given == "no" ~ "never breastfeed"
    ),
    house_roof_regroup = case_when(
      house_roof %in% c("batako", "beton", "genteng logam", "daun palem", "jerami") ~ "others",
      house_roof == "asbes" ~ "asbestos",
      house_roof == "genteng" ~ "clay tile",
      house_roof == "seng" ~ "metal sheet",
      house_roof == "kayu" ~ "wood",
      TRUE ~ house_roof # spandek is spandek
    ),
    house_building_regroup = case_when(
      house_building %in% c("batu", "batu bata", "batu   bata") ~ "brick",
      house_building %in% c("anyaman bambu", "bambu", "kayu", "triplek") ~ "bamboo/plywood",
      house_building == "batako" ~ "concrete block",
      TRUE ~ house_building
    ),
    house_window_regroup = case_when(
      house_window %in% c("bambu", "kayu", "tidak ada/terbuka") ~ "bamboo/wood/open",
      house_window == "kaca/tirai" ~ "glass/curtain",
      TRUE ~ house_window
    ),
    nTotal_people_regroup = case_when(
      is.na(nTotal_people) | nTotal_people < 4 ~ "1-3 (low)",
      nTotal_people >= 4 & nTotal_people < 7 ~ "4-6 (moderate)",
      nTotal_people >= 7 ~ ">6 (high)"
    ),
    nTotal_child_5yo_andBelow_regroup = case_when(
      nTotal_child_5yo_andBelow == 0 ~ "0",
      nTotal_child_5yo_andBelow == 1 | nTotal_child_5yo_andBelow == 2 ~ "1-2 children",
      nTotal_child_5yo_andBelow >= 3 ~ "3-7 children"
    ),
    nTotal_child_5yo_andBelow_sleep_regroup = case_when(
      nTotal_child_5yo_andBelow_sleep == 0 ~ "0",
      nTotal_child_5yo_andBelow_sleep >= 1 ~ "1-3 children"
    ),
    illness_past3days_fever_regroup = case_when(
      illness_past3days_fever == "unknown" ~ "no",
      TRUE ~ illness_past3days_fever
    ),
    # illness_past3days_fever_nDays_regroup = case_when(
    #   illness_past3days_fever_nDays == "no" ~ "0",
    #   illness_past3days_fever_nDays == "yes" ~ "2", # mode
    #   TRUE ~ illness_past3days_fever_nDays
    # ),
    illness_past3days_fever_nDays_regroup = illness_past3days_fever_nDays,
    illness_past24h_cough = case_when(
      is.na(illness_past24h_cough) ~ "no",
      TRUE ~ illness_past24h_cough
    ),
    illness_past24h_runny_nose = case_when(
      is.na(illness_past24h_runny_nose) ~ "no",
      TRUE ~ illness_past24h_runny_nose
    ),
    illness_past24h_difficulty_breathing = case_when(
      is.na(illness_past24h_difficulty_breathing) ~ "no",
      TRUE ~ illness_past24h_difficulty_breathing
    ),
    illness_past24h_difficulty_compiled = case_when(
      illness_past24h_cough == "no" & illness_past24h_runny_nose == "no" & illness_past24h_difficulty_breathing == "no" ~ "no",
      TRUE ~ "≥ 1 respiratory illness",
    ),
    vaccination_hibpentavalent_dc_n_regroup = case_when(
      vaccination_hibpentavalent_dc_n < 4 ~ "1-3 (mandatory)",
      vaccination_hibpentavalent_dc_n >= 4 ~ "4 (booster)"
    ),
    vaccination_pcv13_dc_n_regroup = case_when(
      vaccination_pcv13_dc_n == 0 ~ "0 (not yet)",
      vaccination_pcv13_dc_n < 3 ~ "1-2 (mandatory)",
      vaccination_pcv13_dc_n >= 3 ~ "3-4 (booster)"
    ),
    # mode imputation because > 1% missing values (n_max = 12)
    healthcareVisit_last_3mo = healthcareVisit_last_3mo,
    hospitalised_last_3mo = case_when(
      hospitalised_last_3mo == "" | hospitalised_last_3mo == "unknown" ~ "no",
      TRUE ~ hospitalised_last_3mo
      ),
    antibiotic_past3days = case_when(
      antibiotic_past3days == "unknown" ~ "no",
      TRUE ~ antibiotic_past3days
      ),
  ) %>% 
  glimpse()


write.csv(df_epi_coded_eng, "inputs/epiData_eng.csv", row.names = F)

