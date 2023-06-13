library(effectsize)
library(lavaan)
library(lmerTest)
library(tidyverse)

# set working directory
setwd("H:/ABCD/Relsease4.0/Package_1194636/results/peer_environments")
load("20230209_results.RData")

# load data
abcd <- read_csv("H:/ABCD/Relsease4.0/Package_1194636/abcd.csv")
base_information <- read_csv("H:/ABCD/Relsease4.0/Package_1194636/base_information.csv")
abcd_year2 <- filter(abcd, eventname == "2_year_follow_up_y_arm_1")
abcd_year3 <-  filter(abcd, eventname == "3_year_follow_up_y_arm_1")

# select variables -------------------------------------------------------------
# behavior: abcd_mhp02 abcd_mhy02 abcd_sscey01 abcd_sscep01
behavior <- select(abcd_year2, nihtbx_picvocab_uncorrected:peq_ss_relational_aggs)
behavior_na <- apply(behavior, 2, function(x) sum(is.na(x)))
# exclude variables with too much NA value
behavior <- behavior[, behavior_na <= 2329] %>%
  select(-ple_y_ss_affected_bad_mean) %>%
  names()

# basic variables
basic <- c("subjectkey", "interview_age", "sex", "race_ethnicity",
  "rel_family_id", "income_parent", "edu_parent")
peers <- c("pbp_ss_prosocial_peers", "pbp_ss_rule_break", "PII")

# MRI data
fmri_networks <- names(select(abcd_year2, rsfmri_c_ngd_ad_ngd_ad:rsfmri_c_ngd_vta_ngd_vs))
fmri_within_network <- names(select(abcd_year2, rsfmri_c_ngd_ad_ngd_ad:rsfmri_c_ngd_vs_ngd_vs))
fmri_between_network <- names(select(abcd_year2, rsfmri_c_ngd_ad_ngd_cgc:rsfmri_c_ngd_vta_ngd_vs))
fmri_network_subcortical <- names(select(abcd_year2, rsfmri_cor_ngd_au_scs_crcx:rsfmri_cor_ngd_vs_scs_bs))
smri_thick <- names(select(abcd_year2, contains("_thick_cdk")))[-c(69:71)]
smri_area <- names(select(abcd_year2, contains("_area_cdk")))[-c(69:71)]
smri_vol_cortical <- names(select(abcd_year2, smri_vol_cdk_banksstslh:smri_vol_cdk_insularh))
smri_vol_subcortical <- names(select(abcd_year2, smri_vol_scs_cbwmatterlh:smri_vol_scs_intracranialv))
smri <- c("smri_vol_cdk_total", "smri_thick_cdk_mean", "smri_area_cdk_total")

# exclude missing value and MRI QC
abcd_year2_behavior <- select(abcd_year2, all_of(basic), site_id_l,
  all_of(peers), all_of(behavior)) %>%
  filter(complete.cases(.))

abcd_year2_rsfmri <- select(abcd_year2, all_of(basic), mri_info_deviceserialnumber,
  all_of(peers), rsfmri_c_ngd_meanmotion, all_of(fmri_within_network),
  all_of(fmri_between_network), all_of(fmri_network_subcortical), imgincl_rsfmri_include) %>%
  filter(imgincl_rsfmri_include == 1, complete.cases(.))

abcd_year2_smri <- select(abcd_year2, all_of(basic), all_of(peers),
  mri_info_deviceserialnumber, smri_vol_scs_intracranialv, all_of(smri_thick),
  all_of(smri_area), all_of(smri_vol_cortical), all_of(smri_vol_subcortical),
  all_of(smri), imgincl_t1w_include) %>%
  filter(imgincl_t1w_include == 1, complete.cases(.))

abcd_year3_behavior <- select(abcd_year3, all_of(basic), site_id_l,
  all_of(peers), all_of(behavior)) %>%
  select(-c(nihtbx_picvocab_uncorrected:nihtbx_reading_uncorrected),
    -pgbi_p_ss_score, -c(upps_y_ss_negative_urgency:bis_y_ss_basm_drive)) %>%
  filter(complete.cases(.))

# longitudinal data 
abcd_longitudinal_year2 <- abcd_year2_behavior[abcd_year2_behavior$subjectkey %in% abcd_year3_behavior$subjectkey, ]
abcd_longitudinal_year3 <- abcd_year3_behavior[abcd_year3_behavior$subjectkey %in% abcd_year2_behavior$subjectkey, ]

# basic analysis ---------------------------------------------------------------
basic_stat <- function(data) {
  N <- nrow(data)
  female_percent <- (sum(data$sex == "F") / nrow(data) * 100) %>% round(2)
  mean_age <- (mean(data$interview_age) / 12) %>% round(2)
  sd_age <- (sd(data$interview_age) / 12) %>% round(2)
  income_parent <- mean(data$income_parent) %>% round(2)
  sd_income_parent <- sd(data$income_parent) %>% round(2)
  edu_parent <- mean(data$edu_parent) %>% round(2)
  sd_edu_parent <- sd(data$edu_parent) %>% round(2)
  race <- unlist(table(data$race_ethnicity))
  return(c(N, female_percent, mean_age, sd_age, income_parent, sd_income_parent,
    edu_parent, sd_edu_parent, race))
}
basic_stat(abcd_year2_behavior)
basic_stat(abcd_year2_smri)
basic_stat(abcd_year2_rsfmri)
basic_stat(abcd_longitudinal_year3)

cor.test(~ pbp_ss_prosocial_peers + pbp_ss_rule_break, abcd_year2_behavior)

# 1. Association Analysis ------------------------------------------------------
lmm_vars <- function(var) {
  # behavior
  lmm_behavior <- function(x, df) {
    cat("\n", x, "   Particpant:", nrow(df))
    fit <- lmer(df[[x]] ~ df[[var]] + sex + interview_age + race_ethnicity +
      income_parent + edu_parent + (1|site_id_l:rel_family_id), data = df)
    fit_summary <- summary(fit)
    # 3 df; 4 t-value; 5 p-value
    t_vec <- fit_summary$coefficients[2, 4]
    p_vec <- fit_summary$coefficients[2, 5]
    effect_size <- t_to_eta2(fit_summary$coefficients[2, 4], fit_summary$coefficients[2, 3])$Eta2_partial
    return(list(t_vec, p_vec, effect_size))
  }

  # rs-fMRI
  lmm_rsfmri <- function(x, df) {
    cat("\n", x, "Particpant:", nrow(df))
    fit <- lmer(df[[x]] ~ df[[var]] + sex + interview_age + race_ethnicity +
      income_parent + edu_parent + rsfmri_c_ngd_meanmotion +
      (1|mri_info_deviceserialnumber:rel_family_id), data = df)
    fit_summary <- summary(fit)
    # 3 df; 4 t-value; 5 p-value
    t_vec <- fit_summary$coefficients[2, 4]
    p_vec <- fit_summary$coefficients[2, 5]
    effect_size <- t_to_eta2(fit_summary$coefficients[2, 4], fit_summary$coefficients[2, 3])$Eta2_partial
    return(list(t_vec, p_vec, effect_size))
  }

  # sMRI
  lmm_smri <- function(x, df) {
    cat("\n", x, "Particpant:", nrow(df))
    fit <- lmer(df[[x]] ~ df[[var]] + sex + interview_age +
      race_ethnicity + income_parent + edu_parent +
      smri_vol_scs_intracranialv + (1|mri_info_deviceserialnumber:rel_family_id),
      data = df)
    fit_summary <- summary(fit)
    # 3 df; 4 t-value; 5 p-value
    t_vec <- fit_summary$coefficients[2, 4]
    p_vec <- fit_summary$coefficients[2, 5]
    effect_size <- t_to_eta2(fit_summary$coefficients[2, 4], fit_summary$coefficients[2, 3])$Eta2_partial
    return(list(t_vec, p_vec, effect_size))
  }

  result_behavior <- sapply(behavior, lmm_behavior, abcd_year2_behavior)
  result_networks <- sapply(fmri_networks, lmm_rsfmri, abcd_year2_rsfmri)
  result_network_subcortical <- sapply(fmri_network_subcortical, lmm_rsfmri, abcd_year2_rsfmri)
  result_thick <- sapply(smri_thick, lmm_smri, abcd_year2_smri)
  result_area <- sapply(smri_area, lmm_smri, abcd_year2_smri)
  result_vol_cortical <- sapply(smri_vol_cortical, lmm_smri, abcd_year2_smri)
  result_vol_subcortical <- sapply(smri_vol_subcortical, lmm_smri, abcd_year2_smri)
  result_smri <- sapply(smri, lmm_smri, abcd_year2_smri)

  # output
  results <- list(result_behavior, result_networks, result_network_subcortical,
    result_thick, result_area, result_vol_cortical, result_vol_subcortical,
    result_smri)
  names(results) <- c("behavior", "networks",  "network_sub", "thick", "area",
    "vol_cortical", "vol_subcortical", "sMRI")

  return(results)
}
p_adjust <- function(x, p_correction) {
  x[2, ] <- p.adjust(x[2, ], method = p_correction)
  x <- x[, x[2, ] < 0.05]
  return(x)
}

# PFI
PFI <- lmm_vars("pbp_ss_prosocial_peers")
PFI1 <- lapply(PFI, p_adjust, "fdr")

# DFI
DFI <- lmm_vars("pbp_ss_rule_break")
DFI1 <- lapply(DFI, p_adjust, "fdr")

# PII
PII <- lmm_vars("PII")
PII1 <- lapply(PII, p_adjust, "fdr")

# output
dk_table <- data.frame(
  "PFI" = unlist(PFI$vol_cortical[1, ]),
  "DFI" = unlist(DFI$vol_cortical[1, ]),
  "PII" = unlist(PII$vol_cortical[1, ])
)
dk_table$PF <- ifelse(dk_table$PFI > 2.95, dk_table$PFI, 0)
dk_table$DF <- ifelse(dk_table$DFI < -2.67, dk_table$DFI, 0)
dk_table$RPF <- ifelse(dk_table$DFI > 2.41, dk_table$PII, 0)

write.table(dk_table, "dk_table.txt", row.names = FALSE)


# modify results (FDR corretions)
modify_behavior <- function(data) {
  colnames(data[["behavior"]]) <- ifelse(
    unlist(data[["behavior"]][1, ]) > 0,
    paste0(colnames(data[["behavior"]]), "_P"),
    paste0(colnames(data[["behavior"]]), "_N")
  )
  return(data)
}
PFI_behaviors <- modify_behavior(PFI1)
DFI_behaviors <- modify_behavior(DFI1)

PFI_behaviors$behavior[, colnames(PFI_behaviors$behavior) %in% colnames(DFI_behaviors$behavior)]

# 2. Neurotransmitters ---------------------------------------------------------
# brain t map
vol_PFI <- unlist(PFI$vol_cortical[1, ])
vol_DFI <- unlist(DFI$vol_cortical[1, ])
vol_PII <- unlist(PII$vol_cortical[1, ])
thick_PFI <- unlist(PFI$thick[1, ])
thick_DFI <- unlist(DFI$thick[1, ])
thick_PII <- unlist(PII$thick[1, ])
area_PFI <- unlist(PFI$area[1, ])
area_DFI <- unlist(DFI$area[1, ])
area_PII <- unlist(PII$area[1, ])

tmaps <- data.frame(vol_PFI, thick_PFI, area_PFI, vol_DFI, thick_DFI, area_DFI,
  vol_PII, thick_PII, area_PII)
cor(tmaps)

write.table(vol_PFI, "pet/tmaps/tmap_vol_PFI.txt", col.names = FALSE, row.names = FALSE)
write.table(vol_DFI, "pet/tmaps/tmap_vol_DFI.txt", col.names = FALSE, row.names = FALSE)
write.table(vol_PII, "pet/tmaps/tmap_vol_PII.txt", col.names = FALSE, row.names = FALSE)
write.table(thick_PFI, "pet/tmaps/tmap_thick_PFI.txt", col.names = FALSE, row.names = FALSE)
write.table(thick_DFI, "pet/tmaps/tmap_thick_DFI.txt", col.names = FALSE, row.names = FALSE)
write.table(thick_PII, "pet/tmaps/tmap_thick_PII.txt", col.names = FALSE, row.names = FALSE)
write.table(area_PFI, "pet/tmaps/tmap_area_PFI.txt", col.names = FALSE, row.names = FALSE)
write.table(area_DFI, "pet/tmaps/tmap_area_DFI.txt", col.names = FALSE, row.names = FALSE)
write.table(area_PII, "pet/tmaps/tmap_area_PII.txt", col.names = FALSE, row.names = FALSE)

# load results
Neurotransmitters <- read.csv("pet/Resh.csv") %>%
  rename("Spearman" = Mean.Fisher.s.z..Spearman.rho., "PET" = PET.Map) %>%
  select(File, PET, Spearman, p_exact)
Neurotransmitters$PET <- str_replace(Neurotransmitters$PET, "SERT", "5HTT")
Neurotransmitters$File <- str_split(Neurotransmitters$File, "/", simplify = TRUE)[, 6]
Neurotransmitters$File <- factor(Neurotransmitters$File, levels = unique(Neurotransmitters$File))

# FDR corrections
p_fdr <- Neurotransmitters %>%
  group_by(File) %>%
  summarise_at(vars(p_exact), p.adjust, method = "fdr")
Neurotransmitters$p_fdr <- p_fdr$p_exact
Neurotransmitters$significants <- ifelse(Neurotransmitters$p_fdr < 0.01, "**",
  ifelse(Neurotransmitters$p_fdr < 0.05, "*", " "))

# volume
Neurotransmitters_vol <- Neurotransmitters[str_detect(Neurotransmitters$File, "vol"), ]
# area
Neurotransmitters_area <- Neurotransmitters[str_detect(Neurotransmitters$File, "area"), ]
# thick
Neurotransmitters_thick <- Neurotransmitters[str_detect(Neurotransmitters$File, "thick"), ]

# 3. Mediation Analysis --------------------------------------------------------
# cortical volume
mediation_volume <- select(abcd_year2, all_of(basic), mri_info_deviceserialnumber,
  smri_vol_scs_intracranialv, all_of(peers), all_of(behavior),
  smri_vol_cdk_total, colnames(PFI1$vol_cortical),
  colnames(DFI1$vol_cortical), colnames(PII1$vol_cortical),
  imgincl_t1w_include) %>%
  filter(imgincl_t1w_include == 1) %>%
  select(-imgincl_t1w_include)
mediation_volume <- mediation_volume[complete.cases(mediation_volume), ]
basic_stat(mediation_volume)

mediation_volume$sex <- factor(
  mediation_volume$sex,
  levels = c("F", "M"),
  labels = c(1, 0)
)
mediation_volume$mri_info_deviceserialnumber <- factor(
  mediation_volume$mri_info_deviceserialnumber,
  levels = unique(mediation_volume$mri_info_deviceserialnumber),
  labels = seq(unique(mediation_volume$mri_info_deviceserialnumber))
)

write_csv(mediation_volume, "mediation_volume.csv")

# RSFC
mediation_rsfc <- select(abcd_year2, all_of(basic), mri_info_deviceserialnumber,
  rsfmri_c_ngd_meanmotion, all_of(peers), all_of(behavior),
  colnames(PFI1$networks), colnames(DFI1$networks),
  colnames(DFI1$network_sub), imgincl_rsfmri_include,
  colnames(PII1$networks)) %>%
  filter(imgincl_rsfmri_include == 1) %>%
  select(-imgincl_rsfmri_include)
mediation_rsfc <- mediation_rsfc[complete.cases(mediation_rsfc), ]
basic_stat(mediation_rsfc)

mediation_rsfc$sex <- factor(mediation_rsfc$sex,
  levels = c("F", "M"),
  labels = c(1, 0)
)
mediation_rsfc$mri_info_deviceserialnumber <- factor(
  mediation_rsfc$mri_info_deviceserialnumber,
  levels = unique(mediation_rsfc$mri_info_deviceserialnumber),
  labels = seq(unique(mediation_rsfc$mri_info_deviceserialnumber))
)
write_csv(mediation_rsfc, "mediation_rsfc.csv")

# mediation analysis in Matlab  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_csvs <- function(files) {
  # set row names
  paths_p <- paste0(c("PathA", "PathB", "PathCC", "PathC", "PathAB"), "_p")
  paths_t <- paste0(c("PathA", "PathB", "PathCC'", "PathC", "PathAB"), "_beta")
  CI <- paste0(rep(c("A", "B", "CC", "C", "AB"), 2), rep(1:2, each = 5))
  paths <- c(paths_p, paths_t, CI)
  
  # read files
  csv_file <- paste0("mediation_brain_behavior_p/", files, ".csv")
  result <- read.csv(csv_file, header = FALSE)

  colnames(result) <- paths
  rownames(result) <- paste(files, "-", behavior)
  return(result)
}

# mediation - PFI total volume
PFI_volume <- read_csvs("PFI_volume")
PFI_volume[, 1:5] <- apply(PFI_volume[, 1:5], 2, p.adjust, method = "fdr")
PFI_volume <- filter(PFI_volume, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

# mediation - DFI total volume
DFI_volume <- read_csvs("DFI_volume")
DFI_volume[, 1:5] <- apply(DFI_volume[, 1:5], 2, p.adjust, method = "fdr")
DFI_volume <- filter(DFI_volume, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

PFI_volume_var <- str_split(row.names(PFI_volume), " - ", simplify = TRUE)[, 1]
DFI_volume_var <- str_split(row.names(DFI_volume), " - ", simplify = TRUE)[, 1]
PFI_volume_var[PFI_volume_var %in% DFI_volume_var]

# mediation - PFI regional volume
PFI_ifpllh <- read_csvs("PFI_ifpllh")
PFI_paracnlh <- read_csvs("PFI_paracnlh")
PFI_precnlh <- read_csvs("PFI_precnlh")
PFI_rracatelh <- read_csvs("PFI_rracatelh")
PFI_sufrlh <- read_csvs("PFI_sufrlh")
PFI_postcnrh <- read_csvs("PFI_postcnrh")
PFI_insularh <- read_csvs("PFI_insularh")

PFI_volume_all <- rbind(PFI_ifpllh, PFI_paracnlh, PFI_precnlh, PFI_rracatelh,
  PFI_sufrlh, PFI_postcnrh, PFI_insularh)

PFI_volume_all[1:5] <- apply(PFI_volume_all[1:5], 2, p.adjust, method = "fdr")
PFI_volume_all <- filter(PFI_volume_all, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

# mediation - DFI regional volume
DFI_fusiformlh <- read_csvs("DFI_fusiformlh")
DFI_locclh <- read_csvs("DFI_locclh")
DFI_lobfrlh <- read_csvs("DFI_lobfrlh")
DFI_supllh <- read_csvs("DFI_supllh")
DFI_iftmrh <- read_csvs("DFI_iftmrh")
DFI_loccrh <- read_csvs("DFI_loccrh")
DFI_mdtmrh <- read_csvs("DFI_mdtmrh")
DFI_ptcaterh <- read_csvs("DFI_ptcaterh")
DFI_precnrh <- read_csvs("DFI_precnrh")
DFI_pcrh <- read_csvs("DFI_pcrh")
DFI_suplrh <- read_csvs("DFI_suplrh")

DFI_volume_all <- rbind(DFI_fusiformlh, DFI_locclh, DFI_lobfrlh, DFI_supllh,
  DFI_iftmrh, DFI_loccrh, DFI_mdtmrh, DFI_ptcaterh, DFI_precnrh, DFI_pcrh,
  DFI_suplrh)
DFI_volume_all[1:5] <- apply(DFI_volume_all[1:5], 2, p.adjust, method = "fdr")
DFI_volume_all <- filter(DFI_volume_all, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

# mediation - PFI FC
PFI_rspltp_sa <- read_csvs("PFI_rspltp_sa")
PFI_dt_smm <- read_csvs("PFI_dt_smm")
PFI_dt_rspltp <- read_csvs("PFI_dt_rspltp")
PFI_dt_fo <- read_csvs("PFI_dt_fo")
PFI_dt_dla <- read_csvs("PFI_dt_dla")
PFI_cgc_dt <- read_csvs("PFI_cgc_dt")
PFI_cgc_dla <- read_csvs("PFI_cgc_dla")
PFI_ca_vta <- read_csvs("PFI_ca_vta")
PFI_ca_vs <- read_csvs("PFI_ca_vs")
PFI_ca_smm <- read_csvs("PFI_ca_smm")
PFI_ca_smh <- read_csvs("PFI_ca_smh")
PFI_ad_smm <- read_csvs("PFI_ad_smm")
PFI_ad_sa <- read_csvs("PFI_ad_sa")
PFI_ad_ca <- read_csvs("PFI_ad_ca")

PFI_fc <- rbind(PFI_rspltp_sa, PFI_dt_smm, PFI_dt_rspltp, PFI_dt_fo, PFI_dt_dla,
  PFI_cgc_dt, PFI_cgc_dla, PFI_ca_vta, PFI_ca_vs, PFI_ca_smm, PFI_ca_smh,
  PFI_ad_smm, PFI_ad_sa, PFI_ad_ca)
PFI_fc[1:5] <- apply(PFI_fc[1:5], 2, p.adjust, method = "fdr")
PFI_fc <- filter(PFI_fc, PathAB_p < 0.05, PathA_p < 0.05, PathB_p < 0.05,
  PathC_p < 0.05, PathCC_p < 0.05)

# mediation - DFI FC
DFI_ad_ad <- read_csvs("DFI_ad_ad")
DFI_dt_dt <- read_csvs("DFI_dt_dt")
DFI_dla_dla <- read_csvs("DFI_dla_dla")
DFI_smh_smh <- read_csvs("DFI_smh_smh")
DFI_vs_vs <- read_csvs("DFI_vs_vs")
DFI_ad_smh <- read_csvs("DFI_ad_smh")
DFI_ad_smm <- read_csvs("DFI_ad_smm")
DFI_cgc_ca <- read_csvs("DFI_cgc_ca")
DFI_cgc_dt <- read_csvs("DFI_cgc_dt")
DFI_cgc_vs <- read_csvs("DFI_cgc_vs")
DFI_dt_dla <- read_csvs("DFI_dt_dla")
DFI_dt_vs <- read_csvs("DFI_dt_vs")
DFI_dla_vta <- read_csvs("DFI_dla_vta")
DFI_smh_smm <- read_csvs("DFI_smh_smm")

DFI_fc <- rbind(DFI_ad_ad, DFI_dt_dt, DFI_dla_dla, DFI_smh_smh, DFI_vs_vs,
  DFI_ad_smh, DFI_ad_smm, DFI_cgc_ca, DFI_cgc_dt, DFI_cgc_vs, DFI_dt_dla,
  DFI_dt_vs, DFI_dla_vta, DFI_smh_smm)
DFI_fc[1:5] <- apply(DFI_fc[1:5], 2, p.adjust, method = "fdr")
DFI_fc <- filter(DFI_fc, PathAB_p < 0.05, PathA_p < 0.05, PathB_p < 0.05,
  PathC_p < 0.05, PathCC_p < 0.05)

# mediation - DFI cortico-subcortical FC
DFI_au_cde <- read_csvs("DFI_au_cde")
DFI_au_hp <- read_csvs("DFI_au_hp")
DFI_au_aa <- read_csvs("DFI_au_aa")
DFI_cerc_thp <- read_csvs("DFI_cerc_thp")
DFI_cerc_pt <- read_csvs("DFI_cerc_pt")
DFI_cerc_hp <- read_csvs("DFI_cerc_hp")
DFI_cerc_ag <- read_csvs("DFI_cerc_ag")
DFI_copa_crcx <- read_csvs("DFI_copa_crcx")
DFI_copa_cde <- read_csvs("DFI_copa_cde")
DFI_copa_vtdc <- read_csvs("DFI_copa_vtdc")
DFI_df_pl <- read_csvs("DFI_df_pl")
DFI_df_ag <- read_csvs("DFI_df_ag")
DFI_df_aa <- read_csvs("DFI_df_aa")
DFI_dsa_hp <- read_csvs("DFI_dsa_hp")
DFI_dsa_ag <- read_csvs("DFI_dsa_ag")
DFI_dsa_vtdc <- read_csvs("DFI_dsa_vtdc")
DFI_fopa_crcx <- read_csvs("DFI_fopa_crcx")
DFI_fopa_thp <- read_csvs("DFI_fopa_thp")
DFI_fopa_cde <- read_csvs("DFI_fopa_cde")
DFI_fopa_ag <- read_csvs("DFI_fopa_ag")
DFI_rst_aa <- read_csvs("DFI_rst_aa")
DFI_smh_crcx <- read_csvs("DFI_smh_crcx")
DFI_smh_cde <- read_csvs("DFI_smh_cde")
DFI_smh_pt <- read_csvs("DFI_smh_pt")
DFI_smh_pl <- read_csvs("DFI_smh_pl")
DFI_smh_aa <- read_csvs("DFI_smh_aa")
DFI_smm_pl <- read_csvs("DFI_smm_pl")
DFI_smm_hp <- read_csvs("DFI_smm_hp")
DFI_smm_ag <- read_csvs("DFI_smm_ag")
DFI_sa_hp <- read_csvs("DFI_sa_hp")
DFI_sa_aa <- read_csvs("DFI_sa_aa")
DFI_sa_vtdc <- read_csvs("DFI_sa_vtdc")
DFI_vta_cde <- read_csvs("DFI_vta_cde")
DFI_vta_pt <- read_csvs("DFI_vta_pt")
DFI_vta_pl <- read_csvs("DFI_vta_pl")
DFI_vta_hp <- read_csvs("DFI_vta_hp")
DFI_vs_vtdc <- read_csvs("DFI_vs_vtdc")
DFI_cerc_bs <- read_csvs("DFI_cerc_bs")
DFI_dsa_bs <- read_csvs("DFI_dsa_bs")
DFI_rst_bs <- read_csvs("DFI_rst_bs")
DFI_smh_bs <- read_csvs("DFI_smh_bs")

DFI_sub <- rbind(DFI_au_cde, DFI_au_hp, DFI_au_aa, DFI_cerc_thp, DFI_cerc_pt,
  DFI_cerc_hp, DFI_cerc_ag, DFI_copa_crcx, DFI_copa_cde, DFI_copa_vtdc,
  DFI_df_pl, DFI_df_ag, DFI_df_aa, DFI_dsa_hp, DFI_dsa_ag, DFI_dsa_vtdc,
  DFI_fopa_crcx, DFI_fopa_thp, DFI_fopa_cde, DFI_fopa_ag, DFI_rst_aa,
  DFI_smh_crcx, DFI_smh_cde, DFI_smh_pt, DFI_smh_pl, DFI_smh_aa, DFI_smm_pl,
  DFI_smm_hp, DFI_smm_ag, DFI_sa_hp, DFI_sa_aa, DFI_sa_vtdc, DFI_vta_cde,
  DFI_vta_pt, DFI_vta_pl, DFI_vta_hp, DFI_vs_vtdc, DFI_cerc_bs, DFI_dsa_bs,
  DFI_rst_bs, DFI_smh_bs)
DFI_sub[1:5] <- apply(DFI_sub[1:5], 2, p.adjust, method = "fdr")
DFI_sub <- filter(DFI_sub, PathAB_p < 0.05, PathA_p < 0.05, PathB_p < 0.05,
  PathC_p < 0.05, PathCC_p < 0.05)

# PII - volume
PII_volume <- read_csvs("PII_volume")
PII_volume[, 1:5] <- apply(PII_volume[, 1:5], 2, p.adjust, method = "fdr")
PII_volume <- filter(PII_volume, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)
PII_volume_var <- str_split(row.names(PII_volume), " - ", simplify = TRUE)[, 2]

# mediation - PII volume
PII_ifpllh <- read_csvs("PII_ifpllh")
PII_locclh <- read_csvs("PII_locclh")
PII_lobfrlh <- read_csvs("PII_lobfrlh")
PII_mdtmlh <- read_csvs("PII_mdtmlh")
PII_paracnlh <- read_csvs("PII_paracnlh")
PII_postcnlh <- read_csvs("PII_postcnlh")
PII_precnlh <- read_csvs("PII_precnlh")
PII_pclh <- read_csvs("PII_pclh")
PII_rracatelh <- read_csvs("PII_rracatelh")
PII_sufrlh <- read_csvs("PII_sufrlh")
PII_insulalh <- read_csvs("PII_insulalh")
PII_banksstsrh <- read_csvs("PII_banksstsrh")
PII_cuneusrh <- read_csvs("PII_cuneusrh")
PII_fusiformrh <- read_csvs("PII_fusiformrh")
PII_ifplrh <- read_csvs("PII_ifplrh")
PII_mdtmrh <- read_csvs("PII_mdtmrh")
PII_paracnrh <- read_csvs("PII_paracnrh")
PII_postcnrh <- read_csvs("PII_postcnrh")
PII_ptcaterh <- read_csvs("PII_ptcaterh")
PII_precnrh <- read_csvs("PII_precnrh")
PII_sufrrh <- read_csvs("PII_sufrrh")
PII_suplrh <- read_csvs("PII_suplrh")
PII_smrh <- read_csvs("PII_smrh")
PII_insularh <- read_csvs("PII_insularh")

PII_volume_all <- rbind(PII_ifpllh, PII_locclh, PII_lobfrlh, PII_mdtmlh,
  PII_paracnlh, PII_postcnlh, PII_precnlh, PII_pclh, PII_rracatelh,
  PII_sufrlh, PII_insulalh, PII_banksstsrh, PII_cuneusrh, PII_fusiformrh,
  PII_ifplrh, PII_mdtmrh, PII_paracnrh, PII_postcnrh, PII_ptcaterh,
  PII_precnrh, PII_sufrrh, PII_suplrh, PII_smrh, PII_insularh)

PII_volume_all[1:5] <- apply(PII_volume_all[1:5], 2, p.adjust, method = "fdr")
PII_volume_all <- filter(PII_volume_all, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

# mediation - PII FC
PII_cgc_cgc <- read_csvs("PII_cgc_cgc")
PII_dt_dt <- read_csvs("PII_dt_dt")
PII_dla_dla <- read_csvs("PII_dla_dla")
PII_ad_ca <- read_csvs("PII_ad_ca")
PII_cgc_dt <- read_csvs("PII_cgc_dt")
PII_cgc_dla <- read_csvs("PII_cgc_dla")
PII_cgc_vs <- read_csvs("PII_cgc_vs")
PII_ca_smh <- read_csvs("PII_ca_smh")
PII_ca_vta <- read_csvs("PII_ca_vta")
PII_dt_dla <- read_csvs("PII_dt_dla")
PII_dt_fo <- read_csvs("PII_dt_fo")
PII_dt_rspltp <- read_csvs("PII_dt_rspltp")
PII_dla_vs <- read_csvs("PII_dla_vs")

PII_fc <- rbind(PII_cgc_cgc, PII_dt_dt, PII_dla_dla, PII_ad_ca, PII_cgc_dt,
  PII_cgc_dla, PII_cgc_vs, PII_ca_smh, PII_ca_vta, PII_dt_dla, PII_dt_fo,
  PII_dt_rspltp, PII_dla_vs)

PII_fc[1:5] <- apply(PII_fc[1:5], 2, p.adjust, method = "fdr")
PII_fc <- filter(PII_fc, PathAB_p < 0.05, PathA_p < 0.05,
  PathB_p < 0.05, PathC_p < 0.05, PathCC_p < 0.05)

# 4. Cross Lag Panel Model -----------------------------------------------------
# time: year2 - year3
clpms <- function(x) {

  # regressed out covariates
  residual_value <- function(y, data) {
    fit <- lm(data[[y]] ~ sex + interview_age + race_ethnicity + site_id_l +
    rel_family_id + income_parent + edu_parent, data = data)
    return(resid(fit))
  }

  residual_year2_PFI <- residual_value("pbp_ss_prosocial_peers", abcd_year2_behavior)
  residual_year3_PFI <- residual_value("pbp_ss_prosocial_peers", abcd_year3_behavior)
  residual_year2_DFI <- residual_value("pbp_ss_rule_break", abcd_year2_behavior)
  residual_year3_DFI <- residual_value("pbp_ss_rule_break", abcd_year3_behavior)
  residual_year2_PII <- residual_value("PII", abcd_year2_behavior)
  residual_year3_PII <- residual_value("PII", abcd_year3_behavior)

  residual_year2_x <- residual_value(x, abcd_year2_behavior)
  residual_year3_x <- residual_value(x, abcd_year3_behavior)

  residualed_year2 <- data.frame(
  "subjectkey" = abcd_year2_behavior$subjectkey,
  "PFI_year2" = residual_year2_PFI,
  "DFI_year2" = residual_year2_DFI,
  "PII_year2" = residual_year2_PII,
  "x_year2" = residual_year2_x
  )
  residualed_year3 <- data.frame(
  "subjectkey" = abcd_year3_behavior$subjectkey,
  "PFI_year3" = residual_year3_PFI,
  "DFI_year3" = residual_year3_DFI,
  "PII_year3" = residual_year3_PII,
  "x_year3" = residual_year3_x
  )

  clpm_data <- inner_join(residualed_year2, residualed_year3, by = "subjectkey") %>%
    select(-subjectkey) %>%
    apply(2, scale)
  
  clpm_PFI <- '
    # Estimate the lagged effects between the observed variables.
    PFI_year3 + x_year3 ~ PFI_year2 + x_year2

    # Estimate the covariance between the observed variables at the first wave.
    PFI_year2 ~~ x_year2 # Covariance

    # Estimate the covariances between the residuals of the observed variables.
    PFI_year3 ~~ x_year3

    # Estimate the (residual) variance of the observed variables.
    PFI_year2 ~~ PFI_year2 # Variances
    x_year2 ~~ x_year2
    PFI_year3 ~~ PFI_year3 # Residual variances
    x_year3 ~~ x_year3
  '
  clpm_DFI <- '
    DFI_year3 + x_year3 ~ DFI_year2 + x_year2

    DFI_year2 ~~ x_year2 # Covariance

    DFI_year3 ~~ x_year3

    DFI_year2 ~~ DFI_year2
    x_year2 ~~ x_year2
    DFI_year3 ~~ DFI_year3
    x_year3 ~~ x_year3
  '
  clpm_PII <- '
    PII_year3 + x_year3 ~ PII_year2 + x_year2

    PII_year2 ~~ x_year2 # Covariance

    PII_year3 ~~ x_year3

    PII_year2 ~~ PII_year2
    x_year2 ~~ x_year2
    PII_year3 ~~ PII_year3
    x_year3 ~~ x_year3
  '
  
  CLPM_PFI <- lavaan(clpm_PFI, data = clpm_data, meanstructure = TRUE, int.ov.free = TRUE)
  CLPM_DFI <- lavaan(clpm_DFI, data = clpm_data, meanstructure = TRUE, int.ov.free = TRUE)
  CLPM_PII <- lavaan(clpm_PII, data = clpm_data, meanstructure = TRUE, int.ov.free = TRUE)
  result_PFI <- summary(CLPM_PFI, ci = TRUE)$pe
  result_DFI <- summary(CLPM_DFI, ci = TRUE)$pe
  result_PII <- summary(CLPM_PII, ci = TRUE)$pe

  result <- rbind(result_PFI[1:6, ], result_DFI[1:6, ], result_PII[1:6, ])
  return(result)
}

clpm_vars <- names(select(abcd_year3_behavior, cbcl_scr_syn_anxdep_r:peq_ss_relational_aggs))
clpm_all <- lapply(clpm_vars, clpms)
names(clpm_all) <- clpm_vars
clpm_all

# FDR corrections
time1_time2_PFI <- vector()
crosslag_x1_PFI <- vector()
crosslag_PFI_x1 <- vector()
time1_time2_x1 <- vector()
time1_x1_PFI <- vector()
time2_x1_PFI <- vector()

time1_time2_DFI <- vector()
crosslag_x2_DFI <- vector()
crosslag_DFI_x2 <- vector()
time1_time2_x2 <- vector()
time1_x2_DFI <- vector()
time2_x2_DFI <- vector()

time1_time2_PII <- vector()
crosslag_x3_PII <- vector()
crosslag_PII_x3 <- vector()
time1_time2_x3 <- vector()
time1_x3_PII <- vector()
time2_x3_PII <- vector()

for(i in seq(clpm_vars)) {
  time1_time2_PFI[i] <- clpm_all[[i]][1, 8]
  crosslag_x1_PFI[i] <- clpm_all[[i]][2, 8]
  crosslag_PFI_x1[i] <- clpm_all[[i]][3, 8]
  time1_time2_x1[i] <- clpm_all[[i]][4, 8]
  time1_x1_PFI[i] <- clpm_all[[i]][5, 8]
  time2_x1_PFI[i] <- clpm_all[[i]][6, 8]

  time1_time2_DFI[i] <- clpm_all[[i]][7, 8]
  crosslag_x2_DFI[i] <- clpm_all[[i]][8, 8]
  crosslag_DFI_x2[i] <- clpm_all[[i]][9, 8]
  time1_time2_x2[i] <- clpm_all[[i]][10, 8]
  time1_x2_DFI[i] <- clpm_all[[i]][11, 8]
  time2_x2_DFI[i] <- clpm_all[[i]][12, 8]

  time1_time2_PII[i] <- clpm_all[[i]][13, 8]
  crosslag_x3_PII[i] <- clpm_all[[i]][14, 8]
  crosslag_PII_x3[i] <- clpm_all[[i]][15, 8]
  time1_time2_x3[i] <- clpm_all[[i]][16, 8]
  time1_x3_PII[i] <- clpm_all[[i]][17, 8]
  time2_x3_PII[i] <- clpm_all[[i]][18, 8]
}

cross_lags <- data.frame(
  # PFI
  time1_time2_PFI,
  crosslag_x1_PFI,
  crosslag_PFI_x1,
  time1_time2_x1,
  time1_x1_PFI,
  time2_x1_PFI,
  # DFI
  time1_time2_DFI,
  crosslag_x2_DFI,
  crosslag_DFI_x2,
  time1_time2_x2,
  time1_x2_DFI,
  time2_x2_DFI,
  # PII
  time1_time2_PII,
  crosslag_x3_PII,
  crosslag_PII_x3,
  time1_time2_x3,
  time1_x3_PII,
  time2_x3_PII
)

# FDR correction
cross_lags_fdr <- apply(cross_lags, 2, p.adjust, method = "fdr") %>% data.frame()
rownames(cross_lags_fdr) <- clpm_vars
cross_lags_fdr

x_PFI <- clpm_vars[cross_lags_fdr$crosslag_x1_PFI < 0.05]
PFI_x <- clpm_vars[cross_lags_fdr$crosslag_PFI_x1 < 0.05]
common_PFI <- x_PFI[x_PFI %in% PFI_x]

x_DFI <- clpm_vars[cross_lags_fdr$crosslag_x2_DFI < 0.05]
DFI_x <- clpm_vars[cross_lags_fdr$crosslag_DFI_x2 < 0.05]
common_DFI <- x_DFI[x_DFI %in% DFI_x]

common_all <- common_PFI[common_PFI %in% common_DFI]

cross_lags_fdr1 <- filter(cross_lags_fdr, crosslag_x1_PFI < 0.05,
  crosslag_PFI_x1 < 0.05, crosslag_x2_DFI < 0.05, crosslag_DFI_x2 < 0.05)

x_PII <- clpm_vars[cross_lags_fdr$crosslag_x3_PII < 0.05]
PII_x <- clpm_vars[cross_lags_fdr$crosslag_PII_x3 < 0.05]

# common X
x_PFI[x_PFI %in% x_DFI]
PFI_x[PFI_x %in% DFI_x]

clpm_vars[cross_lags_fdr$cross_lag_x_PII < 0.05]
clpm_vars[cross_lags_fdr$cross_lag_PII_x < 0.05]

# save working space
save.image("20230209_results.RData")