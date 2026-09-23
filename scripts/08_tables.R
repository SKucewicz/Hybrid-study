#print message
cat("Exporting model results...\n")

#run code
export_model_results <- function(model, model_name) {
  
  # -------------------------
  # Type III ANOVA
  # -------------------------
  anova_res <- car::Anova(model, type = 3)
  anova_df <- as.data.frame(anova_res)
  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  
  readr::write_csv(
    anova_df,
    here::here("Outputs", "Tables",
               paste0(model_name, "_anova_type3.csv"))
  )
  
  
  # -------------------------
  # Model coefficients
  # -------------------------
  coef_df <- as.data.frame(summary(model)$tTable)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  
  readr::write_csv(
    coef_df,
    here::here("Outputs", "Tables",
               paste0(model_name, "_model_coefficients.csv"))
  )
  
  
  # -------------------------
  # tab_model equivalent (clean table)
  # -------------------------
  tab_df <- parameters::model_parameters(
    model,
    effects = "fixed",
    ci = 0.95
  )
  
  readr::write_csv(
    as.data.frame(tab_df),
    here::here("Outputs", "Tables",
               paste0(model_name, "_tab_model.csv"))
  )
  
  
  # -------------------------
  # Effect sizes
  # -------------------------
  eff <- effectsize::eta_squared(model, partial = TRUE)
  eff_df <- as.data.frame(eff)
  
  readr::write_csv(
    eff_df,
    here::here("Outputs", "Tables",
               paste0(model_name, "_effect_sizes.csv"))
  )
  
  cat("Saved all tables for:", model_name, "\n")
}



dir.create(here::here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)

final_models <- list(

  vo2peak = reml_vo2_peak_time_con_psex,
  vt1 = vt1_time_cond_order_het_reml,
  vt2 = vt2_time_con_sex_reml,
  wingate_avg_power = reml_p_avg_wingate_time_con_psex,
  wingate_avg_power_bw = reml_pbw_avg_wingate_time_con_sex,
  wingate_peak_power = reml_p_peak_wingate_time_con_sex_het,
  wingate_peak_power_bw = reml_p_pbw_wingate_time_con_psex,
  wingate_fatigue_index = reml_fi_wingate_time_con_sex_het,
  wingate_lactate = reml_lac_time_con_sex_het,
  imtp_peak = reml_imtp_peak_time_con_psex,
  imtp_peak_bw = reml_imtp_pbw_time_con_psex,
  imtp_rfd = reml_imtp_rfd_time_con_sex_het,
  imtp_force_200ms = reml_imtp_200_time_con_sex_het,
  cmj_con_mean_power = reml_cmj_con_mean_power_time_con_psex_het_con,
  cmj_peak_power = reml_cmj_pp_time_con_psex_het,
  cmj_peak_power_bw = reml_cmj_pbm_time_con_psex_het_sex,
  cmj_jump_height = reml_cmj_hei_time_con_psex_het,
  cmj_mean_force = reml_cmj_con_avg_time_con_psex_het_con,
  cmj_peak_force = reml_cmj_con_p_time_con_psex,
  cmj_mean_power = reml_cmj_con_mean_power_time_con_mean_powersex,
  rm_w = reml_rm_w_time_con_sex_het,
  mg_all = reml_smo2_mg_per_con_het_sex,
  bf_all = reml_smo2_bf_per_con_het_sex,
  vl_all = reml_smo2_vl_per_con_het_sex,
  mg_25 = reml_mg_25_time_con_sex_het,
  mg_50 = reml_mg_50_time_con_sex_het,
  mg_75 = reml_mg_75_time_con_sex_het,
  mg_100 = reml_mg_100_time_con_sex_het,
  bf_25 = reml_bf_25_time_con_sex_het_con,
  bf_50 = reml_bf_50_time_con_sex_het_con,
  bf_75 = reml_bf_75_time_con_psex,
  bf_100 = reml_bf_100_time_con_psex,
  vl_25 = reml_vl_25_time_con_psex,
  vl_50 = reml_vl_50_time_con_psex,
  vl_75 = reml_vl_75_time_con_psex,
  vl_100 = reml_vl_100_time_con_psex,
  p_25 = reml_p_25_time_con_psex,
  p_50 = reml_p_50_time_con_sex_het,
  p_75 = reml_p_75_time_con_psex,
  p_100 = reml_p_100_time_con_psex
)

for (name in names(final_models)) {
  export_model_results(final_models[[name]], name)
}

#print message
cat("Finished exporting model results...\n")

cat("Exporting mean & SD tables...\n")
####PART 7: MEAN & SD TABLES####

summary_table_all <- modex_xfit %>%
  group_by(condition, time) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

write.csv(summary_table_all, file = "summary_table_all.csv", row.names = FALSE)

summary_table_sex <- modex_xfit %>%
  group_by(condition, time,sex) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

write.csv(summary_table_sex, file = "summary_table_sex.csv", row.names = FALSE)

summary_table_sex_age <- modex_xfit %>%
  group_by(sex) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

write.csv(summary_table_sex_age, file = "summary_table_sex_age.csv", row.names = FALSE)

#print message 
cat("Finished exporting mean & SD tables...\n")
