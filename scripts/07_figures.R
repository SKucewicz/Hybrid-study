summary_table_all <- modex_xfit %>%
  group_by(condition, time) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

view(summary_table_all)
write.csv(summary_table_all, file = "summary_table_all.csv", row.names = FALSE)

summary_table_sex <- modex_xfit %>%
  group_by(condition, time,sex) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

view(summary_table_sex)

write.csv(summary_table_sex, file = "summary_table_sex.csv", row.names = FALSE)

summary_table_sex_age <- modex_xfit %>%
  group_by(sex) %>%
  summarise(across(where(is.numeric), list(
    avg = ~mean(.x, na.rm = TRUE),
    sd = ~sd(.x, na.rm = TRUE)
  ), .names = "{col}_{fn}"))

view(summary_table_sex_age)

write.csv(summary_table_sex_age, file = "summary_table_sex_age.csv", row.names = FALSE)
