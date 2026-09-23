
cat("Cleaning Data.\n")
#PART 2: DATA CLEANING----
modex_xfit <- janitor::clean_names(modex_xfit)

modex_supfit <- janitor::clean_names(modex_supfit)

options(contrasts = c("contr.sum", "contr.poly"))


#modex_xfit <- modex_xfit %>% dplyr::select(-x26)
modex_xfit <- modex_xfit %>% dplyr::filter(!if_all(everything(), is.na))

modex_supfit <- modex_supfit %>% dplyr::filter(!if_all(everything(), is.na))

#Label Factors
modex_xfit <- modex_xfit %>%
  mutate(
    id = factor(modex_xfit$id),
    sex  = factor(sex,  levels=c(0,1), labels=c("Female","Male")),
    time = factor(time, levels=c(0,1), labels=c("Pre","Post")),
    condition = factor(condition, levels=c(0,1), labels=c("PLA","PYC")),
    order = factor(order, levels=c(0,1), labels=c("PYC/PLA", "PLA/PYC")))

modex_supfit <- modex_supfit %>%
  dplyr::select(where(~!all(is.na(.)))) %>% 
  mutate(
    id = factor(id),
    condition = factor(condition),
    order = factor(order, levels=c(0,1), labels=c("PYC/PLA", "PLA/PYC")),
    guess_order = factor(guess_order, levels=c(0,1), labels=c("PYC/PLA", "PLA/PYC"))) %>%
  mutate(across(
    starts_with("sup_"),
    ~ as.numeric(as.character(.))))

#set reference levels 
# Set reference levels
modex_xfit$time      <- relevel(factor(modex_xfit$time),      ref = "Pre")
modex_xfit$condition <- relevel(factor(modex_xfit$condition), ref = "PLA")
modex_xfit$sex       <- relevel(factor(modex_xfit$sex),       ref = "Female")

#run to create dot plots grouping# 
modex_xfit$condition.time <- paste(modex_xfit$condition, modex_xfit$time, sep = "_")
modex_xfit$condition.time <- factor(modex_xfit$condition.time, levels = c("PLA_Pre", "PLA_Post", "PYC_Pre", "PYC_Post"))
head(modex_xfit$condition.time)

modex_xfit_male$condition.time <- paste(modex_xfit_male$condition, modex_xfit_male$time, sep = "_")
modex_xfit_male$condition.time <- factor(modex_xfit_male$condition.time, levels = c("PLA_Pre", "PLA_Post", "PYC_Pre", "PYC_Post"))
head(modex_xfit_male$condition.time)

modex_xfit_female$condition.time <- paste(modex_xfit_female$condition, modex_xfit_female$time, sep = "_")
modex_xfit_female$condition.time <- factor(modex_xfit_female$condition.time, levels = c("PLA_Pre", "PLA_Post", "PYC_Pre", "PYC_Post"))
head(modex_xfit_female$condition.time)



#Separating male and female set - filter the set
#modex_xfit_male <- modex_xfit %>%
#dplyr::filter(sex == "Male")%>%
#dplyr::select(-sex)

#modex_xfit_female <- modex_xfit %>%
#dplyr::filter(sex == "Female")%>%
#dplyr::select(-sex)



#Drop id levels from data
#modex_xfit_male$id <- droplevels(modex_xfit_male$id)
#modex_xfit_female$id <- droplevels(modex_xfit_female$id)

# Split by order
data_pyc_pla <- modex_supfit %>%
  dplyr::filter(order == "PYC/PLA")%>%
  dplyr::select(-order)

data_pla_pyc <- modex_supfit %>%
  dplyr::filter(order == "PLA/PYC")%>%
  dplyr::select(-order)

#Drop unused ID levels (sex)
data_pyc_pla$id <- droplevels(factor(data_pyc_pla$id))
data_pla_pyc$id <- droplevels(factor(data_pla_pyc$id))

#output message 
cat("Pivoting datasets.\n")


#Pivot force data sets from wide to long format - BF data
bf_long <- modex_xfit |>
  select(id, order, condition, time, sex, starts_with("bf_")) |>
  pivot_longer(cols = starts_with("bf_"),        # pivot only BF_ columns
               names_to = "percentage",
               values_to = "SmO2") |>
  mutate(percentage = as.numeric(sub("bf_", "", percentage)))

#Pivot force data sets from wide to long format - vl data
vl_long <- modex_xfit |>
  select(id, order, condition, time, sex, starts_with("vl_")) |>
  pivot_longer(cols = starts_with("vl_"),        # pivot only vl_ columns
               names_to = "percentage",
               values_to = "SmO2") |>
  mutate(percentage = as.numeric(sub("vl_", "", percentage)))

#Pivot force data sets from wide to long format - vl data
mg_long <- modex_xfit |>
  select(id, order, condition, time, sex, starts_with("mg_")) |>
  pivot_longer(cols = starts_with("mg_"),        # pivot only mg_ columns
               names_to = "percentage",
               values_to = "SmO2") |>
  mutate(percentage = as.numeric(sub("mg_", "", percentage)))

cat("Data successfully cleaned.\n")