#PART 3: EXPLORATORY DATA ANALYSIS----

#Descriptive statistics - dplyr & skimr package
modex_desc <- modex_xfit %>% 
  group_by(condition, order, time) %>% 
  skim(age:rm_w)  

modex_desc_sex <- modex_xfit %>% 
  group_by(condition, time, sex) %>% 
  skim(age:rm_w)  

#Print - Descriptive statistics
print(modex_desc_sex)
