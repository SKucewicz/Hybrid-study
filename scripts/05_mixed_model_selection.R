####5.1: GXT####
######5.1.1 VO2peak#### 
#Model 1 - Time * Condition * sex * order
lme_vo2_peak_all <- lme(vo2_peak ~ time*condition*sex*order,
                        random=(~1 | id),
                        data = modex_xfit,
                        method="ML",
                        na.action=na.omit)
#Check Anova
car::Anova(lme_vo2_peak_all, type = 3)
###No 4 way interaction order dropped###

#Model 2 - Time * Condition * sex
lme_vo2_peak_time_con_sex <- lme(vo2_peak ~ time*condition*sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 3 - Time * condition + sex 
lme_vo2_peak_time_con_psex <- lme(vo2_peak ~ time*condition+sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Check - Model Comparison
anova(lme_vo2_peak_all,
      lme_vo2_peak_time_con_sex,
      lme_vo2_peak_time_con_psex)

#Model 3 - Time * condition + sex is the simplest model

#Check models with heterogeneous variances against current parsimonious model

#Model 4 - Time * Condition + sex with heterogeneity sex
lme_vo2_peak_time_con_sex_het <- lme(vo2_peak ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 5 - Time * Condition + sex with heterogeneity condition
lme_vo2_peak_time_con_sex_het_con <- lme(vo2_peak ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

#Check - Model Comparison
anova(lme_vo2_peak_time_con_psex,
      lme_vo2_peak_time_con_sex_het,
      lme_vo2_peak_time_con_sex_het_con)

#Model 3 is the parsimonious - refit to REML 
reml_vo2_peak_time_con_psex <- lme(vo2_peak ~ time*condition+sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="REML",
                                   na.action=na.omit)

#Check - ANOVA
Anova(reml_vo2_peak_time_con_psex, type=3)

#Check - anova
anova(reml_vo2_peak_time_con_psex)

#Check - Model Diagnostics
performance::check_model(reml_vo2_peak_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_vo2_peak_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_vo2_peak_time_con_psex, digits = 1)

#determine effect size 
eta_squared(reml_vo2_peak_time_con_psex, partial = TRUE)

######5.1.2:  VT1####
#Model 1 - Time * Condition * sex * order
lme_vt1_all <- lme(vt1 ~ time*condition*sex*order,
                   random=(~1 | id),
                   data = modex_xfit,
                   method="ML",
                   na.action=na.omit)

#Check - ANOVA
car::Anova(lme_vt1_all, type = 3)
#Sig 4-way interaction!
#No real effects of sex, so remove and refit thee way interaction model
#Drop sex

#Model 2 - Time * Condition * Order
lme_vt1_time_con_order <- lme(vt1 ~ time*condition*order,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)

#Model 3 - Time * Condition * Order + age
lme_vt1_time_con_order_age <- lme(vt1 ~ time*condition*order+age,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)
#Check - Model Comparison
anova(lme_vt1_all,
      lme_vt1_time_con_order,
      lme_vt1_time_con_order_age)
#3-way: time * condition * order is parsimonious (lower BIC)

#Model 4 - Time * Condition * Order, heterogeneous variances for condition
lme_vt1_time_con_order_het <- lme(vt1 ~ time*condition*order,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  weights=varIdent(form=~1|condition),
                                  method="ML",
                                  na.action=na.omit)

#Check - ANOVA
anova(lme_vt1_time_con_order,
      lme_vt1_time_con_order_het)

#Model 4 is parsimonious (lower BIC) - Refit with REML
vt1_time_cond_order_het_reml <- lme(vt1 ~ time*condition*order,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        weights=varIdent(form=~1|condition),
                                        method="REML",
                                        na.action=na.omit)

#Check - ANOVA
Anova(vt1_time_cond_order_het_reml, type = 3)

#Check - Model Diagnostics
performance::check_model(vt1_time_cond_order_het_reml)

#Plot - Model Effects
plot(allEffects(vt1_time_cond_order_het_reml))

#Check - Model Table
sjPlot::tab_model(vt1_time_cond_order_het_reml)

######5.1.3:  VT2 ####
#Model 1 - Time * Condition * sex * order
lme_vt2_all <- lme(vt2 ~ time*condition*sex*order,
                   random=(~1 | id),
                   data = modex_xfit,
                   method="ML",
                   na.action=na.omit)
#Check anova
car::Anova(lme_vt2_all, type = 3)
#NO 4 way interaction dropped order

#Model 2 - Time * Condition * sex
lme_vt2_time_con_sex <- lme(vt2 ~ time*condition*sex,
                            random=(~1 | id),
                            data = modex_xfit,
                            method="ML",
                            na.action=na.omit)

#Model 3 - time * condition + sex
lme_vt2_time_con_sexp <- lme(vt2 ~ time*condition+sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)

#Check models 
anova(lme_vt2_all,
      lme_vt2_time_con_sex,
      lme_vt2_time_con_sexp)

# Model 3 is parsimonious  

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_vt2_time_con_sex_het <- lme(vt2 ~ time*condition*sex,
                                random=(~1 | id),
                                weights = varIdent(form = ~1|sex),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)


#Model 4 - Time * Condition * sex, heterogeneous variances for condition
lme_vt2_time_sex_het_con <- lme(vt2 ~ time*condition*sex,
                                random=(~1 | id),
                                weights = varIdent(form = ~1|condition),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#
anova(lme_vt2_all,
      lme_vt2_time_con_sex,
      lme_vt2_time_con_sexp,
      lme_vt2_time_con_sex_het,
      lme_vt2_time_sex_het_con)

#No need for heterogeneous Level 1 variance
#Time * Condition * sex model is simplest

#Check - ANOVA
Anova(lme_vt2_time_con_sex)

#Model 2 is parsimonious (lower BIC) - refit with REML 
vt2_time_con_sex_reml <- lme(vt2 ~ time*condition*sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="REML",
                                 na.action=na.omit)
#Check - ANOVA
Anova(vt2_time_con_sex_reml, type = 3)

#Check - Model diagnostics
performance::check_model(vt2_time_con_sex_reml)

#Plot - Model Effect
plot(allEffects(vt2_time_con_sex_reml))

#Check - Model Table
sjPlot::tab_model(vt2_time_con_sex_reml)
###5.2: ALL OUT TEST####

######5.2.1:  AVERAGE POWER####
#Model 1 - Time * Condition * sex * order
lme_p_avg_wingate_all <- lme(p_avg_wingate ~ time*condition*sex*order,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_avg_wingate_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_p_avg_wingate_time_con_sex <- lme(p_avg_wingate ~ time*condition*sex,
                                      random=(~1 | id),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_p_avg_wingate_time_con_psex <- lme(p_avg_wingate ~ time*condition+sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)


#Model 5 time * condidition + sex +order
lme_p_avg_wingate_time_con_psex_porder <- lme(p_avg_wingate ~ time*condition+sex+order,
                                              random=(~1 | id),
                                              data = modex_xfit,
                                              method="ML",
                                              na.action=na.omit)


#Check - Model Comparisions
anova(lme_p_avg_wingate_all,
      lme_p_avg_wingate_time_con_sex,
      lme_p_avg_wingate_time_con_psex,
      lme_p_avg_wingate_time_con_psex_porder)

#Model 4 - Time * Condition + sex is simplest model

#Model 6 - Time * Condition + sex  - Heterogeneous variances for sex
lme_p_avg_wingate_time_con_sex_het <- lme(p_avg_wingate ~ time*condition+sex,
                                          random=(~1 | id),
                                          weights = varIdent(form = ~1|sex),
                                          data = modex_xfit,
                                          method="ML",
                                          na.action=na.omit)

#Model 7 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_p_avg_wingate_time_con_sex_het_con <- lme(p_avg_wingate ~ time*condition+sex,
                                              random=(~1 | id),
                                              weights = varIdent(form = ~1|condition),
                                              data = modex_xfit,
                                              method="ML",
                                              na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_avg_wingate_time_con_psex,
      lme_p_avg_wingate_time_con_sex_het,
      lme_p_avg_wingate_time_con_sex_het_con)

#Model 6 - Time * Condition + sex   - Heterogeneous variances for sex is simplest
#Model 6 - Refit to REML

reml_p_avg_wingate_time_con_psex <- lme(p_avg_wingate ~ time*condition+sex,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        method="REML",
                                        na.action=na.omit)

#Check - ANOVA
Anova(reml_p_avg_wingate_time_con_psex, type=3)

#Check - Model Diagnostics
performance::check_model(reml_p_avg_wingate_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_p_avg_wingate_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_p_avg_wingate_time_con_psex, digits = 0)

#effect size 
eta_squared(reml_p_avg_wingate_time_con_psex, partial = TRUE)

######5.2.2: AVERAGE POWER/BM####
#Model 1 - Time * Condition * sex * order
lme_pbw_avg_wingate_all <- lme(pbw_avg_wingate ~ time*condition*sex*order,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)
#Check - ANOVA
car::Anova(lme_pbw_avg_wingate_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_pbw_avg_wingate_time_con_sex <- lme(pbw_avg_wingate ~ time*condition*sex,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_pbw_avg_wingate_time_con_psex <- lme(pbw_avg_wingate ~ time*condition+sex,
                                         random=(~1 | id),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

#Check - Model Comparisions
anova(lme_pbw_avg_wingate_all,
      lme_pbw_avg_wingate_time_con_sex,
      lme_pbw_avg_wingate_time_con_psex)

#Model 4 - Time * Condition + sex is simplest model

#Model 6 - Time * Condition + sex  - Heterogeneous variances for sex
lme_pbw_avg_wingate_time_con_sex_het <- lme(pbw_avg_wingate ~ time*condition+sex,
                                            random=(~1 | id),
                                            weights = varIdent(form = ~1|sex),
                                            data = modex_xfit,
                                            method="ML",
                                            na.action=na.omit)

#Model 7 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_pbw_avg_wingate_time_con_sex_het_con <- lme(pbw_avg_wingate ~ time*condition+sex,
                                                random=(~1 | id),
                                                weights = varIdent(form = ~1|condition),
                                                data = modex_xfit,
                                                method="ML",
                                                na.action=na.omit)

#Check - Model Comparisions
anova(lme_pbw_avg_wingate_time_con_psex,
      lme_pbw_avg_wingate_time_con_sex_het,
      lme_pbw_avg_wingate_time_con_sex_het_con)

#Model 6 - Time * Condition -  is simplest
#Model 6 - Refit to REML

reml_pbw_avg_wingate_time_con_sex <- lme(pbw_avg_wingate ~ time*condition+sex,
                                         random=(~1 | id),
                                         data = modex_xfit,
                                         method="REML",
                                         na.action=na.omit)

#Check - ANOVA
Anova(reml_pbw_avg_wingate_time_con_sex, type=3)

#Check - Model Diagnostics
performance::check_model(reml_pbw_avg_wingate_time_con_sex)

#Plot - Model Effects
plot(allEffects(reml_pbw_avg_wingate_time_con_sex))

#Check - Model Table
sjPlot::tab_model(reml_pbw_avg_wingate_time_con_sex)

#effect size 
eta_squared(reml_pbw_avg_wingate_time_con_sex, partial = TRUE)

######5.2.3: PEAK POWER####
#Model 1 - Time * Condition * sex * order
lme_p_peak_wingate_all <- lme(p_peak_wingate ~ time*condition*sex*order,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_peak_wingate_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_p_peak_wingate_time_con_sex <- lme(p_peak_wingate ~ time*condition*sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 3 - Time * Condition + sex
lme_p_peak_wingate_time_con_psex <- lme(p_peak_wingate ~ time*condition+sex,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)



#Check - Model Comparisions
anova(lme_p_peak_wingate_all,
      lme_p_peak_wingate_time_con_sex,
      lme_p_peak_wingate_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 6 - Time * Condition * sex, heterogeneous variances for sex
lme_p_peak_wingate_time_con_sex_het <- lme(p_peak_wingate ~ time*condition+sex,
                                           random=(~1 | id),
                                           weights = varIdent(form = ~1|sex),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)


#Model 7 - Time * Condition * sex, heterogeneous variances for condition
lme_p_peak_wingate_time_con_sex_het_con <- lme(p_peak_wingate ~ time*condition+sex,
                                               random=(~1 | id),
                                               weights = varIdent(form = ~1|condition),
                                               data = modex_xfit,
                                               method="ML",
                                               na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_peak_wingate_time_con_psex, 
      lme_p_peak_wingate_time_con_sex_het,
      lme_p_peak_wingate_time_con_sex_het_con)

#Model 6 - Time * Condition + Sex het sex is parsimonious - Refit to REML
reml_p_peak_wingate_time_con_sex_het <- lme(p_peak_wingate ~ time*condition+sex,
                                            random=(~1 | id),
                                            weights = varIdent(form = ~1|sex),
                                            data = modex_xfit,
                                            method="REML",
                                            na.action=na.omit)
#Check - anova 
anova(reml_p_peak_wingate_time_con_sex_het)

#Check - ANOVA
Anova(reml_p_peak_wingate_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_p_peak_wingate_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_p_peak_wingate_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_p_peak_wingate_time_con_sex_het)

#effect size 
eta_squared(reml_p_peak_wingate_time_con_sex_het, partial = TRUE)

######5.2.4: PEAK POWER/ BODY WEIGHT####
#Model 1 - Time * Condition * sex * order
lme_p_pbw_wingate_all <- lme(p_pbw_wingate ~ time*condition*sex*order,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_pbw_wingate_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_p_pbw_wingate_time_con_sex <- lme(p_pbw_wingate ~ time*condition*sex,
                                      random=(~1 | id),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Model 3 - Time * Condition + sex,
lme_p_pbw_wingate_time_con_psex <- lme(p_pbw_wingate ~ time*condition+sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 4 - Time * Condition + sex + age,
lme_p_pbw_wingate_time_con_psex_page <- lme(p_pbw_wingate ~ time*condition+sex+age,
                                            random=(~1 | id),
                                            data = modex_xfit,
                                            method="ML",
                                            na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_pbw_wingate_all,
      lme_p_pbw_wingate_time_con_sex,
      lme_p_pbw_wingate_time_con_psex,
      lme_p_pbw_wingate_time_con_psex_page)

#Model 3 - Time * Condition + sex, is the simplest model

#Model 6 - Time * Condition * sex + age, , heterogeneous variances for sex
lme_p_pbw_wingate_time_con_sex_age_het <- lme(p_pbw_wingate ~ time*condition+sex,
                                              random=(~1 | id),
                                              weights = varIdent(form = ~1|sex),
                                              data = modex_xfit,
                                              method="ML",
                                              na.action=na.omit)

#Model 7 - Time * Condition * sex, heterogeneous variances for condition 
lme_p_pbw_wingate_time_con_sex_het_con <- lme(p_pbw_wingate ~ time*condition+sex,
                                              random=(~1 | id),
                                              weights = varIdent(form = ~1|condition),
                                              data = modex_xfit,
                                              method="ML",
                                              na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_pbw_wingate_time_con_psex,
      lme_p_pbw_wingate_time_con_sex_age_het,
      lme_p_pbw_wingate_time_con_sex_het_con)

#Model 3 - Time * Condition + Sex is parsimonious (lower BIC) - Refit to REML 
reml_p_pbw_wingate_time_con_psex <- lme(p_pbw_wingate ~ time*condition+sex,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        method="REML",
                                        na.action=na.omit)

#Check - anova 
anova(reml_p_pbw_wingate_time_con_psex)

#Check - ANOVA
Anova(reml_p_pbw_wingate_time_con_psex)

#Check - Model Diagnostics
performance::check_model(reml_p_pbw_wingate_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_p_pbw_wingate_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_p_pbw_wingate_time_con_psex)

#effect size 
eta_squared(reml_p_pbw_wingate_time_con_psex, partial = TRUE)

######5.2.5: FATIGUE INDEX####
#Model 1 - Time * Condition * sex * order
lme_fi_wingate_all <- lme(fi_wingate ~ time*condition*sex*order,
                          random=(~1 | id),
                          data = modex_xfit,
                          method="ML",
                          na.action=na.omit)
#Check - ANOVA
car::Anova(lme_fi_wingate_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_fi_wingate_time_con_sex <- lme(fi_wingate ~ time*condition*sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 3 - Time * Condition + sex
lme_fi_wingate_time_con_psex <- lme(fi_wingate ~ time*condition+sex,
                                    random=(~1 | id),
                                    data = modex_xfit,
                                    method="ML",
                                    na.action=na.omit)

#Model 4 - Time * Condition + sex + age
lme_fi_wingate_time_con_psex_page <- lme(fi_wingate ~ time*condition+sex+age,
                                         random=(~1 | id),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)
#Model 3 - Time * Condition + sex + order
lme_fi_wingate_time_con_psex_porder <- lme(fi_wingate ~ time*condition+sex+order,
                                           random=(~1 | id),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)


#Check - Model Comparisons
anova(lme_fi_wingate_all,
      lme_fi_wingate_time_con_sex,
      lme_fi_wingate_time_con_psex,
      lme_fi_wingate_time_con_psex_page,
      lme_fi_wingate_time_con_psex_porder)

#Model 3 - Time * Condition + sex, simplest model

#Model 6 - Time * Condition + sex, heterogeneous variances for sex
lme_fi_wingate_time_con_sex_het <- lme(fi_wingate ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|sex),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 7 - Time * Condition + sex, heterogeneous variances for condition
lme_fi_wingate_time_con_sex_het_con <- lme(fi_wingate ~ time*condition+sex,
                                           random=(~1 | id),
                                           weights = varIdent(form = ~1|condition),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)

#Check - Model Comparisons
anova(lme_fi_wingate_time_con_psex,
      lme_fi_wingate_time_con_sex_het,
      lme_fi_wingate_time_con_sex_het_con)

#Time * Condition + Sex, heterogeneous variances for sex, is parsimonious - refit to reml#
reml_fi_wingate_time_con_sex_het <- lme(fi_wingate ~ time*condition+sex,
                                        random=(~1 | id),
                                        weights = varIdent(form = ~1|sex),
                                        data = modex_xfit,
                                        method="REML",
                                        na.action=na.omit)
#check - anova 
anova(reml_fi_wingate_time_con_sex_het)

#Check - ANOVA
car::Anova(reml_fi_wingate_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_fi_wingate_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_fi_wingate_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_fi_wingate_time_con_sex_het, digits = 0)

#effect size 
eta_squared(reml_fi_wingate_time_con_sex_het, partial = TRUE)

######5.2.6: LACTATE WINGATE####
#Model 1 - Time * Condition * sex * order
lme_lac_all <- lme(lac_wingate ~ time*condition*sex*order,
                   random=(~1 | id),
                   data = modex_xfit,
                   method="ML",
                   na.action=na.omit)

#Check - ANOVA
car::Anova(lme_lac_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_lac_time_con_sex <- lme(lac_wingate ~ time*condition*sex,
                            random=(~1 | id),
                            data = modex_xfit,
                            method="ML",
                            na.action=na.omit)

#Model 3 - Time * Condition + sex
lme_lac_time_con_psex <- lme(lac_wingate ~ time*condition+sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)

#anova - Check models 
anova(lme_lac_all,
      lme_lac_time_con_sex,
      lme_lac_time_con_psex)

#Model 3 - Time * Condition + sex, is the simplest model

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_lac_time_con_sex_het <- lme(lac_wingate ~ time*condition+sex,
                                random=(~1 | id),
                                weights = varIdent(form = ~1|sex),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#Model 5 - Time * Condition * sex, heterogeneous variances for condition
lme_lac_time_con_sex_het_con <- lme(lac_wingate ~ time*condition+sex,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|condition),
                                    data = modex_xfit,
                                    method="ML",
                                    na.action=na.omit)


anova(lme_lac_time_con_psex,
      lme_lac_time_con_sex_het,
      lme_lac_time_con_sex_het_con)

#Model 6 - Time * Condition * sex, heterogeneous variances for sex is parsimonious- refit to reml
reml_lac_time_con_sex_het <- lme(lac_wingate ~ time*condition+sex,
                                 random=(~1 | id),
                                 weights = varIdent(form = ~1|sex),
                                 data = modex_xfit,
                                 method="REML",
                                 na.action=na.omit)

#Check - ANOVA
anova(reml_lac_time_con_sex_het)

#Check - ANOVA
Anova(reml_lac_time_con_sex_het, type=3)

#Check - Model Diagnostics
performance::check_model(reml_lac_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_lac_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_lac_time_con_sex_het)

#effect size 
eta_squared(reml_lac_time_con_sex_het, partial = TRUE)

#####5.3: IMTP#####

######5.3.1: PEAK POWER####
#Model 1 - Time * Condition * sex * order
lme_imtp_peak_all <- lme(imtp_peak ~ time*condition*sex*order,
                         random=(~1 | id),
                         data = modex_xfit,
                         method="ML",
                         na.action=na.omit)

#Check - ANOVA
car::Anova(lme_imtp_peak_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_imtp_peak_time_con_sex <- lme(imtp_peak ~ time*condition*sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 3 - Time * Condition * sex + age
lme_imtp_peak_time_con_sex_page <- lme(imtp_peak ~ time*condition*sex+age,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 4 - Time * Condition + sex + age
lme_imtp_peak_time_con_psex_page <- lme(imtp_peak ~ time*condition+sex+age,
                                        random=(~1 | id),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)

#Model 5 - Time * Condition + sex * age
lme_imtp_peak_time_con_psex_age <- lme(imtp_peak ~ time*condition*age+sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 6 - Time * Condition + sex
lme_imtp_peak_time_con_psex <- lme(imtp_peak ~ time*condition+sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#anova - Check models 
anova(lme_imtp_peak_all,
      lme_imtp_peak_time_con_sex,
      lme_imtp_peak_time_con_sex_page,
      lme_imtp_peak_time_con_psex_page,
      lme_imtp_peak_time_con_psex_age,
      lme_imtp_peak_time_con_psex)

#Model 6 - Time * Condition + sex is parsimonious

#Model 7 - Time * Condition + sex, heterogeneous variances for sex
lme_imtp_peak_time_con_sex_het <- lme(imtp_peak ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|sex),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Model 8 - Time * Condition * sex, heterogeneous variances for condition
lme_imtp_peak_time_con_sex_het_con <- lme(imtp_peak ~ time*condition+sex,
                                          random=(~1 | id),
                                          weights = varIdent(form = ~1|condition),
                                          data = modex_xfit,
                                          method="ML",
                                          na.action=na.omit)

#Check - Model Comparisons 
anova(lme_imtp_peak_time_con_psex,
      lme_imtp_peak_time_con_sex_het,
      lme_imtp_peak_time_con_sex_het_con)

#Model 6 - Time * Condition + Sex is parsimonious (lower BIC) - refit model 2 to reml
reml_imtp_peak_time_con_psex <- lme(imtp_peak ~ time*condition+sex,
                                    random=(~1 | id),
                                    data = modex_xfit,
                                    method="REML",
                                    na.action=na.omit)

#Check - Anova 
Anova(reml_imtp_peak_time_con_psex, type = 3)

#Check - anova
anova(reml_imtp_peak_time_con_psex)

#Check - Model Diagnostics
performance::check_model(reml_imtp_peak_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_imtp_peak_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_imtp_peak_time_con_psex, digits = 0)

#effect size 
eta_squared(reml_imtp_peak_time_con_psex, partial = TRUE)

######5.3.2: PEAK POWER/ BODY WEIGHT####
#Model 1 - Time * Condition * sex * order
lme_imtp_pbw_all <- lme(imtp_pbw ~ time*condition*sex*order,
                        random=(~1 | id),
                        data = modex_xfit,
                        method="ML",
                        na.action=na.omit)

car::Anova(lme_imtp_pbw_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_imtp_pbw_time_con_sex <- lme(imtp_pbw ~ time*condition*sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 3 - Time * Condition + sex
lme_imtp_pbw_time_con_psex <- lme(imtp_pbw ~ time*condition+sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 4 - Time * Condition + sex + age
lme_imtp_pbw_time_con_psex_page <- lme(imtp_pbw ~ time*condition+sex+age,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 5 - Time * Condition + sex * age
lme_imtp_pbw_time_con_psex_age <- lme(imtp_pbw ~ time*condition*age+sex,
                                      random=(~1 | id),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#anova - Check models
anova(lme_imtp_pbw_all,
      lme_imtp_pbw_time_con_sex,
      lme_imtp_pbw_time_con_psex,
      lme_imtp_pbw_time_con_psex_age,
      lme_imtp_pbw_time_con_psex_page)

#Model 3 - Time * Condition + sex is parsimonious 

#Model 6 - Time * Condition + sex,  heterogeneous variances for sex
lme_imtp_pbw_time_con_sex_het <- lme(imtp_pbw ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 7 - Time * Condition + sex,  heterogeneous variances for condition
lme_imtp_pbw_time_con_sex_het_con <- lme(imtp_pbw ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

anova(lme_imtp_pbw_time_con_psex,
      lme_imtp_pbw_time_con_sex_het,
      lme_imtp_pbw_time_con_sex_het_con)

#Time * condition + sex, simplest model based off of BIC 
#Refit to REML
reml_imtp_pbw_time_con_psex <- lme(imtp_pbw ~ time*condition+sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="REML",
                                   na.action=na.omit)
#Check - ANOVA
Anova(reml_imtp_pbw_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_imtp_pbw_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_imtp_pbw_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_imtp_pbw_time_con_psex, digits = 1)

#effect size 
eta_squared(reml_imtp_pbw_time_con_psex, partial = TRUE)

######5.3.3: IMTP RFD#####
#Model 1 - Time * Condition * sex * order
lme_imtp_rfd_all <- lme(imtp_rfd ~ time*condition*sex*order,
                        random=(~1 | id),
                        data = modex_xfit,
                        method="ML",
                        na.action=na.omit)
#Check - ANOVA 
car::Anova(lme_imtp_rfd_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_imtp_rfd_time_con_sex <- lme(imtp_rfd ~ time*condition*sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 3 - Time * Condition + sex 
lme_imtp_rfd_time_con_psex <- lme(imtp_rfd ~ time*condition+sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 4 - Time * Condition + sex + age
lme_imtp_rfd_time_con_psex_page <- lme(imtp_rfd ~ time*condition+sex+age,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 5 - Time * Condition + sex + age
lme_imtp_rfd_time_con_psex_age <- lme(imtp_rfd ~ time*condition*age+sex,
                                      random=(~1 | id),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#anova - check models 
anova(lme_imtp_rfd_all,
      lme_imtp_rfd_time_con_psex,
      lme_imtp_rfd_time_con_psex_age,
      lme_imtp_rfd_time_con_psex_page,
      lme_imtp_rfd_time_con_sex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 6 - Time * Condition * sex, heterogeneous variances for sex
lme_imtp_rfd_time_con_sex_het <- lme(imtp_rfd ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 7 - Time * Condition * sex, heterogeneous variances for condition
lme_imtp_rfd_time_con_sex_het_con <- lme(imtp_rfd ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

anova(lme_imtp_rfd_time_con_psex,
      lme_imtp_rfd_time_con_sex_het,
      lme_imtp_rfd_time_con_sex_het_con)

###Time * Condition * sex, heterogeneous variances for sex is the simplest model - Refit to REML
reml_imtp_rfd_time_con_sex_het <- lme(imtp_rfd ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|sex),
                                      data = modex_xfit,
                                      method="REML",
                                      na.action=na.omit)

#Check - anova 
anova(reml_imtp_rfd_time_con_sex_het)

#Check - ANOVA
Anova(reml_imtp_rfd_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_imtp_rfd_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_imtp_rfd_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_imtp_rfd_time_con_sex_het)

#effect size 
eta_squared(reml_imtp_rfd_time_con_sex_het, partial = TRUE)

######5.3.4: FORCE @ 200MS#####
#Model 1 - Time * Condition * sex * order
lme_imtp_200_all <- lme(imtp_200 ~ time*condition*sex*order,
                        random=(~1 | id),
                        data = modex_xfit,
                        method="ML",
                        na.action=na.omit)
#Check - ANOVA
car::Anova(lme_imtp_200_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_imtp_200_time_con_sex <- lme(imtp_200 ~ time*condition*sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 2 - Time * Condition * sex
lme_imtp_200_time_con_psex <- lme(imtp_200 ~ time*condition+sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 3 - Time * Condition * sex + age
lme_imtp_200_time_con_sex_age <- lme(imtp_200 ~ time*condition*sex+age,
                                     random=(~1 | id),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_imtp_200_time_con_sex_het <- lme(imtp_200 ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)



#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_imtp_200_time_con_sex_het_con <- lme(imtp_200 ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)


anova(lme_imtp_200_all,
      lme_imtp_200_time_con_sex,
      lme_imtp_200_time_con_psex,
      lme_imtp_200_time_con_sex_age,
      lme_imtp_200_time_con_sex_het,
      lme_imtp_200_time_con_sex_het_con
)

###Time * condition + sex, heterogeneous variance for sex 

#Refit to REML  
reml_imtp_200_time_con_sex_het <- lme(imtp_200 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|sex),
                                      data = modex_xfit,
                                      method="REML",
                                      na.action=na.omit)

#Check - ANOVA
Anova(reml_imtp_200_time_con_sex_het)

#Check - Model Diagnostics
performance::check_model(reml_imtp_200_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_imtp_200_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_imtp_200_time_con_sex_het)

#effect size 
eta_squared(reml_imtp_200_time_con_sex_het, partial = TRUE)

#####5.4: CMJ####
######5.4.1: Concentric mean power#######
#Model 1 - Time * Condition * sex * order
lme_cmj_con_mean_power_all <- lme(cmj_con_mean_power ~ time*condition*sex*order,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_con_mean_power_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_cmj_con_mean_power_time_con_sex <- lme(cmj_con_mean_power ~ time*condition*sex,
                                           random=(~1 | id),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)

#Model 3 - Time * Condition * sex + age
lme_cmj_con_mean_power_time_con_psex <- lme(cmj_con_mean_power ~ time*condition+sex,
                                            random=(~1 | id),
                                            data = modex_xfit,
                                            method="ML",
                                            na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_mean_power_time_con_sex_het <- lme(cmj_con_mean_power ~ time*condition*sex,
                                               random=(~1 | id),
                                               weights = varIdent(form = ~1|sex),
                                               data = modex_xfit,
                                               method="ML",
                                               na.action=na.omit)

#Model 5 - Time * Condition * sex + age, heterogeneous variances for sex
lme_cmj_con_mean_power_time_con_psex_het <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                random=(~1 | id),
                                                weights = varIdent(form = ~1|sex),
                                                data = modex_xfit,
                                                method="ML",
                                                na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_mean_power_time_con_sex_het_con <- lme(cmj_con_mean_power ~ time*condition*sex,
                                                   random=(~1 | id),
                                                   weights = varIdent(form = ~1|condition),
                                                   data = modex_xfit,
                                                   method="ML",
                                                   na.action=na.omit)

lme_cmj_con_mean_power_time_con_psex_het_con <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                    random=(~1 | id),
                                                    weights = varIdent(form = ~1|condition),
                                                    data = modex_xfit,
                                                    method="ML",
                                                    na.action=na.omit)

#Model 7 - Time * Condition * sex + age, heterogeneous variances for condition
lme_cmj_con_mean_power_time_con_psex_het_con <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                    random=(~1 | id),
                                                    weights = varIdent(form = ~1|condition),
                                                    data = modex_xfit,
                                                    method="ML",
                                                    na.action=na.omit)

anova(lme_cmj_con_mean_power_all,
      lme_cmj_con_mean_power_time_con_sex,
      lme_cmj_con_mean_power_time_con_psex,
      lme_cmj_con_mean_power_time_con_sex_het,
      lme_cmj_con_mean_power_time_con_psex_het,
      lme_cmj_con_mean_power_time_con_sex_het_con,
      lme_cmj_con_mean_power_time_con_psex_het_con)

## Time * Condition p sex, heterogeneous variances for condition is simplest (lower BIC)
#Check - ANOVA 
anova(lme_cmj_con_mean_power_time_con_psex_het)

#Refit to REML
reml_cmj_con_mean_power_time_con_psex_het_con <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                     random=(~1 | id),
                                                     weights = varIdent(form = ~1|condition),
                                                     data = modex_xfit,
                                                     method="REML",
                                                     na.action=na.omit)



#Check - ANOVA
Anova(reml_cmj_con_mean_power_time_con_psex_het_con , type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_con_mean_power_time_con_psex_het_con )

#Plot - Model Effects
plot(allEffects(reml_cmj_con_mean_power_time_con_psex_het_con ))

#Check - Model Table
sjPlot::tab_model(reml_cmj_con_mean_power_time_con_psex_het_con )

######5.4.2: Peak Power####
#Model 1 - Time * Condition * sex * order
lme_cmj_pp_all <- lme(cmj_pp ~ time*condition*sex*order,
                      random=(~1 | id),
                      data = modex_xfit,
                      method="ML",
                      na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_pp_all, type = 3)
#No 4 way interaction - order dropped#

#Model 2 - Time * Condition * sex
lme_cmj_pp_time_con_sex <- lme(cmj_pp ~ time*condition*sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)
#Model 2 - Time * Condition * sex
lme_cmj_pp_time_con_order_sex <- lme(cmj_pp ~ time*condition*order+sex,
                                     random=(~1 | id),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 3 - Time * Condition * sex + age
lme_cmj_pp_time_con_psex <- lme(cmj_pp ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_pp_time_con_sex_het <- lme(cmj_pp ~ time*condition*sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 5 - Time * Condition * sex + age, heterogeneous variances for sex
lme_cmj_pp_time_con_psex_het <- lme(cmj_pp ~ time*condition+sex,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = modex_xfit,
                                    method="ML",
                                    na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_pp_time_con_sex_het_con <- lme(cmj_pp ~ time*condition*sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

lme_cmj_pp_time_con_psex_het_con <- lme(cmj_pp ~ time*condition+sex,
                                        random=(~1 | id),
                                        weights = varIdent(form = ~1|condition),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)

#Model 7 - Time * Condition * sex + age, heterogeneous variances for condition
lme_cmj_pp_time_con_psex_het_con <- lme(cmj_pp ~ time*condition+sex,
                                        random=(~1 | id),
                                        weights = varIdent(form = ~1|condition),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)

# - Time * Condition * order + sex, heterogeneous variances for condition
lme_cmj_pp_time_con_order_psex_het_con <- lme(cmj_pp ~ time*condition*order+sex,
                                              random=(~1 | id),
                                              weights = varIdent(form = ~1|condition),
                                              data = modex_xfit,
                                              method="ML",
                                              na.action=na.omit)

lme_cmj_pp_time_con_order_het_con <- lme(cmj_pp ~ time*condition*order,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

anova(lme_cmj_pp_all,
      lme_cmj_pp_time_con_sex,
      lme_cmj_pp_time_con_psex,
      lme_cmj_pp_time_con_sex_het,
      lme_cmj_pp_time_con_psex_het,
      lme_cmj_pp_time_con_sex_het_con,
      lme_cmj_pp_time_con_psex_het_con,
      lme_cmj_pp_time_con_order_sex,
      lme_cmj_pp_time_con_order_psex_het_con,
      lme_cmj_pp_time_con_order_het_con)

## Time * Condition p sex, heterogeneous variances for condition is simplest (lower BIC)
#Check - ANOVA 
anova(lme_cmj_pp_time_con_psex_het)

#Refit to REML
reml_cmj_pp_time_con_psex_het <- lme(cmj_pp ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="REML",
                                     na.action=na.omit)


#Check - ANOVA
Anova(reml_cmj_pp_time_con_psex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_pp_time_con_psex_het)

#Plot - Model Effects
plot(allEffects(reml_cmj_pp_time_con_psex_het))

#Check - Model Table
sjPlot::tab_model(reml_cmj_pp_time_con_psex_het)

#effect size 
eta_squared(reml_cmj_pp_time_con_psex_het, partial = TRUE)

######5.4.3: Peak Power / Body weight####
#Model 1 - Time * Condition * sex * order
lme_cmj_pbm_all <- lme(cmj_pbm ~ time*condition*sex*order,
                       random=(~1 | id),
                       data = modex_xfit,
                       method="ML",
                       na.action=na.omit)

car::Anova(lme_cmj_pbm_all, type = 3)
#Significant 4 way interaction
#No real effects of sex, so remove and refit thee way interaction model
#Drop sex

#Model 2 - Time * Condition * Order
lme_cmj_pbm_time_con_order <- lme(cmj_pbm ~ time*condition*order,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 3 - Time * Condition * Order + sex
lme_cmj_pbm_time_con_order_psex <- lme(cmj_pbm ~ time*condition*order+sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)
#Compare models
#Check - Model Comparison
anova(lme_cmj_pbm_all,
      lme_cmj_pbm_time_con_order,
      lme_cmj_pbm_time_con_order_psex)

#lme_cmj_pbm_time_con_order_sex is simplest model
Anova(lme_cmj_pbm_time_con_order_psex)

#Model 4 - Time * Condition * Order + sex, heterogeneous variances for condition
lme_cmj_pbm_time_con_psex_order_het_con <- lme(cmj_pbm ~ time*condition*order+sex,
                                               random=(~1 | id),
                                               data = modex_xfit,
                                               weights=varIdent(form=~1|condition),
                                               method="ML",
                                               na.action=na.omit)

#Model 5 - Time * Condition * Order, heterogeneous variances for condition
lme_cmj_pbm_time_con_order_het_con <- lme(cmj_pbm ~ time*condition*order,
                                          random=(~1 | id),
                                          data = modex_xfit,
                                          weights=varIdent(form=~1|condition),
                                          method="ML",
                                          na.action=na.omit)

#Model 6 - Time * Condition * Order + age, heterogeneous variances for sex
lme_cmj_pbm_time_con_psex_order_het_sex <- lme(cmj_pbm ~ time*condition*order+sex,
                                               random=(~1 | id),
                                               data = modex_xfit,
                                               weights=varIdent(form=~1|sex),
                                               method="ML",
                                               na.action=na.omit)

#Model 7 - Time * Condition * Order + age, heterogeneous variances for sex
lme_cmj_pbm_time_con_order_het_sex <- lme(cmj_pbm ~ time*condition*order,
                                          random=(~1 | id),
                                          data = modex_xfit,
                                          weights=varIdent(form=~1|sex),
                                          method="ML",
                                          na.action=na.omit)
#Model 8 - Time * Condition * Order + sex, heterogeneous variances for condition
lme_cmj_pbm_time_con_psex_het_con <- lme(cmj_pbm ~ time*condition+sex,
                                         random=(~1 | id),
                                         data = modex_xfit,
                                         weights=varIdent(form=~1|condition),
                                         method="ML",
                                         na.action=na.omit)

#Model 9 - Time * Condition * Order + age, heterogeneous variances for sex
lme_cmj_pbm_time_con_psex_het_sex <- lme(cmj_pbm ~ time*condition+sex,
                                         random=(~1 | id),
                                         data = modex_xfit,
                                         weights=varIdent(form=~1|sex),
                                         method="ML",
                                         na.action=na.omit)


#Check - Model Comparison
anova(lme_cmj_pbm_time_con_order_psex,
      lme_cmj_pbm_time_con_psex_order_het_con,
      lme_cmj_pbm_time_con_order_het_con,
      lme_cmj_pbm_time_con_psex_order_het_sex,
      lme_cmj_pbm_time_con_order_het_sex,
      lme_cmj_pbm_time_con_psex_het_sex,
      lme_cmj_pbm_time_con_psex_het_con)



#time*condition+sex, heterogeneous variances for sex  is parsimonious (lower BIC)
#Refit to REML
reml_cmj_pbm_time_con_psex_het_sex <- lme(cmj_pbm ~ time*condition+sex,
                                          random=(~1 | id),
                                          data = modex_xfit,
                                          weights=varIdent(form=~1|sex),
                                          method="REML",
                                          na.action=na.omit)

#
#Check - ANOVA
Anova(reml_cmj_pbm_time_con_psex_het_sex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_pbm_time_con_psex_het_sex)

#Plot - Model Effects
plot(allEffects(reml_cmj_pbm_time_con_psex_het_sex))

#Check - Model Table
sjPlot::tab_model(reml_cmj_pbm_time_con_psex_het_sex)

#effect size 
eta_squared(reml_cmj_pbm_time_con_psex_het_sex, partial = TRUE)

######5.4.4: Jump Height####
#Model 1 - Time * Condition * sex * order
lme_cmj_hei_all <- lme(cmj_hei ~ time*condition*sex*order,
                       random=(~1 | id),
                       data = modex_xfit,
                       method="ML",
                       na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_hei_all, type = 3)
#No 4 way interaction - drop order

#Model 2 - Time * Condition * sex
lme_cmj_hei_time_con_sex <- lme(cmj_hei ~ time*condition*sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#Model 3 - Time * Condition * sex + age
lme_cmj_hei_time_con_psex <- lme(cmj_hei ~ time*condition+sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_hei_time_con_sex_het <- lme(cmj_hei ~ time*condition*sex,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = modex_xfit,
                                    method="ML",
                                    na.action=na.omit)

#Model 5 - Time * Condition + sex, heterogeneous variances for sex
lme_cmj_hei_time_con_psex_het <- lme(cmj_hei ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|sex),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_hei_time_con_sex_het_con <- lme(cmj_hei ~ time*condition*sex,
                                        random=(~1 | id),
                                        weights = varIdent(form = ~1|condition),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)

#Model 7 - Time * Condition + sex, heterogeneous variances for condition
lme_cmj_hei_time_con_psex_het_con <- lme(cmj_hei ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|condition),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)

#Model 8 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_hei_time_con_order_psex_het_con <- lme(cmj_hei ~ time*condition*order+sex,
                                               random=(~1 | id),
                                               weights = varIdent(form = ~1|condition),
                                               data = modex_xfit,
                                               method="ML",
                                               na.action=na.omit)

#Model 9 - Time * Condition * sex + age, heterogeneous variances for sex
lme_cmj_hei_time_con_order_psex_het <- lme(cmj_hei ~ time*condition*order+sex,
                                           random=(~1 | id),
                                           weights = varIdent(form = ~1|sex),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)

#Model 10 - Time * Condition * order + sex 
lme_cmj_hei_time_con_order_psex <- lme(cmj_hei ~ time*condition*order+sex,
                                       random=(~1 | id),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Model 11 - Time * Condition * order 
lme_cmj_hei_time_con_order <- lme(cmj_hei ~ time*condition*order,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

anova(lme_cmj_hei_all,
      lme_cmj_hei_time_con_sex,
      lme_cmj_hei_time_con_psex,
      lme_cmj_hei_time_con_sex_het,
      lme_cmj_hei_time_con_psex_het,
      lme_cmj_hei_time_con_sex_het_con,
      lme_cmj_hei_time_con_psex_het_con,
      lme_cmj_hei_time_con_order_psex_het_con,
      lme_cmj_hei_time_con_order_psex_het,
      lme_cmj_hei_time_con_order_psex,
      lme_cmj_hei_time_con_order)

##Time * condition * sex is simplest (lowest BIC)
#Check - ANOVA
anova(lme_cmj_hei_time_con_psex_het)

#Refit to REML
reml_cmj_hei_time_con_psex_het <- lme(cmj_hei ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|sex),
                                      data = modex_xfit,
                                      method="REML",
                                      na.action=na.omit)
#Check - ANOVA
Anova(reml_cmj_hei_time_con_psex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_hei_time_con_psex_het)

#Plot - Model Effects
plot(allEffects(reml_cmj_hei_time_con_psex_het))

#Check - Model Table
sjPlot::tab_model(reml_cmj_hei_time_con_psex_het)

#Check effect size 
eta_squared(reml_cmj_hei_time_con_psex_het, partial = TRUE)

######5.4.5: Mean concentric force####
#Model 1 - Time * Condition * sex * order
lme_cmj_con_avg <- lme(cmj_con_avg ~ time*condition*sex*order,
                       random=(~1 | id),
                       data = modex_xfit,
                       method="ML",
                       na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_con_avg, type = 3)
#No 4 way interaction - drop order

#Model 2 - Time * Condition * sex
lme_cmj_con_avg_time_con_sex <- lme(cmj_con_avg ~ time*condition*sex,
                                    random=(~1 | id),
                                    data = modex_xfit,
                                    method="ML",
                                    na.action=na.omit)
#Model 2 - Time * Condition + sex
lme_cmj_con_avg_time_con_psex <- lme(cmj_con_avg ~ time*condition+sex,
                                     random=(~1 | id),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)


#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_avg_time_con_sex_het <- lme(cmj_con_avg ~ time*condition*sex,
                                        random=(~1 | id),
                                        weights = varIdent(form = ~1|sex),
                                        data = modex_xfit,
                                        method="ML",
                                        na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_avg_time_con_psex_het <- lme(cmj_con_avg ~ time*condition+sex,
                                         random=(~1 | id),
                                         weights = varIdent(form = ~1|sex),
                                         data = modex_xfit,
                                         method="ML",
                                         na.action=na.omit)


#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_avg_time_con_sex_het_con <- lme(cmj_con_avg ~ time*condition*sex,
                                            random=(~1 | id),
                                            weights = varIdent(form = ~1|condition),
                                            data = modex_xfit,
                                            method="ML",
                                            na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_avg_time_con_psex_het_con <- lme(cmj_con_avg ~ time*condition+sex,
                                             random=(~1 | id),
                                             weights = varIdent(form = ~1|condition),
                                             data = modex_xfit,
                                             method="ML",
                                             na.action=na.omit)

anova(lme_cmj_con_avg,
      lme_cmj_con_avg_time_con_sex,
      lme_cmj_con_avg_time_con_psex,
      lme_cmj_con_avg_time_con_sex_het,
      lme_cmj_con_avg_time_con_psex_het,
      lme_cmj_con_avg_time_con_sex_het_con,
      lme_cmj_con_avg_time_con_psex_het_con)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_avg_time_con_psex_het_con <- lme(cmj_con_avg ~ time*condition+sex,
                                             random=(~1 | id),
                                             weights = varIdent(form = ~1|condition),
                                             data = modex_xfit,
                                             method="ML",
                                             na.action=na.omit)
#Check - ANOVA
anova(lme_cmj_con_avg_time_con_psex_het_con)

#Refit to REML
reml_cmj_con_avg_time_con_psex_het_con <- lme(cmj_con_avg ~ time*condition+sex,
                                              random=(~1 | id),
                                              weights = varIdent(form = ~1|condition),
                                              data = modex_xfit,
                                              method="REML",
                                              na.action=na.omit)
#Check - ANOVA
Anova(reml_cmj_con_avg_time_con_psex_het_con, type =3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_con_avg_time_con_psex_het_con)

#Plot - Model Effects
plot(allEffects(reml_cmj_con_avg_time_con_psex_het_con))

#Check - Model Table
sjPlot::tab_model(reml_cmj_con_avg_time_con_psex_het_con)

######5.4.6:  Peak Concentrentic force (N/kg)####
#Model 1 - Time * Condition * sex * order
lme_cmj_con_p <- lme(cmj_con_p ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_con_p, type = 3)
#No 4 way interaction - drop order

#Model 2 - Time * Condition * sex
lme_cmj_con_p_time_con_sex <- lme(cmj_con_p ~ time*condition*sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)
#Model 2 - Time * Condition + sex
lme_cmj_con_p_time_con_psex <- lme(cmj_con_p ~ time*condition+sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)


#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_p_time_con_sex_het <- lme(cmj_con_p ~ time*condition*sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|sex),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_p_time_con_psex_het <- lme(cmj_con_p ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|sex),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)


#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_p_time_con_sex_het_con <- lme(cmj_con_p ~ time*condition*sex,
                                          random=(~1 | id),
                                          weights = varIdent(form = ~1|condition),
                                          data = modex_xfit,
                                          method="ML",
                                          na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_p_time_con_psex_het_con <- lme(cmj_con_p ~ time*condition+sex,
                                           random=(~1 | id),
                                           weights = varIdent(form = ~1|condition),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)

anova(lme_cmj_con_p,
      lme_cmj_con_p_time_con_sex,
      lme_cmj_con_p_time_con_psex,
      lme_cmj_con_p_time_con_sex_het,
      lme_cmj_con_p_time_con_psex_het,
      lme_cmj_con_p_time_con_sex_het_con,
      lme_cmj_con_p_time_con_psex_het_con)

#Model 2 - Time * Condition + sex is the simplest
lme_cmj_con_p_time_con_psex <- lme(cmj_con_p ~ time*condition+sex,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)
#Check - ANOVA
anova(lme_cmj_con_p_time_con_psex)

#Refit to REML
reml_cmj_con_p_time_con_psex <- lme(cmj_con_p ~ time*condition+sex,
                                    random=(~1 | id),
                                    data = modex_xfit,
                                    method="REML",
                                    na.action=na.omit)
#Check - ANOVA
Anova(reml_cmj_con_p_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_con_p_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_cmj_con_p_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_cmj_con_p_time_con_psex)



######5.4.7: Mean Concentrentic power (N/kg)####
#Model 1 - Time * Condition * sex * order
lme_cmj_con_mean_power <- lme(cmj_con_mean_power ~ time*condition*sex*order,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Check - ANOVA
car::Anova(lme_cmj_con_mean_power, type = 3)
#No 4 way interaction - drop order

#Model 2 - Time * Condition * sex
lme_cmj_con_mean_power_time_con_sex <- lme(cmj_con_mean_power ~ time*condition*sex,
                                           random=(~1 | id),
                                           data = modex_xfit,
                                           method="ML",
                                           na.action=na.omit)
#Model 2 - Time * Condition + sex
lme_cmj_con_mean_power_time_con_mean_powersex <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                     random=(~1 | id),
                                                     data = modex_xfit,
                                                     method="ML",
                                                     na.action=na.omit)


#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_mean_power_time_con_sex_het <- lme(cmj_con_mean_power ~ time*condition*sex,
                                               random=(~1 | id),
                                               weights = varIdent(form = ~1|sex),
                                               data = modex_xfit,
                                               method="ML",
                                               na.action=na.omit)

#Model 4 - Time * Condition * sex, heterogeneous variances for sex
lme_cmj_con_mean_power_time_con_mean_powersex_het <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                         random=(~1 | id),
                                                         weights = varIdent(form = ~1|sex),
                                                         data = modex_xfit,
                                                         method="ML",
                                                         na.action=na.omit)


#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_mean_power_time_con_sex_het_con <- lme(cmj_con_mean_power ~ time*condition*sex,
                                                   random=(~1 | id),
                                                   weights = varIdent(form = ~1|condition),
                                                   data = modex_xfit,
                                                   method="ML",
                                                   na.action=na.omit)

#Model 6 - Time * Condition * sex, heterogeneous variances for condition
lme_cmj_con_mean_power_time_con_mean_powersex_het_con <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                             random=(~1 | id),
                                                             weights = varIdent(form = ~1|condition),
                                                             data = modex_xfit,
                                                             method="ML",
                                                             na.action=na.omit)

anova(lme_cmj_con_mean_power,
      lme_cmj_con_mean_power_time_con_sex,
      lme_cmj_con_mean_power_time_con_mean_powersex,
      lme_cmj_con_mean_power_time_con_sex_het,
      lme_cmj_con_mean_power_time_con_mean_powersex_het,
      lme_cmj_con_mean_power_time_con_sex_het_con,
      lme_cmj_con_mean_power_time_con_mean_powersex_het_con)

#Model 2 - Time * Condition + sex is the simplest
reml_cmj_con_mean_power_time_con_mean_powersex <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                      random=(~1 | id),
                                                      data = modex_xfit,
                                                      method="REML",
                                                      na.action=na.omit)

#Check - ANOVA
anova(reml_cmj_con_mean_power_time_con_mean_powersex)

#Refit to REML
reml_cmj_con_mean_power_time_con_mean_powersex <- lme(cmj_con_mean_power ~ time*condition+sex,
                                                      random=(~1 | id),
                                                      data = modex_xfit,
                                                      method="REML",
                                                      na.action=na.omit)
#Check - ANOVA
Anova(reml_cmj_con_mean_power_time_con_mean_powersex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_cmj_con_mean_power_time_con_mean_powersex)

#Plot - Model Effects
plot(allEffects(reml_cmj_con_mean_power_time_con_mean_powersex))

#Check - Model Table
sjPlot::tab_model(reml_cmj_con_mean_power_time_con_mean_powersex)



#####5.5: 1RM ####
#Model 1 - Time * Condition * sex * order
lme_rm_w_all <- lme(rm_w ~ time*condition*sex*order,
                    random=(~1 | id),
                    data = modex_xfit,
                    method="ML",
                    na.action=na.omit)
#Check - ANOVA
car::Anova(lme_rm_w_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_rm_w_time_con_sex <- lme(rm_w ~ time*condition*sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_rm_w_time_con_psex <- lme(rm_w ~ time*condition+sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)

#Model 4 - Time * Condition + sex + age
lme_rm_w_time_con_psex_page <- lme(rm_w ~ time*condition+sex+age,
                                   random=(~1 | id),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 5 - Time * Condition * age + sex
lme_rm_w_time_con_psex_age <- lme(rm_w ~ time*condition*age+sex,
                                  random=(~1 | id),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)
#Check - Model Comparisions
anova(lme_rm_w_all,
      lme_rm_w_time_con_sex,
      lme_rm_w_time_con_psex,
      lme_rm_w_time_con_psex_page,
      lme_rm_w_time_con_psex_age)

#Model 3 - Time * Condition + sex is the simplest model

#Model 6 - Time * Condition + sex + age  - Heterogeneous variances for sex
lme_rm_w_time_con_sex_het <- lme(rm_w ~ time*condition+sex,
                                 random=(~1 | id),
                                 weights = varIdent(form = ~1|sex),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 7 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_rm_w_time_con_sex_het_con <- lme(rm_w ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|condition),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Check - Model Comparisions
anova(lme_rm_w_time_con_psex,
      lme_rm_w_time_con_sex_het,
      lme_rm_w_time_con_sex_het_con)

#Model 6 - Time * Condition + sex, is simplest - refit to reml
reml_rm_w_time_con_sex_het <- lme(rm_w ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="REML",
                                  na.action=na.omit)

#Check - ANOVA
Anova(reml_rm_w_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_rm_w_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_rm_w_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_rm_w_time_con_sex_het)

#effect size 
eta_squared(reml_rm_w_time_con_sex_het, partial = TRUE)

####5.6: MOXY - SMO2####

######5.6.1: MG all####
#Model 1 - percentage*condition*time*sex*order
lme_smo2_mg_all <- lme(SmO2 ~ percentage*condition*time*sex*order,
                       random=(~1 | id),
                       data = mg_long,
                       method="ML",
                       na.action=na.omit)
#Check - ANOVA
car::Anova(lme_smo2_mg_all, type = 3)

##No 5 way interaction - order dropped##
#Model 2 
lme_smo2_mg_per_con_time_sex <- lme(SmO2 ~ percentage*condition*time*sex*order,
                                    random=(~1 | id),
                                    data = mg_long,
                                    method="ML",
                                    na.action=na.omit)
#Model 3 
lme_smo2_mg_per_con_time_psex <- lme(SmO2 ~ percentage*condition*time+sex,
                                     random=(~1 | id),
                                     data = mg_long,
                                     method="ML",
                                     na.action=na.omit)

#Model 4 
lme_smo2_mg_per_con_time <- lme(SmO2 ~ percentage*condition*time,
                                random=(~1 | id),
                                data = mg_long,
                                method="ML",
                                na.action=na.omit)
#Check - Model Comparisions
anova(lme_smo2_mg_all,
      lme_smo2_mg_per_con_time_sex,
      lme_smo2_mg_per_con_time_psex,
      lme_smo2_mg_per_con_time)

#Model 4 is the simplest - check for heterogenous variances 
#Model 5 - Time * Condition + sex  - Heterogeneous variances for sex
lme_smo2_mg_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = mg_long,
                                   method="ML",
                                   na.action=na.omit)

#Model 6 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_smo2_mg_per_con_het_con <- lme(SmO2 ~ percentage*condition*time,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|condition),
                                   data = mg_long,
                                   method="ML",
                                   na.action=na.omit)

#Check - Model Comparisions
anova(lme_smo2_mg_per_con_time,
      lme_smo2_mg_per_con_het_sex,
      lme_smo2_mg_per_con_het_con)

#Model 4 is the simplest - change to REML
reml_smo2_mg_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = mg_long,
                                    method="REML",
                                    na.action=na.omit)

#Check - ANOVA
Anova(reml_smo2_mg_per_con_het_sex, type = 3)
anova(reml_smo2_mg_per_con_het_sex)

#Check - Model Diagnostics
performance::check_model(reml_smo2_mg_per_con_het_sex)

#Plot - Model Effects
plot(allEffects(reml_smo2_mg_per_con_het_sex))

#Check - Model Table
sjPlot::tab_model(reml_smo2_mg_per_con_het_sex, digits = 0)

######5.6.2: BF all####
#Model 1 - percentage*condition*time*sex*order
lme_smo2_bf_all <- lme(SmO2 ~ percentage*condition*time*sex*order,
                       random=(~1 | id),
                       data = bf_long,
                       method="ML",
                       na.action=na.omit)
#Check - ANOVA
car::Anova(lme_smo2_bf_all, type = 3)

##No 5 way interaction - order dropped##
#Model 2 
lme_smo2_bf_per_con_time_sex <- lme(SmO2 ~ percentage*condition*time*sex*order,
                                    random=(~1 | id),
                                    data = bf_long,
                                    method="ML",
                                    na.action=na.omit)
#Model 3 
lme_smo2_bf_per_con_time_psex <- lme(SmO2 ~ percentage*condition*time+sex,
                                     random=(~1 | id),
                                     data = bf_long,
                                     method="ML",
                                     na.action=na.omit)

#Model 4 
lme_smo2_bf_per_con_time <- lme(SmO2 ~ percentage*condition*time,
                                random=(~1 | id),
                                data = bf_long,
                                method="ML",
                                na.action=na.omit)
#Check - Model Comparisions
anova(lme_smo2_bf_all,
      lme_smo2_bf_per_con_time_sex,
      lme_smo2_bf_per_con_time_psex,
      lme_smo2_bf_per_con_time)

#Model 3 is the simplest - check for heterogenous variances 
#Model 5 - Time * Condition + sex  - Heterogeneous variances for sex
lme_smo2_bf_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = bf_long,
                                   method="ML",
                                   na.action=na.omit)

#Model 6 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_smo2_bf_per_con_het_con <- lme(SmO2 ~ percentage*condition*time+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|condition),
                                   data = bf_long,
                                   method="ML",
                                   na.action=na.omit)

#Check - Model Comparisions
anova(lme_smo2_bf_per_con_time_psex,
      lme_smo2_bf_per_con_het_sex,
      lme_smo2_bf_per_con_het_con)

#Model 4 is the simplest - change to REML
reml_smo2_bf_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time+sex,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = bf_long,
                                    method="REML",
                                    na.action=na.omit)

#Check - ANOVA
Anova(reml_smo2_bf_per_con_het_sex, type = 3)
anova(reml_smo2_bf_per_con_het_sex)

#Check - Model Diagnostics
performance::check_model(reml_smo2_bf_per_con_het_sex)

#Plot - Model Effects
plot(allEffects(reml_smo2_bf_per_con_het_sex))

#Check - Model Table
sjPlot::tab_model(reml_smo2_bf_per_con_het_sex, digits = 0)

######5.6.3: VL all####
#Model 1 - percentage*condition*time*sex*order
lme_smo2_vl_all <- lme(SmO2 ~ percentage*condition*time*sex*order,
                       random=(~1 | id),
                       data = vl_long,
                       method="ML",
                       na.action=na.omit)
#Check - ANOVA
car::Anova(lme_smo2_vl_all, type = 3)

##No 5 way interaction - order dropped##
#Model 2 
lme_smo2_vl_per_con_time_sex <- lme(SmO2 ~ percentage*condition*time*sex*order,
                                    random=(~1 | id),
                                    data = vl_long,
                                    method="ML",
                                    na.action=na.omit)
#Model 3 
lme_smo2_vl_per_con_time_psex <- lme(SmO2 ~ percentage*condition*time+sex,
                                     random=(~1 | id),
                                     data = vl_long,
                                     method="ML",
                                     na.action=na.omit)

#Model 4 
lme_smo2_vl_per_con_time <- lme(SmO2 ~ percentage*condition*time,
                                random=(~1 | id),
                                data = vl_long,
                                method="ML",
                                na.action=na.omit)
#Check - Model Comparisions
anova(lme_smo2_vl_all,
      lme_smo2_vl_per_con_time_sex,
      lme_smo2_vl_per_con_time_psex,
      lme_smo2_vl_per_con_time)

#Model 4 is the simplest - check for heterogenous variances 
#Model 5 - Time * Condition + sex  - Heterogeneous variances for sex
lme_smo2_vl_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = vl_long,
                                   method="ML",
                                   na.action=na.omit)

#Model 6 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_smo2_vl_per_con_het_con <- lme(SmO2 ~ percentage*condition*time,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|condition),
                                   data = vl_long,
                                   method="ML",
                                   na.action=na.omit)

#Check - Model Comparisions
anova(lme_smo2_vl_per_con_time,
      lme_smo2_vl_per_con_het_sex,
      lme_smo2_vl_per_con_het_con)

#Model 4 is the simplest - change to REML
reml_smo2_vl_per_con_het_sex <- lme(SmO2 ~ percentage*condition*time,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = vl_long,
                                    method="REML",
                                    na.action=na.omit)

#Check - ANOVA
Anova(reml_smo2_vl_per_con_het_sex, type = 3)
anova(reml_smo2_vl_per_con_het_sex)

#Check - Model Diagnostics
performance::check_model(reml_smo2_vl_per_con_het_sex)

#Plot - Model Effects
plot(allEffects(reml_smo2_vl_per_con_het_sex))

#Check - Model Table
sjPlot::tab_model(reml_smo2_vl_per_con_het_sex, digits = 0)

######5.6.4: MG 25####
#Model 1 - Time * Condition * sex * order
lme_mg_25_all <- lme(mg_25 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_mg_25_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_mg_25_time_con_sex <- lme(mg_25 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_mg_25_time_con_psex <- lme(mg_25 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#Check - Model Comparisions
anova(lme_mg_25_all,
      lme_mg_25_time_con_sex,
      lme_mg_25_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_mg_25_time_con_sex_het <- lme(mg_25 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_mg_25_time_con_sex_het_con <- lme(mg_25 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_mg_25_time_con_psex,
      lme_mg_25_time_con_sex_het,
      lme_mg_25_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_mg_25_time_con_sex_het <- lme(mg_25 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="REML",
                                   na.action=na.omit)

#Check - ANOVA
Anova(reml_mg_25_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_mg_25_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_mg_25_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_mg_25_time_con_sex_het, digits = 0)


######5.6.5: MG 50####
#Model 1 - Time * Condition * sex * order
lme_mg_50_all <- lme(mg_50 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_mg_50_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_mg_50_time_con_sex <- lme(mg_50 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_mg_50_time_con_psex <- lme(mg_50 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_mg_50_all,
      lme_mg_50_time_con_sex,
      lme_mg_50_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_mg_50_time_con_sex_het <- lme(mg_50 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_mg_50_time_con_sex_het_con <- lme(mg_50 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_mg_50_time_con_psex,
      lme_mg_50_time_con_sex_het,
      lme_mg_50_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_mg_50_time_con_sex_het <- lme(mg_50 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="REML",
                                   na.action=na.omit)

#Check - ANOVA
Anova(reml_mg_50_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_mg_50_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_mg_50_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_mg_50_time_con_sex_het, digits = 0)

######5.6.6: MG 75####
#Model 1 - Time * Condition * sex * order
lme_mg_75_all <- lme(mg_75 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_mg_75_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_mg_75_time_con_sex <- lme(mg_75 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_mg_75_time_con_psex <- lme(mg_75 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_mg_75_all,
      lme_mg_75_time_con_sex,
      lme_mg_75_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_mg_75_time_con_sex_het <- lme(mg_75 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_mg_75_time_con_sex_het_con <- lme(mg_75 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_mg_75_time_con_psex,
      lme_mg_75_time_con_sex_het,
      lme_mg_75_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_mg_75_time_con_sex_het <- lme(mg_75 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="REML",
                                   na.action=na.omit)

#Check - ANOVA
Anova(reml_mg_75_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_mg_75_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_mg_75_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_mg_75_time_con_sex_het)

######5.6.7: MG max####
#Model 1 - Time * Condition * sex * order
lme_mg_100_all <- lme(mg_100 ~ time*condition*sex*order,
                      random=(~1 | id),
                      data = modex_xfit,
                      method="ML",
                      na.action=na.omit)
#Check - ANOVA
car::Anova(lme_mg_100_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_mg_100_time_con_sex <- lme(mg_100 ~ time*condition*sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_mg_100_time_con_psex <- lme(mg_100 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_mg_100_all,
      lme_mg_100_time_con_sex,
      lme_mg_100_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_mg_100_time_con_sex_het <- lme(mg_100 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_mg_100_time_con_sex_het_con <- lme(mg_100 ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Check - Model Comparisions
anova(lme_mg_100_time_con_psex,
      lme_mg_100_time_con_sex_het,
      lme_mg_100_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_mg_100_time_con_sex_het <- lme(mg_100 ~ time*condition+sex,
                                    random=(~1 | id),
                                    weights = varIdent(form = ~1|sex),
                                    data = modex_xfit,
                                    method="REML",
                                    na.action=na.omit)

#Check - ANOVA
Anova(reml_mg_100_time_con_sex_het, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_mg_100_time_con_sex_het)

#Plot - Model Effects
plot(allEffects(reml_mg_100_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_mg_100_time_con_sex_het)

######5.6.8: BF 25####
#Model 1 - Time * Condition * sex * order
lme_bf_25_all <- lme(bf_25 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_bf_25_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_bf_25_time_con_sex <- lme(bf_25 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_bf_25_time_con_psex <- lme(bf_25 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_bf_25_all,
      lme_bf_25_time_con_sex,
      lme_bf_25_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_bf_25_time_con_sex_het <- lme(bf_25 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_bf_25_time_con_sex_het_con <- lme(bf_25 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_bf_25_time_con_psex,
      lme_bf_25_time_con_sex_het,
      lme_bf_25_time_con_sex_het_con)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
reml_bf_25_time_con_sex_het_con <- lme(bf_25 ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="REML",
                                       na.action=na.omit)

#Check - ANOVA
Anova(reml_bf_25_time_con_sex_het_con, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_bf_25_time_con_sex_het_con)

#Plot - Model Effects
plot(allEffects(reml_bf_25_time_con_sex_het_con))

#Check - Model Table
sjPlot::tab_model(reml_bf_25_time_con_sex_het_con)

######5.6.9: BF 50####
#Model 1 - Time * Condition * sex * order
lme_bf_50_all <- lme(bf_50 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_bf_50_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_bf_50_time_con_sex <- lme(bf_50 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_bf_50_time_con_psex <- lme(bf_50 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_bf_50_all,
      lme_bf_50_time_con_sex,
      lme_bf_50_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_bf_50_time_con_sex_het <- lme(bf_50 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_bf_50_time_con_sex_het_con <- lme(bf_50 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_bf_50_time_con_psex,
      lme_bf_50_time_con_sex_het,
      lme_bf_50_time_con_sex_het_con)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions is simplest - refit to reml
reml_bf_50_time_con_sex_het_con <- lme(bf_50 ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="REML",
                                       na.action=na.omit)


#Check - ANOVA
Anova(reml_bf_50_time_con_sex_het_con, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_bf_50_time_con_sex_het_con)

#Plot - Model Effects
plot(allEffects(reml_bf_50_time_con_sex_het_con))

#Check - Model Table
sjPlot::tab_model(reml_bf_50_time_con_sex_het_con)

######5.6.10: BF 75####
#Model 1 - Time * Condition * sex * order
lme_bf_75_all <- lme(bf_75 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_bf_75_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_bf_75_time_con_sex <- lme(bf_75 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_bf_75_time_con_psex <- lme(bf_75 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_bf_75_all,
      lme_bf_75_time_con_sex,
      lme_bf_75_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_bf_75_time_con_sex_het <- lme(bf_75 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_bf_75_time_con_sex_het_con <- lme(bf_75 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_bf_75_time_con_psex,
      lme_bf_75_time_con_sex_het,
      lme_bf_75_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_bf_75_time_con_psex <- lme(bf_75 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="REML",
                                na.action=na.omit)

#Check - ANOVA
Anova(reml_bf_75_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_bf_75_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_bf_75_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_bf_75_time_con_psex)

######5.6.11 BF 100####
#Model 1 - Time * Condition * sex * order
lme_bf_100_all <- lme(bf_100 ~ time*condition*sex*order,
                      random=(~1 | id),
                      data = modex_xfit,
                      method="ML",
                      na.action=na.omit)
#Check - ANOVA
car::Anova(lme_bf_100_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_bf_100_time_con_sex <- lme(bf_100 ~ time*condition*sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_bf_100_time_con_psex <- lme(bf_100 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_bf_100_all,
      lme_bf_100_time_con_sex,
      lme_bf_100_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_bf_100_time_con_sex_het <- lme(bf_100 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_bf_100_time_con_sex_het_con <- lme(bf_100 ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Check - Model Comparisions
anova(lme_bf_100_time_con_psex,
      lme_bf_100_time_con_sex_het,
      lme_bf_100_time_con_sex_het_con)

#Model 4 - Time * Condition + sex   is simplest - refit to reml
reml_bf_100_time_con_psex <- lme(bf_100 ~ time*condition+sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="REML",
                                 na.action=na.omit)

#Check - ANOVA
Anova(reml_bf_100_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_bf_100_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_bf_100_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_bf_100_time_con_psex)

######5.6.12: MOXY - VL 25####
#Model 1 - Time * Condition * sex * order
lme_vl_25_all <- lme(vl_25 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_vl_25_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_vl_25_time_con_sex <- lme(vl_25 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_vl_25_time_con_psex <- lme(vl_25 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_vl_25_all,
      lme_vl_25_time_con_sex,
      lme_vl_25_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_vl_25_time_con_sex_het <- lme(vl_25 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_vl_25_time_con_sex_het_con <- lme(vl_25 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_vl_25_time_con_psex,
      lme_vl_25_time_con_sex_het,
      lme_vl_25_time_con_sex_het_con)

#Model 5 - Time * Condition + sex 
reml_vl_25_time_con_psex <- lme(vl_25 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="REML",
                                na.action=na.omit)

#Check - ANOVA
Anova(reml_vl_25_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_vl_25_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_vl_25_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_vl_25_time_con_psex)

######5.6.13: MOXY - VL 50####
#Model 1 - Time * Condition * sex * order
lme_vl_50_all <- lme(vl_50 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_vl_50_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_vl_50_time_con_sex <- lme(vl_50 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_vl_50_time_con_psex <- lme(vl_50 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_vl_50_all,
      lme_vl_50_time_con_sex,
      lme_vl_50_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_vl_50_time_con_sex_het <- lme(vl_50 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_vl_50_time_con_sex_het_con <- lme(vl_50 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_vl_50_time_con_psex,
      lme_vl_50_time_con_sex_het,
      lme_vl_50_time_con_sex_het_con)

#Model 5 - Time * Condition * sex  - refit to reml
reml_vl_50_time_con_psex <- lme(vl_50 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="REML",
                                na.action=na.omit)


#Check - ANOVA
Anova(reml_vl_50_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_vl_50_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_vl_50_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_vl_50_time_con_psex)

######5.6.14: MOXY - VL 75####
#Model 1 - Time * Condition * sex * order
lme_vl_75_all <- lme(vl_75 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_vl_75_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_vl_75_time_con_sex <- lme(vl_75 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_vl_75_time_con_psex <- lme(vl_75 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_vl_75_all,
      lme_vl_75_time_con_sex,
      lme_vl_75_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_vl_75_time_con_sex_het <- lme(vl_75 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition * sex - Heterogeneous variances for Conditions
lme_vl_75_time_con_sex_het_con <- lme(vl_75 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_vl_75_time_con_psex,
      lme_vl_75_time_con_sex_het,
      lme_vl_75_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to reml
reml_vl_75_time_con_psex <- lme(vl_75 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="REML",
                                na.action=na.omit)

#Check - ANOVA
Anova(reml_vl_75_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_vl_75_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_vl_75_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_vl_75_time_con_psex)

######5.6.15: MOXY - VL 100####
#Model 1 - Time * Condition * sex * order
lme_vl_100_all <- lme(vl_100 ~ time*condition*sex*order,
                      random=(~1 | id),
                      data = modex_xfit,
                      method="ML",
                      na.action=na.omit)
#Check - ANOVA
car::Anova(lme_vl_100_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_vl_100_time_con_sex <- lme(vl_100 ~ time*condition*sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_vl_100_time_con_psex <- lme(vl_100 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="ML",
                                na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_vl_100_all,
      lme_vl_100_time_con_sex,
      lme_vl_100_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_vl_100_time_con_sex_het <- lme(vl_100 ~ time*condition+sex,
                                   random=(~1 | id),
                                   weights = varIdent(form = ~1|sex),
                                   data = modex_xfit,
                                   method="ML",
                                   na.action=na.omit)

#Model 5 - Time * Condition + sex - Heterogeneous variances for Conditions
lme_vl_100_time_con_sex_het_con <- lme(vl_100 ~ time*condition+sex,
                                       random=(~1 | id),
                                       weights = varIdent(form = ~1|condition),
                                       data = modex_xfit,
                                       method="ML",
                                       na.action=na.omit)

#Check - Model Comparisions
anova(lme_vl_100_time_con_psex,
      lme_vl_100_time_con_sex_het,
      lme_vl_100_time_con_sex_het_con)

#Model 4 - Time * Condition + sex   is simplest - refit to reml
reml_vl_100_time_con_psex <- lme(vl_100 ~ time*condition+sex,
                                 random=(~1 | id),
                                 data = modex_xfit,
                                 method="REML",
                                 na.action=na.omit)

#Check - ANOVA
Anova(reml_vl_100_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_vl_100_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_vl_100_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_vl_100_time_con_psex)

#####5.7 MOXY - POWER TERMS VO2####

######5.7.1: MOXY - W @ 25% VO2MAX#### 
#Model 1 - Time * Condition * sex * order
lme_p_25_all <- lme(p_25 ~ time*condition*sex*order,
                    random=(~1 | id),
                    data = modex_xfit,
                    method="ML",
                    na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_25_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_p_25_time_con_sex <- lme(p_25 ~ time*condition*sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_p_25_time_con_psex <- lme(p_25 ~ time*condition+sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_p_25_all,
      lme_p_25_time_con_sex,
      lme_p_25_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_p_25_time_con_sex_het <- lme(p_25 ~ time*condition+sex,
                                 random=(~1 | id),
                                 weights = varIdent(form = ~1|sex),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 5 - Time * Condition + sex - Heterogeneous variances for Conditions
lme_p_25_time_con_sex_het_con <- lme(p_25 ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|condition),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_25_time_con_psex,
      lme_p_25_time_con_sex_het,
      lme_p_25_time_con_sex_het_con)

#Model 4 - Time * Condition + sex   is simplest - refit to reml
reml_p_25_time_con_psex <- lme(p_25 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="REML",
                               na.action=na.omit)

#Check - ANOVA
Anova(reml_p_25_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_p_25_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_p_25_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_p_25_time_con_psex)

######5.7.2: MOXY - W @ 50% VO2MAX#### 
#Model 1 - Time * Condition * sex * order
lme_p_50_all <- lme(p_50 ~ time*condition*sex*order,
                    random=(~1 | id),
                    data = modex_xfit,
                    method="ML",
                    na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_50_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_p_50_time_con_sex <- lme(p_50 ~ time*condition*sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_p_50_time_con_psex <- lme(p_50 ~ time*condition+sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_p_50_all,
      lme_p_50_time_con_sex,
      lme_p_50_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_p_50_time_con_sex_het <- lme(p_50 ~ time*condition+sex,
                                 random=(~1 | id),
                                 weights = varIdent(form = ~1|sex),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 5 - Time * Condition + sex - Heterogeneous variances for Conditions
lme_p_50_time_con_sex_het_con <- lme(p_50 ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|condition),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_50_time_con_psex,
      lme_p_50_time_con_sex_het,
      lme_p_50_time_con_sex_het_con)

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex is simplest - refit to REML
reml_p_50_time_con_sex_het <- lme(p_50 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="REML",
                                  na.action=na.omit)

#Check - ANOVA
Anova(reml_p_50_time_con_sex_het , type = 3)

#Check - Model Diagnostics
performance::check_model(reml_p_50_time_con_sex_het )

#Plot - Model Effects
plot(allEffects(reml_p_50_time_con_sex_het))

#Check - Model Table
sjPlot::tab_model(reml_p_50_time_con_sex_het)

######5.7.3: MOXY - W 75% VO2MAX#### 
#Model 1 - Time * Condition * sex * order
lme_p_75_all <- lme(p_75 ~ time*condition*sex*order,
                    random=(~1 | id),
                    data = modex_xfit,
                    method="ML",
                    na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_75_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_p_75_time_con_sex <- lme(p_75 ~ time*condition*sex,
                             random=(~1 | id),
                             data = modex_xfit,
                             method="ML",
                             na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_p_75_time_con_psex <- lme(p_75 ~ time*condition+sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_p_75_all,
      lme_p_75_time_con_sex,
      lme_p_75_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_p_75_time_con_sex_het <- lme(p_75 ~ time*condition+sex,
                                 random=(~1 | id),
                                 weights = varIdent(form = ~1|sex),
                                 data = modex_xfit,
                                 method="ML",
                                 na.action=na.omit)

#Model 5 - Time * Condition + sex - Heterogeneous variances for Conditions
lme_p_75_time_con_sex_het_con <- lme(p_75 ~ time*condition+sex,
                                     random=(~1 | id),
                                     weights = varIdent(form = ~1|condition),
                                     data = modex_xfit,
                                     method="ML",
                                     na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_75_time_con_psex,
      lme_p_75_time_con_sex_het,
      lme_p_75_time_con_sex_het_con)

#Model 4 - Time * Condition + sex   is simplest - refit to reml
reml_p_75_time_con_psex <- lme(p_75 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="REML",
                               na.action=na.omit)

#Check - ANOVA
Anova(reml_p_75_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_p_75_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_p_75_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_p_75_time_con_psex)

######5.7.4: MOXY - W 100% VO2MAX#### 
#Model 1 - Time * Condition * sex * order
lme_p_100_all <- lme(p_100 ~ time*condition*sex*order,
                     random=(~1 | id),
                     data = modex_xfit,
                     method="ML",
                     na.action=na.omit)
#Check - ANOVA
car::Anova(lme_p_100_all, type = 3)
##No 4 way interaction - order dropped##

#Model 2 - Time * Condition * sex
lme_p_100_time_con_sex <- lme(p_100 ~ time*condition*sex,
                              random=(~1 | id),
                              data = modex_xfit,
                              method="ML",
                              na.action=na.omit)
#Model 3 - Time * Condition + sex
lme_p_100_time_con_psex <- lme(p_100 ~ time*condition+sex,
                               random=(~1 | id),
                               data = modex_xfit,
                               method="ML",
                               na.action=na.omit)

#
#Check - Model Comparisions
anova(lme_p_100_all,
      lme_p_100_time_con_sex,
      lme_p_100_time_con_psex)

#Model 3 - Time * Condition + sex is the simplest model

#Model 4 - Time * Condition + sex  - Heterogeneous variances for sex
lme_p_100_time_con_sex_het <- lme(p_100 ~ time*condition+sex,
                                  random=(~1 | id),
                                  weights = varIdent(form = ~1|sex),
                                  data = modex_xfit,
                                  method="ML",
                                  na.action=na.omit)

#Model 5 - Time * Condition + sex - Heterogeneous variances for Conditions
lme_p_100_time_con_sex_het_con <- lme(p_100 ~ time*condition+sex,
                                      random=(~1 | id),
                                      weights = varIdent(form = ~1|condition),
                                      data = modex_xfit,
                                      method="ML",
                                      na.action=na.omit)

#Check - Model Comparisions
anova(lme_p_100_time_con_psex,
      lme_p_100_time_con_sex_het,
      lme_p_100_time_con_sex_het_con)

#Model 4 - Time * Condition + sex   is simplest - refit to reml
reml_p_100_time_con_psex <- lme(p_100 ~ time*condition+sex,
                                random=(~1 | id),
                                data = modex_xfit,
                                method="REML",
                                na.action=na.omit)

#Check - ANOVA
Anova(reml_p_100_time_con_psex, type = 3)

#Check - Model Diagnostics
performance::check_model(reml_p_100_time_con_psex)

#Plot - Model Effects
plot(allEffects(reml_p_100_time_con_psex))

#Check - Model Table
sjPlot::tab_model(reml_p_100_time_con_psex)


#####5.8 SUP BELIEF SURVEY#####
run_sup_clmm <- function(df,
                         outcome_prefix = "sup_",
                         id_var = "id",
                         predictor = "condition",
                         order_var = "order") {
  
  # identify all supplementary outcomes
  outcomes <- grep(paste0("^", outcome_prefix), names(df), value = TRUE)
  
  # ensure factors are correct
  df[[id_var]] <- factor(df[[id_var]])
  df[[predictor]] <- factor(df[[predictor]])
  df[[order_var]] <- factor(df[[order_var]])
  
  # ensure ordinal outcomes are ordered
  df[outcomes] <- lapply(df[outcomes], function(x) {
    factor(x, ordered = TRUE)
  })
  
  # fit CLMM models
  models <- lapply(outcomes, function(var) {
    
    f <- as.formula(
      paste0(var, " ~ ", predictor, " + ", order_var, " + (1 | ", id_var, ")")
    )
    
    clmm(f, data = df, link = "logit")
  })
  
  names(models) <- outcomes
  return(models)
}

#LIKERT ANALYSIS GRAPH#
# Remove rows with NA in any Likert item
sup_clean <- modex_supfit %>%
  filter(
    !is.na(sup_performance) &
      !is.na(sup_confidence) &
      !is.na(sup_recovery) &
      !is.na(sup_potential) &
      !is.na(sup_training))

# 2. Set the consistent ordered levels (1–6) and rename
likert_levels <- c("1","2","3","4","5","6")

likert_labels <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree")

# 3. Convert items to ordered factors with new labels
belief_items <- sup_clean %>%
  dplyr::select(sup_performance, sup_confidence, sup_recovery, sup_potential, sup_training) %>%
  mutate(across(everything(),
                ~ factor(
                  as.character(.),
                  levels = likert_levels,
                  labels = likert_labels,
                  ordered = TRUE)))

# 4. Dummy rows to enforce all levels
dummy <- data.frame(
  sup_performance = factor(likert_levels, levels = likert_levels, labels = likert_labels, ordered = TRUE),
  sup_confidence  = factor(likert_levels, levels = likert_levels, labels = likert_labels, ordered = TRUE),
  sup_recovery    = factor(likert_levels, levels = likert_levels, labels = likert_labels, ordered = TRUE),
  sup_potential   = factor(likert_levels, levels = likert_levels, labels = likert_labels, ordered = TRUE),
  sup_training    = factor(likert_levels, levels = likert_levels, labels = likert_labels, ordered = TRUE))

# 5. Append dummy rows
items_with_dummy <- rbind(belief_items, dummy)

# 6. Keep only the original participant rows
items_expanded <- items_with_dummy[1:nrow(belief_items), ]
items_expanded <- as.data.frame(items_expanded)

# 7. RENAME ITEM LABELS HERE
colnames(items_expanded) <- c(
  "Supplement improved my performance",
  "Supplement improved my confidence",
  "Supplement improved my recovery",
  "Supplement helped me realise my potential",
  "Supplement improved my quality of training")

# 8. Grouping variable
grouping_expanded <- factor(sup_clean$condition)

sup_models <- run_sup_clmm(sup_clean)
lapply(sup_models, summary)

#####5.9 BLINDING DATA#####

# Correct guesses in Group A are "Guessed A". Incorrect are "Guessed B".
nAA <- 4 # Number in Actual A who Guessed A
nBA <- 3 # Number in Actual A who Guessed B

# Correct guesses in Group B are "Guessed B". Incorrect are "Guessed A".
nAB <- 1 # Number in Actual B who Guessed A (this is incorrect guess for B)
nBB <- 4 # Number in Actual B who Guessed B

# Create the 3x2 matrix (Guessed A, Guessed B, Don't Know) x (Actual A, Actual B)
blinding_matrix <- matrix(c(nAA, nAB, nBA, nBB, 0, 0), 
                          nrow = 3, 
                          ncol = 2, 
                          byrow = TRUE) # Fills the matrix row by row

# Add names for clarity in the output
rownames(blinding_matrix) <- c("Guessed A", "Guessed B", "Don't Know")
colnames(blinding_matrix) <- c("Actual A", "Actual B")

# View the matrix structure
print("The input data matrix:")
print(blinding_matrix)

# Calculate the Blinding Indices
results <- BI(blinding_matrix, conf.level = 0.95, group.names = c("Group A", "Group B"))

# Print the results
print("The Blinding Index Results:")
print(results)
