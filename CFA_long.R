#### ### ----------------- ### ####
# Nicholas Judd - njudd.com
# Donders Institute - LCD Lab
# File: long_CFA.R - Created on 2025-01-16
#### ### ----------------- ### ####

# Goals:

# packages & data
if (!require(pacman)){install.packages('pacman')}; options(scipen = 999)
pacman::p_load(tidyverse, lavaan)

uncorrected_wm_visit <- readRDS("~/surfdrive/Shared/uncorrected_wm_visit.rds")

#this does not have visit_type
full_scores<- readRDS("~/surfdrive/Shared/full_scores.rds")

#scaling working memory (for both data frames)
full_scores$tfmri_nb_all_beh_ctotal_rate<-full_scores$tfmri_nb_all_beh_ctotal_rate*100

uncorrected_wm_visit$working_mem<-uncorrected_wm_visit$working_mem*100

#making wide 
full_scores_wide<-reshape(full_scores, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

#renaming columns
colnames(full_scores_wide)<-c("ID", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")




#### ### ----------------- ### ####
# making latents per domain
#### ### ----------------- ### ####

# something messed up...

# using the CFA function instead
# https://rpubs.com/deondb/measurement_invariance


# baselineDomain_CFA<- '
# flanker=~ flanker_T1 + flanker_T2 + flanker_T3
# wm =~ wm_T1 + wm_T2 + wm_T3
# picvocab=~ picvocab_T1 + picvocab_T2 + picvocab_T3
# reading =~ reading_T1 + reading_T2 + reading_T3
# picture=~ picture_T1 + picture_T2 + picture_T3
# pattern=~ pattern_T1 + pattern_T2 + pattern_T3
# 
# # all of the covariances
# flanker ~~ wm + picvocab + reading + picture + pattern
# wm ~~ picvocab + reading + picture + pattern
# picvocab ~~ reading + picture + pattern
# reading ~~ picture + pattern
# picture ~~ pattern
# 
# # setting their variances to 1
# flanker ~~ 1*flanker
# wm ~~ 1*wm
# picvocab ~~ 1*picvocab
# reading ~~ 1*reading
# picture ~~ 1*picture
# pattern ~~ 1*pattern
# 
# flanker_T1~~flanker_T1
# flanker_T2~~flanker_T2
# flanker_T3~~flanker_T3
# 
# wm_T1~~wm_T1
# wm_T2~~wm_T2
# wm_T3~~wm_T3
# 
# picvocab_T1~~picvocab_T1
# picvocab_T2~~picvocab_T2
# picvocab_T3~~picvocab_T3
# 
# reading_T1~~reading_T1
# reading_T2~~reading_T2
# reading_T3~~reading_T3
# 
# picture_T1~~picture_T1
# picture_T2~~picture_T2
# picture_T3~~picture_T3
# 
# pattern_T1~~pattern_T1
# pattern_T2~~pattern_T2
# pattern_T3~~pattern_T3
# '

baselineDomain_CFA<- '
flanker=~ flanker_T1 + flanker_T2 + flanker_T3
wm =~ wm_T1 + wm_T2 + wm_T3
picvocab=~ picvocab_T1 + picvocab_T2 + picvocab_T3
reading =~ reading_T1 + reading_T2 + reading_T3
picture=~ picture_T1 + picture_T2 + picture_T3
pattern=~ pattern_T1 + pattern_T2 + pattern_T3'

#I get the same warning whether I fix the error variance for wm or not 
# (also tried not fixing er var fr any domain and still get it) 
#the individual wm model is ok without its error variances fixed though 

#fit model
# fit_full<-lavaan(baselineDomain_CFA, data=full_scores_wide, missing='fiml', estimator='mlr')
# summary(fit_full, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

fit_tasks <- cfa(model = baselineDomain_CFA,data = full_scores_wide, missing='fiml', estimator = 'mlr')
summary(fit_tasks, standardized = TRUE, fit.measures = TRUE)





#### ### ----------------- ### ####
# making longitudinal time CFA
#### ### ----------------- ### ####

# this is broken, i am forgetting something; using the CFA function
# long_CFA<- '
# T1 =~ flanker_T1 + wm_T1 + picvocab_T1 + reading_T1 + picture_T1 + pattern_T1
# T2 =~ flanker_T2 + wm_T2 + picvocab_T2 + reading_T2 + picture_T2 + pattern_T2
# T3 =~ flanker_T3 + wm_T3 + picvocab_T3 + reading_T3 + picture_T3 + pattern_T3
# 
# # all of the covariances
# T1 ~~ T2 + T3
# T2 ~~ T3
# 
# 
# # whoopsi daisies
# 
# # flanker_T1 ~~ flanker_T2 + flanker_T3
# # flanker_T2 ~~ flanker_T3
# # 
# # wm_T1 ~~ wm_T2 + wm_T3
# # wm_T2 ~~ wm_T3
# # 
# # picvocab_T1 ~~ picvocab_T2 + picvocab_T3
# # picvocab_T2 ~~ picvocab_T3
# # 
# # reading_T1 ~~ reading_T2 + reading_T3
# # reading_T2 ~~ reading_T3
# # 
# # picture_T1 ~~ picture_T2 + picture_T3
# # picture_T2 ~~ picture_T3
# # 
# # pattern_T1 ~~ pattern_T2 + pattern_T3
# # pattern_T2 ~~ pattern_T3
# 
# # errvar
# flanker_T1~~flanker_T1
# flanker_T2~~flanker_T2
# flanker_T3~~flanker_T3
# 
# wm_T1~~wm_T1
# wm_T2~~wm_T2
# wm_T3~~wm_T3
# 
# picvocab_T1~~picvocab_T1
# picvocab_T2~~picvocab_T2
# picvocab_T3~~picvocab_T3
# 
# reading_T1~~reading_T1
# reading_T2~~reading_T2
# reading_T3~~reading_T3
# 
# picture_T1~~picture_T1
# picture_T2~~picture_T2
# picture_T3~~picture_T3
# 
# pattern_T1~~pattern_T1
# pattern_T2~~pattern_T2
# pattern_T3~~pattern_T3
# '
# 
# fit_longCFA <- lavaan(long_CFA, data=full_scores_wide, missing='fiml', estimator='mlr')
# summary(fit_longCFA, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

long_CFA <- '
T1 =~ flanker_T1 + wm_T1 + picvocab_T1 + reading_T1 + picture_T1 + pattern_T1
T2 =~ flanker_T2 + wm_T2 + picvocab_T2 + reading_T2 + picture_T2 + pattern_T2
T3 =~ flanker_T3 + wm_T3 + picvocab_T3 + reading_T3 + picture_T3 + pattern_T3
'
fit_longCFA1 <- cfa(model = long_CFA,data = full_scores_wide, missing='fiml', estimator = 'mlr')
summary(fit_longCFA1, standardized = TRUE, fit.measures = TRUE)


woopsie_daysies <- '
flanker_T1 ~~ flanker_T2 + flanker_T3
flanker_T2 ~~ flanker_T3

wm_T1 ~~ wm_T2 + wm_T3
wm_T2 ~~ wm_T3

picvocab_T1 ~~ picvocab_T2 + picvocab_T3
picvocab_T2 ~~ picvocab_T3

reading_T1 ~~ reading_T2 + reading_T3
reading_T2 ~~ reading_T3

picture_T1 ~~ picture_T2 + picture_T3
picture_T2 ~~ picture_T3

pattern_T1 ~~ pattern_T2 + pattern_T3
pattern_T2 ~~ pattern_T3
'


fit_longCFA2 <- cfa(model = paste0(long_CFA, woopsie_daysies), missing='fiml', data = full_scores_wide, estimator = 'mlr')
summary(fit_longCFA2, standardized = TRUE, fit.measures = TRUE)









