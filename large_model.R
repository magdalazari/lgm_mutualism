#to check if script is reproducible 
rm(list=ls())

#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)


#checking at which stage the negative cov warning occurs 

#Model A: 1. int+sl, 2. fixed error var 
#Model B: 1. int+sl, 2. correlated error variances between tests
#Model C: 1. int+sl, 2. correlated error variances 3. equal variance across waves for pairs of tests
#Model D: 1. int+sl, 2. freed error variances 


#df with visit type
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")

#df without visit type
full_scores<- readRDS("full_scores.rds")

#scaling working memory (for both df)

full_scores$tfmri_nb_all_beh_ctotal_rate<-full_scores$tfmri_nb_all_beh_ctotal_rate*100
uncorrected_wm_visit$working_mem<-uncorrected_wm_visit$working_mem*100

#making wide 

full_scores_wide<-reshape(full_scores, idvar = "src_subject_id", timevar = "eventname", direction = "wide")
uncorrected_wm_visit_wide<-reshape(uncorrected_wm_visit, idvar="ID", timevar="eventname", direction = "wide")

#renaming columns

colnames(uncorrected_wm_visit_wide)<-c("ID","visit_T1", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1","visit_T2", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "visit_T3", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")
colnames(full_scores_wide)<-c("ID", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")



###All 6 cognitive domains 

full_model_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 


picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 

pattern_intercept=~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope=~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3


wm_T1~~b*wm_T1
wm_T2~~b*wm_T2
wm_T3~~b*wm_T3


picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3

picture_T1~~e*picture_T1
picture_T2~~e*picture_T2
picture_T3~~e*picture_T3

pattern_T1~~f*pattern_T1
pattern_T2~~f*pattern_T2
pattern_T3~~f*pattern_T3
'

#fit model
fit_full_A<-growth(full_model_A, data=full_scores_wide,missing='fiml')
summary(fit_full, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#not positive cov matrix, nothing looks bad in std.all 

#Removing working memory 

no_wm_model_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 

pattern_intercept=~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope=~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3

picture_T1~~e*picture_T1
picture_T2~~e*picture_T2
picture_T3~~e*picture_T3

pattern_T1~~f*pattern_T1
pattern_T2~~f*pattern_T2
pattern_T3~~f*pattern_T3
'

#fit model
fit_no_wm_A<-growth(no_wm_model_A, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#not positive cov matrix, std.all is ok  

#Removing working memory and picture

no_wm_picture_model_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

pattern_intercept=~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope=~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3

pattern_T1~~f*pattern_T1
pattern_T2~~f*pattern_T2
pattern_T3~~f*pattern_T3
'

#fit model
fit_no_wm_picture_A<-growth(no_wm_picture_model_A, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_picture_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#Different error: some estimated lv variances are negative. 
#Covariances: flanker and pattern slope std.all is 1.142
#slope for pattern can't be estimated 


#Model without working memory, picture and pattern

no_wm_picture_pattern_model_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3
'

#fit model
fit_no_wm_picture_pattern_A<-growth(no_wm_picture_pattern_model_A, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_picture_pattern_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#no warnings/negative values, the slope covariances look very small 


#Removing working memory and pattern (so adding picture)

no_wm_pattern_model_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3

picture_T1~~e*picture_T1
picture_T2~~e*picture_T2
picture_T3~~e*picture_T3
'

#fit model
fit_no_wm_pattern_A<-growth(no_wm_pattern_model_A, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_pattern_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#no warnings/negative values 
#picvocab slope to reading slope: 0.556
#picvocab slope to picture slope: 0.395

#removing just pattern (so adding working memory)

no_pattern_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3


wm_T1~~b*wm_T1
wm_T2~~b*wm_T2
wm_T3~~b*wm_T3


picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3

picture_T1~~e*picture_T1
picture_T2~~e*picture_T2
picture_T3~~e*picture_T3
'

#fit model
fit_no_pattern_A<-growth(no_pattern_A, data=full_scores_wide,missing='fiml')
summary(fit_no_pattern_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#get negative values warning, so probably wm and pattern are problematic 

#freeing the error variances to see what is happening

no_wm_pattern_model_D<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 
'

#fit model
fit_no_wm_pattern_D<-growth(no_wm_pattern_model_D, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_pattern_D, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


fitted.values(fit_full)

#nicks suggestion
fit_full<-growth(full_model, data=full_scores_wide,missing='fiml', estimator='mlr')
summary(fit_full, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#checking ???
Phi <- lavInspect(fit_full, "cov.lv")
eigen(Phi)
lavInspect(fit_full, "cov.lv")
