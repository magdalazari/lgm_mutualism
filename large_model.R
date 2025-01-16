#to check if script is reproducible 
rm(list=ls())



#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)

uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")

#this does not have visit_type
full_scores<- readRDS("full_scores.rds")

#scaling working memory (for both data frames)
full_scores$tfmri_nb_all_beh_ctotal_rate<-full_scores$tfmri_nb_all_beh_ctotal_rate*100

uncorrected_wm_visit$working_mem<-uncorrected_wm_visit$working_mem*100

#making wide 
full_scores_wide<-reshape(full_scores, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

#renaming columns
colnames(full_scores_wide)<-c("ID", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")



#####################################No need to run these 
#subsetting flanker and wm 

flanker_wm_scores<-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$flanker, uncorrected_wm_visit$working_mem)

#making wide 
flanker_wm_wide<-reshape(flanker_wm_scores, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")

#renaming columns
colnames(flanker_wm_wide)<-c("ID", "flanker_T1", "wm_T1", "flanker_T2", "wm_T2", "flanker_T3", "wm_T3")

#reordering 
flanker_wm_wide<-flanker_wm_wide[ ,c(1,2,4,6,3,5,7)]


#subsetting picture and making wide 
picture_uncorrected <-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$picture)
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")
colnames(picture_uncorrected_wide)<-c('ID','picture_T1', 'picture_T2', 'picture_T3')


#subsettiing pattern and making wide 
pattern_uncorrected<- data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$pattern)
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")
colnames(pattern_uncorrected_wide)<-c('ID','pattern_T1', 'pattern_T2', 'pattern_T3')

######subsetting picture vocabulary and reading 

picvocab_reading_scores<-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$pic_vocab, uncorrected_wm_visit$reading)

#making wide 
picvocab_reading_wide<-reshape(picvocab_reading_scores, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")

#renaming columns
colnames(picvocab_reading_wide)<-c("ID", "picvocab_T1", "reading_T1", "picvocab_T2", "reading_T2", "picvocab_T3", "reading_T3")

#reordering 
picvocab_reading_wide<-picvocab_reading_wide[ ,c(1,2,4,6,3,5,7)]

###Grouping Flanker and working memory in a model 

#Model A/standard: 1. int+sl, 2.fixed error var (what about wm?)

flanker_wm_model_A <- ' 
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 


flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

wm_T1~~b*wm_T1
wm_T2~~b*wm_T2
wm_T3~~b*wm_T3
'

#fit model
fit_flanker_wm_A<- growth(flanker_wm_model_A, data=flanker_wm_wide,missing='fiml')
summary(fit_flanker_wm_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Model B: 1. int+sl, 2. correlated error variances between tests 

flanker_wm_model_B <- ' 
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 


flanker_T1~~wm_T1
flanker_T2~~wm_T2
flanker_T3~~wm_T3
'

fit_flanker_wm_B<- growth(flanker_wm_model_B, data=flanker_wm_wide,missing='fiml')
summary(fit_flanker_wm_B, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Model C: 1. int+sl 2. wd (correlated error variance) 3. equal variance across waves for pairs of tests 

flanker_wm_model_C <- ' 
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 


flanker_T1~~wm_T1
flanker_T2~~wm_T2
flanker_T3~~wm_T3

flanker_T1~~a*wm_T1
flanker_T2~~a*wm_T2
flanker_T3~~a*wm_T3
'
fit_flanker_wm_C<- growth(flanker_wm_model, data=flanker_wm_wide,missing='fiml')
summary(fit_flanker_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



### Modeling picvocab and reading  

#Model A/standard: 1. int+sl, 2.fixed error var

picvocab_reading_model_A<-' 
picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3

reading_T1~~b*reading_T1
reading_T2~~b*reading_T2
reading_T3~~b*reading_T3
'

#fit model #check missing 
fit_picvocab_reading_A<- growth(picvocab_reading_model_A, data=picvocab_reading_wide, missing='fiml')
summary(fit_picvocab_reading_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Model B: 1. int+sl, 2. wd (correlated error variance)
picvocab_reading_model_B<- ' 
picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picvocab_intercept~~reading_intercept
picvocab_slope~~reading_slope
picvocab_intercept~~reading_slope
reading_intercept~~picvocab_slope 

picvocab_T1~~reading_T1
picvocab_T2~~reading_T2
picvocab_T3~~reading_T3
'

#fit model
fit_picvocab_reading_B<- growth(picvocab_reading_model_B, data=picvocab_reading_wide,missing='fiml')
summary(fit_picvocab_reading_B, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

picvocab_reading_wide_NOna <- picvocab_reading_wide[complete.cases(picvocab_reading_wide),]


#Model C:1. int+sl, 2. wd (correlated error variance) 3. equal variance across waves for pairs of tests 
picvocab_reading_model_C<- ' 
picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

picvocab_intercept~~reading_intercept
picvocab_slope~~reading_slope
picvocab_intercept~~reading_slope
reading_intercept~~picvocab_slope 

picvocab_T1~~reading_T1
picvocab_T2~~reading_T2
picvocab_T3~~reading_T3

picvocab_T1~~a*reading_T1
picvocab_T2~~a*reading_T2
picvocab_T3~~a*reading_T3
'

#fit model
fit_picvocab_reading_C<- growth(picvocab_reading_model_C, data=picvocab_reading_wide,missing='fiml')
summary(fit_picvocab_reading_C, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



#Model A/standard: 1. int+sl, 2.fixed error var

picture_pattern_model_A<-' 
picture_intercept=~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope=~ 0*picture_T1 +picture_T2 + 1*picture_T3 

pattern_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 


picture_T1~~a*picture_T1
picture_T2~~a*picture_T2
picture_T3~~a*picture_T3

pattern_T1~~b*pattern_T1
pattern_T2~~b*pattern_T2
pattern_T3~~b*pattern_T3
'

fit_picture_pattern<-growth(picture_pattern_model_A, data=full_scores_wide,missing='fiml')
summary(fit_picture_pattern, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



######Combining flanker, wm, picvocab, reading 

#subsetting flanker, wm, picvocab, reading 

flanker_wm_picvocab_reading_scores<-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$flanker, uncorrected_wm_visit$working_mem, uncorrected_wm_visit$pic_vocab, uncorrected_wm_visit$reading)

#making wide 
flanker_wm_picvocab_reading_wide<-reshape(flanker_wm_picvocab_reading_scores, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")

#renaming columns
colnames(flanker_wm_picvocab_reading_wide)<-c("ID", "flanker_T1", "wm_T1", "picvocab_T1", "reading_T1", "flanker_T2", "wm_T2", "picvocab_T2", "reading_T2", "flanker_T3", "wm_T3", "picvocab_T3", "reading_T3")


#Model A: 1. int+sl, 2. fixed error variances 
fl_wm_pv_r<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 

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
'

#fit model
fit_fl_wm_pv_r<-growth(fl_wm_pv_r, data=flanker_wm_picvocab_reading_wide,missing='fiml')
summary(fit_fl_wm_pv_r, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


predict_4v<-data.frame(predict(fit_fl_wm_pv_r))
cor(predict_4v, use='pairwise.complete.obs')

predict_pv_reading<-data.frame(predict(fit_picvocab_reading_A))
cor(predict_pv_reading, use='pairwise.complete.obs')

ggcorrplot(cor_picvocab_reading,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())



#combining all variables 
###Model A but with all of the variables 

full_model<- '
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

#I get the same warning whether I fix the error variance for wm or not (also tried not fixing er var fr any domain and still get it) 
#the individual wm model is ok without its error variances fixed though 

#fit model
fit_full<-growth(full_model, data=full_scores_wide,missing='fiml')
summary(fit_full, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
fitted.values(fit_full)

#nicks suggestion
fit_full<-growth(full_model, data=full_scores_wide,missing='fiml', estimator='mlr')
summary(fit_full, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#checking ???
Phi <- lavInspect(fit_full, "cov.lv")
eigen(Phi)
lavInspect(fit_full, "cov.lv")
