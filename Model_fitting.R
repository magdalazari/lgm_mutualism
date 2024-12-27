#reading data from cleaning script 
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")


#loading packages
library(lavaan)
library(ggplot2)
library(tidyverse)

#####Picture Vocabulary 

#making wide & renaming columns (again?!)
picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(picvocab_uncorrected_wide)<-c('ID','picvocab_T1', 'picvocab_T2', 'picvocab_T3')

##equality constraints of residual error variances to get more degrees of freedom for comparison (homogeneity of variances)

###Linear model
linear_picvocab_constrained <- ' 
picvocab_linear_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_linear_slope_c =~ 0*picvocab_T1 + 1*picvocab_T2 + 2*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'
#like mixed models, which assume that across time the error variance is the same

#fit model
fit_linear_picvocab_constrained<- growth(linear_picvocab_constrained, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_linear_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

#specify model 
basis_picvocab_constrained <- ' 
picvocab_basis_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope_c =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'
#alternative way/string manipulations? instead of rewriting the same code (best for longer model specifications maybe)
#maybe nor correct, check 
constrained_error <- "picvocab_T1 ~~ a*picvocab_T1, picvocab_T2~~a*picvocab_T2, picvocab_T3~~a*picvocab_T3"
picvocab_basis_err <- paste0(basis_picvocab, constrained_error)

basis_picvocab_constrained <- ' 
picvocab_basis_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope_c =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_basis_err' 

#fit model
fit_basis_picvocab_constrained<- growth(basis_picvocab_constrained, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_basis_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#comparing model fit for the 2 models after constraining error variance
#(so that it wont be estimated uniquely -automatically- by growth function, we gain 2 df for each model)
anova(fit_basis_picvocab_constrained,fit_linear_picvocab_constrained)


#predicted scores at baseline and predicted scores of change #same as linear, check
predicted_scores_basis_picvocab<-data.frame(predict(fit_basis_picvocab))
plot(predicted_scores_basis_picvocab)


#####Flanker 

#making wide & renaming columns 
flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(flanker_uncorrected_wide)<-c('ID','flanker_T1', 'flanker_T2', 'flanker_T3')


###Linear model


#specify model (with constrained variances)
linear_flanker <- ' 
flanker_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope =~ 0*flanker_T1 + 1*flanker_T2 + 2*flanker_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3
'
#fit model
fit_linear_flanker<- growth(linear_flanker, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_linear_flanker, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Basis model 

basis_flanker <- ' 
flanker_basis_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_slope =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3
'
#fit model
fit_basis_flanker<- growth(basis_flanker, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_basis_flanker, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_flanker, fit_linear_flanker)

#####Pattern

#making wide & renaming columns 
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(pattern_uncorrected_wide)<-c('ID','pattern_T1', 'pattern_T2', 'pattern_T3')

###Linear model 

#specify model
linear_pattern<- ' 
pattern_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope =~ 0*pattern_T1 + 1*pattern_T2 + 2*pattern_T3 


pattern_T1~~a*pattern_T1
pattern_T2~~a*pattern_T2
pattern_T3~~a*pattern_T3
'
#fit model
fit_linear_pattern<- growth(linear_pattern, data=pattern_uncorrected_wide,missing='fiml')
summary(fit_linear_pattern, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_pattern <- ' 
pattern_basis_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_basis_slope =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 


pattern_T1~~a*pattern_T1
pattern_T2~~a*pattern_T2
pattern_T3~~a*pattern_T3
'
#fit model
fit_basis_pattern<- growth(basis_pattern, data=pattern_uncorrected_wide,missing='fiml')
summary(fit_basis_pattern, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


anova(fit_basis_pattern, fit_linear_pattern)


#####Picture 

#making wide & renaming columns 
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(picture_uncorrected_wide)<-c('ID','picture_T1', 'picture_T2', 'picture_T3')


###Linear model 

#specify model
linear_picture<- ' 
picture_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + 1*picture_T2 + 2*picture_T3 


picture_T1~~a*picture_T1
picture_T2~~a*picture_T2
picture_T3~~a*picture_T3
'
#fit model
fit_linear_picture<- growth(linear_picture, data=picture_uncorrected_wide,missing='fiml')
summary(fit_linear_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 


basis_picture <- ' 
picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 


picture_T1~~a*picture_T1
picture_T2~~a*picture_T2
picture_T3~~a*picture_T3
'
#fit model
fit_basis_picture<- growth(basis_picture, data=picture_uncorrected_wide,missing='fiml')
summary(fit_basis_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_picture, fit_linear_picture)


#####Reading 

#making wide & renaming columns 
reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(reading_uncorrected_wide)<-c('ID','reading_T1', 'reading_T2', 'reading_T3')


###Linear model 

#specify model
linear_reading<- ' 
reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + 12*reading_T2 + 2*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
#fit model
fit_linear_reading<- growth(linear_reading, data=reading_uncorrected_wide,missing='fiml')
summary(fit_linear_reading, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_reading <- ' 
reading_basis_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
#fit model
fit_basis_reading<- growth(basis_reading, data=reading_uncorrected_wide,missing='fiml')
summary(fit_basis_reading, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_reading, fit_linear_reading)


#####Working memory

#making wide & renaming columns
wm_wide<-reshape(working_memory, idvar = "src_subject_id", timevar = "eventname", direction = "wide")
colnames(wm_wide)<-c('ID','wm_T1', 'wm_T2', 'wm_T3')

wm_multiplied<-mutate_if(wm_wide, is.numeric, ~ . * 100)

###Linear model (free error var)

#specify model
linear_wm<- ' 
wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + 1*wm_T2 + 2*wm_T3 

wm_T1~~a*wm_T1
wm_T2~~a*wm_T2
wm_T3~~a*wm_T3

'


#fit model
fit_linear_wm<- growth(linear_wm, data=wm_multiplied,missing='fiml')
summary(fit_linear_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model (free error var)

basis_wm <- ' 
wm_basis_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 
'
#fit model
fit_basis_wm<- growth(basis_wm, data=wm_multiplied,missing='fiml')
summary(fit_basis_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_wm, fit_linear_wm)
