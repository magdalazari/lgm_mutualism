#reading data from cleaning script 
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")


#loading packages
library(lavaan)
library(ggplot2)

###Picture Vocabulary 

#Linear model

picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")


#specify model
linear_growth_model_picvocab <- ' 
verbal_intercept =~ 1*T1_verbal + 1*T2_verbal + 1*T3_verbal + 1*T4_verbal
verbal_slope =~ 0*T1_verbal + 1*T2_verbal + 3*T3_verbal + 5*T4_verbal 
'
#fit model
fit_linear_growth_model_verbal <- growth(linear_growth_model_verbal, data=wisc_verbal,missing='fiml')
summary(fit_linear_growth_model_verbal, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
