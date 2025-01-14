#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)

uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")

#Script for: subsetting complete cases for picture vocabulary and reading to see what is going on
#speficied: slope and intercept, fixed error variances 


###Subseting + complete cases 

#Wide picture vocabulary 

picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(picvocab_uncorrected_wide)<-c('ID','picvocab_T1', 'picvocab_T2', 'picvocab_T3')

#complete cases: 4271

complete.cases(picvocab_uncorrected_wide)
picvocab_complete <- picvocab_uncorrected_wide[complete.cases(picvocab_uncorrected_wide),]
dim(picvocab_complete)


#Wide reading
reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(reading_uncorrected_wide)<-c('ID','reading_T1', 'reading_T2', 'reading_T3')

#complete cases: 4229

complete.cases(reading_uncorrected_wide)
reading_complete <- reading_uncorrected_wide[complete.cases(reading_uncorrected_wide),]
dim(reading_complete)


#Subsetting picture vocabulary and reading 

picvocab_reading_scores<-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$pic_vocab, uncorrected_wm_visit$reading)

#making wide 
picvocab_reading_wide<-reshape(picvocab_reading_scores, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")

#renaming columns
colnames(picvocab_reading_wide)<-c("ID", "picvocab_T1", "reading_T1", "picvocab_T2", "reading_T2", "picvocab_T3", "reading_T3")

#reordering 
picvocab_reading_wide<-picvocab_reading_wide[ ,c(1,2,4,6,3,5,7)]

#complete cases: 4228

complete.cases(picvocab_reading_wide)
picvocab_reading_complete <- picvocab_reading_wide[complete.cases(picvocab_reading_wide),]
dim(picvocab_reading_complete)

###Model fitting 

#####Picture Vocabulary 

#Linear 
linear_picvocab<- ' 
picvocab_linear_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_linear_slope_c =~ 0*picvocab_T1 + 1*picvocab_T2 + 2*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'
fit_picvocab_linear_complete<- growth(linear_picvocab, data=picvocab_complete)
summary(fit_picvocab_linear_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#Basis 
basis_picvocab <- ' 
picvocab_basis_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope_c =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'

fit_picvocab_basis_complete<- growth(basis_picvocab, data=picvocab_complete)
summary(fit_picvocab_basis_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#####Reading 

#Linear 
linear_reading<- ' 
reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + 1*reading_T2 + 2*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
fit_reading_linear_complete<- growth(linear_reading, data=reading_complete)
summary(fit_reading_linear_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Basis 
basis_reading <- ' 
reading_basis_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
fit_reading_basis_complete<- growth(basis_reading, data=reading_complete)
summary(fit_reading_basis_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Combined picvocab and reading linear 

linear_picvocab_reading<-' 
picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +1*picvocab_T2 + 2*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 +1*reading_T2 + 2*reading_T3 


picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3

reading_T1~~b*reading_T1
reading_T2~~b*reading_T2
reading_T3~~b*reading_T3
'

fit_picvocab_reading_linear_complete<- growth(linear_picvocab_reading, data=picvocab_reading_complete)
summary(fit_picvocab_reading_linear_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#Combined picvocab and reading basis 

basis_picvocab_reading<-' 
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

fit_picvocab_reading_basis_complete<- growth(basis_picvocab_reading, data=picvocab_reading_complete)
summary(fit_picvocab_reading_basis_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Predict for the single models 

#Picture vocabulary linear (int-slope: 0.392)

predict_picvocab_linear_complete<-data.frame(predict(fit_picvocab_linear_complete))
cor(predict_picvocab_linear_complete)

#Picture vocabulary basis (int-slope:0.388)

predict_picvocab_basis_complete<-data.frame(predict(fit_picvocab_basis_complete))


#Reading linear (int-slope:0.236)

predict_reading_linear_complete<-data.frame(predict(fit_reading_linear_complete))
cor(predict_reading_linear_complete)

#Reading Basis (int-slope:0.203)

predict_reading_basis_complete<-data.frame(predict(fit_reading_basis_complete))
cor(predict_reading_basis_complete)

###Predict for combined models 

#Linear: slope-slope: 0.88

predict_picvocab_reading_linear_complete<-data.frame(predict(fit_picvocab_reading_linear_complete))

cor_predict_picvocab_reading_linear_complete<- cor(predict_picvocab_reading_linear_complete)

ggcorrplot(cor_predict_picvocab_reading_linear_complete,
           hc.order = TRUE,
           title = "Linear",
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())

#Basis: slope-slope: 0.84

predict_picvocab_reading_basis_complete<-data.frame(predict(fit_picvocab_reading_basis_complete))

cor_predict_picvocab_reading_basis_complete<- cor(predict_picvocab_reading_basis_complete)

ggcorrplot(cor_predict_picvocab_reading_basis_complete,
           hc.order = TRUE,
           type = 'lower',
           title = "Basis",
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())

dim(predict_picvocab_reading_basis_complete)

###Lavaan covariances 

#Picture vocabulary linear (int-slope: 0.045)

summary(fit_picvocab_linear_complete, standardized = T)

#Picture vocabulary basis (int-slope: 0.047)

summary(fit_picvocab_basis_complete, standardized = T)

#Reading linear (int-slope: -0.046)

summary(fit_reading_linear_complete, standardized = T)

#Reading Basis (int-slope: -0.055)

summary(fit_reading_basis_complete, standardized = T)

#Combined linear (slope-slope: 0.640)

summary(fit_picvocab_reading_linear_complete, standardized = T)

#Combined basis (slope-slope: 0.602)

summary(fit_picvocab_reading_basis_complete, standardized = T)

