#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)

# Subsetting complete cases for 2 domains (picture vocabulary, reading), basis and linear 
# to see if the difference between predict and lavaan estimations decreases
# specified: slope and intercept, fixed error variances 
 

# Picvocab complete cases: 4271

complete.cases(picvocab_uncorrected_wide)
picvocab_complete <- picvocab_uncorrected_wide[complete.cases(picvocab_uncorrected_wide),]
dim(picvocab_complete)

# Reading complete cases: 4229

complete.cases(reading_uncorrected_wide)
reading_complete <- reading_uncorrected_wide[complete.cases(reading_uncorrected_wide),]
dim(reading_complete)


# Combined picture vocabulary and reading 

picvocab_reading_scores<-data.frame(uncorrected_wm_visit$ID, uncorrected_wm_visit$eventname, uncorrected_wm_visit$pic_vocab, uncorrected_wm_visit$reading)

# wide, renaming and reordering

picvocab_reading_wide<-reshape(picvocab_reading_scores, idvar = "uncorrected_wm_visit.ID", timevar = "uncorrected_wm_visit.eventname", direction = "wide")

colnames(picvocab_reading_wide)<-c("ID", "picvocab_T1", "reading_T1", "picvocab_T2", "reading_T2", "picvocab_T3", "reading_T3")

picvocab_reading_wide<-picvocab_reading_wide[ ,c("ID","picvocab_T1","picvocab_T2","picvocab_T3","reading_T1","reading_T2", "reading_T3")]

#complete cases picvocab-reading: 4228

complete.cases(picvocab_reading_wide)
picvocab_reading_complete <- picvocab_reading_wide[complete.cases(picvocab_reading_wide),]
dim(picvocab_reading_complete)

### Model fitting 

#Linear 

fit_linear_picvocab_constrained_complete<- growth(linear_picvocab_constrained, data=picvocab_reading_complete)
summary(fit_linear_picvocab_constrained_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

fit_linear_reading_constrained_complete<- growth(linear_reading_constrained, data=picvocab_reading_complete)
summary(fit_linear_reading_constrained_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#Basis 

fit_basis_picvocab_constrained_complete<- growth(basis_picvocab_constrained, data=picvocab_reading_complete)
summary(fit_basis_picvocab_constrained_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

fit_basis_reading_constrained_complete<- growth(basis_reading_constrained, data=picvocab_reading_complete)
summary(fit_basis_reading_constrained_complete, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


### Combined picvocab and reading 

# linear 

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

# Basis 

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


### Predict for the single domain complete models 

#Picture vocabulary linear (int-slope: 0.39)

predict_linear_picvocab_complete<-data.frame(predict(fit_linear_picvocab_constrained_complete))
cor(predict_linear_picvocab_complete)
plot(predict_linear_picvocab_complete)

#Picture vocabulary basis (int-slope:0.39)

predict_basis_picvocab_complete<-data.frame(predict(fit_basis_picvocab_constrained_complete))
cor(predict_basis_picvocab_complete)
plot(predict_basis_picvocab_complete)

#Reading linear (int-slope:0.23)

predict_linear_reading_complete<-data.frame(predict(fit_linear_reading_constrained_complete))
cor(predict_linear_reading_complete)
plot(predict_linear_reading_complete)

#Reading Basis (int-slope:0.2)

predict_basis_reading_complete<-data.frame(predict(fit_basis_reading_constrained_complete))
cor(predict_basis_reading_complete)
plot(predict_basis_reading_complete)

### Predict for combined models 

# Linear: slope-slope: 0.88

predict_picvocab_reading_linear_complete<-data.frame(predict(fit_picvocab_reading_linear_complete))
cor_predict_picvocab_reading_linear_complete<- cor(predict_picvocab_reading_linear_complete)

ggcorrplot(cor_predict_picvocab_reading_linear_complete,
           hc.order = TRUE,
           title = "Linear",
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())


#Basis: slope-slope: 0.85

predict_picvocab_reading_basis_complete<-data.frame(predict(fit_picvocab_reading_basis_complete))
cor_predict_picvocab_reading_basis_complete<- cor(predict_picvocab_reading_basis_complete)
ggcorrplot(cor_predict_picvocab_reading_basis_complete,
           hc.order = TRUE,
           type = 'lower',
           title = "Basis",
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())

### Lavaan covariances 

# Picture vocabulary linear (int-slope: 0.048)

summary(fit_linear_picvocab_constrained_complete, standardized = T)

# basis (int-slope: 0.05)

summary(fit_basis_picvocab_constrained_complete, standardized = T)

# Reading 
# linear (int-slope: -0.046)

summary(fit_linear_reading_constrained_complete, standardized = T)

#Reading Basis (int-slope: -0.055)

summary(fit_basis_reading_constrained_complete, standardized = T)

# Combined pv and reading
# linear (slope-slope: 0.64)

summary(fit_picvocab_reading_linear_complete, standardized = T)

# basis (slope-slope: 0.602)

summary(fit_picvocab_reading_basis_complete, standardized = T)

###same 
#Linear pv and reading: slope-slope est.std: 0.64 
standardizedSolution(fit_picvocab_reading_linear_complete)

#Basis pv and reading: slope-slope est.std: 0.602 
standardizedSolution(fit_picvocab_reading_basis_complete)
