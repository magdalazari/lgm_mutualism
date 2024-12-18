#reading data from cleaning script 
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")


#loading packages
library(lavaan)
library(ggplot2)

#####Picture Vocabulary 

#making wide & renaming columns (again?!)
picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(picvocab_uncorrected_wide)<-c('ID','picvocab_T1', 'picvocab_T2', 'picvocab_T3')


###Linear model

#specify model 
linear_picvocab <- ' 
picvocab_intercept =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope =~ 0*picvocab_T1 + 1*picvocab_T2 + 2*picvocab_T3 
'
#fit model
fit_linear_picvocab<- growth(linear_picvocab, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_linear_picvocab, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#extracting intercepts and slopes (for everyone?)
predicted_scores_linear_picvocab<-data.frame(predict(fit_linear_picvocab))
plot(predicted_scores_linear_picvocab)


###Basis model

#specify model (growth factors as latent variables)
basis_picvocab <- ' 
picvocab_basis_intercept =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 
'
#fit model
fit_basis_picvocab<- growth(basis_picvocab, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_basis_picvocab, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#predicted scores at baseline and predicted scores of change #same as linear, check
predicted_scores_basis_picvocab<-data.frame(predict(fit_basis_picvocab))
plot(predicted_scores_basis_picvocab)


#comparing model fit 
anova(fit_basis_picvocab,fit_linear_picvocab)





# Create quadratic growth model #MANY NAS 
quad_growth_model <- 'i =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3 
                      s =~ 0*picvocab_T1 + 1*picvocab_T2 + 2*picvocab_T3 
                      q =~ 0*picvocab_T1 + 1*picvocab_T2 + 4*picvocab_T3 '
# Fit model
fit_quad_growth_model <- growth(quad_growth_model, data=picvocab_uncorrected_wide,missing='fiml')
# Output results
summary(fit_quad_growth_model, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)
#



#####Flanker #RMSEA not good 

#making wide & renaming columns 
flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(flanker_uncorrected_wide)<-c('ID','flanker_T1', 'flanker_T2', 'flanker_T3')


###Linear model

#specify model
linear_flanker <- ' 
flanker_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope =~ 0*flanker_T1 + 1*flanker_T2 + 2*flanker_T3 
'
#fit model
fit_linear_flanker<- growth(linear_flanker, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_linear_flanker, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Basis model 

basis_flanker <- ' 
flanker_basis_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_slope =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 
'
#fit model
fit_basis_flanker<- growth(basis_flanker, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_basis_flanker, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#####Pattern

#making wide & renaming columns 
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(pattern_uncorrected_wide)<-c('ID','pattern_T1', 'pattern_T2', 'pattern_T3')

###Linear model #RMSEA not good 

#specify model
linear_pattern<- ' 
pattern_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope =~ 0*pattern_T1 + 1*pattern_T2 + 2*pattern_T3 
'
#fit model
fit_linear_pattern<- growth(linear_pattern, data=pattern_uncorrected_wide,missing='fiml')
summary(fit_linear_pattern, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_pattern <- ' 
pattern_basis_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_basis_slope =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 
'
#fit model
fit_basis_pattern<- growth(basis_pattern, data=pattern_uncorrected_wide,missing='fiml')
summary(fit_basis_pattern, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)




#####Picture 

#making wide & renaming columns 
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(picture_uncorrected_wide)<-c('ID','picture_T1', 'picture_T2', 'picture_T3')


###Linear model #RMSEA not good 

#specify model
linear_picture<- ' 
picture_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + 1*picture_T2 + 2*picture_T3 
'
#fit model
fit_linear_picture<- growth(linear_picture, data=picture_uncorrected_wide,missing='fiml')
summary(fit_linear_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 


basis_picture <- ' 
picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 
'
#fit model
fit_basis_picture<- growth(basis_picture, data=picture_uncorrected_wide,missing='fiml')
summary(fit_basis_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#####Reading 

#making wide & renaming columns 
reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
colnames(reading_uncorrected_wide)<-c('ID','reading_T1', 'reading_T2', 'reading_T3')


###Linear model 

#specify model
linear_reading<- ' 
reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + 12*reading_T2 + 2*reading_T3 
'
#fit model
fit_linear_reading<- growth(linear_reading, data=reading_uncorrected_wide,missing='fiml')
summary(fit_linear_reading, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_reading <- ' 
reading_basis_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 
'
#fit model
fit_basis_reading<- growth(basis_reading, data=reading_uncorrected_wide,missing='fiml')
summary(fit_basis_reading, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)



#####Working memory

#making wide & renaming columns
wm_wide<-reshape(working_memory, idvar = "src_subject_id", timevar = "eventname", direction = "wide")
colnames(wm_wide)<-c('ID','wm_T1', 'wm_T2', 'wm_T3')

###Linear model #RMSEA not good 

#specify model
linear_wm<- ' 
wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + 1*wm_T2 + 2*wm_T3 
'
#fit model
fit_linear_wm<- growth(linear_wm, data=wm_wide,missing='fiml')
summary(fit_linear_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_reading <- ' 
wm_basis_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 
'
#fit model
fit_basis_wm<- growth(basis_wm, data=wm_uncorrected_wide,missing='fiml')
summary(fit_basis_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

