# just reading files from cleaning script
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")

uncorrected_wm_visit_wide <- readRDS("uncorrected_wm_visit_wide.rds")

#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)


### or running everything from cleaning script 

source("Data_cleaning.R")


####### Basis-linear comparison for each task to estimate best fit 

##equality constraints of residual error variances to get more degrees of freedom for comparison (homogeneity of variance)
#(like mixed models, which assume equal error variance across time)

##### Picture Vocabulary

###Linear model
linear_picvocab_constrained <- ' 
picvocab_linear_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_linear_slope_c =~ 0*picvocab_T1 + 1*picvocab_T2 + 2*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'

#fit model
fit_linear_picvocab_constrained<- growth(linear_picvocab_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
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


#fit model
fit_basis_picvocab_constrained<- growth(basis_picvocab_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#comparing model fit for the 2 models 

anova(fit_basis_picvocab_constrained,fit_linear_picvocab_constrained)



##### Flanker 

###Linear model

linear_flanker_constrained <- ' 
flanker_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope =~ 0*flanker_T1 + 1*flanker_T2 + 2*flanker_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3
'
#fit model
fit_linear_flanker_constrained<- growth(linear_flanker_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_linear_flanker_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Basis model 

basis_flanker_constrained <- ' 
flanker_basis_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_slope =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3
'
#fit model
fit_basis_flanker_constrained<- growth(basis_flanker_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_flanker_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_flanker_constrained, fit_linear_flanker_constrained)

##### Pattern

###Linear model 

#specify model
linear_pattern_constrained<- ' 
pattern_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_slope =~ 0*pattern_T1 + 1*pattern_T2 + 2*pattern_T3 


pattern_T1~~a*pattern_T1
pattern_T2~~a*pattern_T2
pattern_T3~~a*pattern_T3
'
#fit model
fit_linear_pattern_constrained<- growth(linear_pattern_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_linear_pattern_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_pattern_constrained <- ' 
pattern_basis_intercept =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_basis_slope =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 


pattern_T1~~a*pattern_T1
pattern_T2~~a*pattern_T2
pattern_T3~~a*pattern_T3
'
#fit model
fit_basis_pattern_constrained<- growth(basis_pattern_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_pattern_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


anova(fit_basis_pattern_constrained, fit_linear_pattern_constrained)


##### Picture 

# Here we freed the error variance in order to proceed with basis (when equal. constrained, linear was better)

###Linear model 

#specify model

linear_picture<- ' 
picture_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope =~ 0*picture_T1 + 1*picture_T2 + 2*picture_T3 
'
#fit model
fit_linear_picture<- growth(linear_picture, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_linear_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 


basis_picture <- ' 
picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope =~ 0*picture_T1 + picture_T2 + 1*picture_T3 
'
#fit model
fit_basis_picture<- growth(basis_picture, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_picture, fit_linear_picture)


#####Reading 

###Linear model 

linear_reading_constrained<- ' 
reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + 1*reading_T2 + 2*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
#fit model
fit_linear_reading_constrained<- growth(linear_reading_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_linear_reading_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model 

basis_reading_constrained <- ' 
reading_basis_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3
'
#fit model
fit_basis_reading_constrained<- growth(basis_reading_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_reading_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_reading_constrained, fit_linear_reading_constrained)


#####Working memory

###Linear model (free error var because of HC) 

#specify model
linear_wm<- ' 
wm_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_slope =~ 0*wm_T1 + 1*wm_T2 + 2*wm_T3 
'


#fit model
fit_linear_wm<- growth(linear_wm, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_linear_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Basis model (free error var)

basis_wm <- ' 
wm_basis_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope =~ 0*wm_T1 + wm_T2 + 1*wm_T3 
'
#fit model
fit_basis_wm<- growth(basis_wm, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_wm, fit_linear_wm)


#####Extracting intercepts and slopes for each cognitive domain

#Picture vocabulary (basis)

predict_picvocab<-data.frame(predict(fit_basis_picvocab_constrained))

#Flanker (basis)

predict_flanker<-data.frame(predict(fit_basis_flanker_constrained))

#Pattern (basis)

predict_pattern<-data.frame(predict(fit_basis_pattern_constrained))

#Picture (basis, unconstrained err var)

predict_picture<-data.frame(predict(fit_basis_picture))

#Reading (basis)

predict_reading<-data.frame(predict(fit_basis_reading_constrained))

#Working memory (basis, unconstrained err var)

predict_wm<-data.frame(predict(fit_basis_wm))


# combining all predict() data frames by ID

# checking that the number of rows match between "predict" and wide df 

dim(uncorrected_wm_visit_wide)

#picture vocabulary 
dim(predict_picvocab)

#flanker 
dim(predict_flanker)

#pattern
dim(predict_pattern)

#picture
dim(predict_picture)

#reading
dim(predict_reading)

#working momory 
dim(predict_wm)

#since the numbers of rows match, extract the ID column from the wide cognitive domain df and assign it to the predict object (as a new column)


#code to add the ID column in a dataframe (since we checked that they have the same number of rows)

predict_picvocab$ID <- uncorrected_wm_visit_wide$ID

predict_flanker$ID <- uncorrected_wm_visit_wide$ID

predict_pattern$ID <- uncorrected_wm_visit_wide$ID

predict_picture$ID <- uncorrected_wm_visit_wide$ID

predict_reading$ID <- uncorrected_wm_visit_wide$ID

predict_wm$ID <- uncorrected_wm_visit_wide$ID


#I think full_join only works with 2 df

#pass all dfs into a list 

list_scores = list(predict_picvocab, predict_flanker, predict_pattern, predict_picture, predict_reading, predict_wm)

# turning into df

predicted_scores_basis_ID<- list_scores%>% reduce(full_join, by='ID')

dim(predicted_scores_basis_ID)

#removing the ID column to correlate

predicted_scores_basis<-subset(predicted_scores_basis_ID, select = -ID)

# slope-slope subset

predicted_slopes_basis<-subset(predicted_scores_basis, select = c(2,4,6,8,10,12))

# int-int subset
predicted_intercepts_basis<-subset(predicted_scores_basis, select=c(1,2,3,4,5,6))

### cor matrix

# int-slope
cor_predict_basis <- cor(predicted_scores_basis, use = 'pairwise.complete.obs')

# slope-slope
cor_slopes_basis<- cor(predicted_slopes_basis, use='pairwise.complete.obs')

# int-slope

cor_intercepts_basis<- cor(predicted_intercepts_basis, use = 'pairwise.complete.obs')


#Plotting intercept and slope correlations 
ggcorrplot(cor_predict_basis,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           lab_size = 2.5,
           ggtheme = theme_minimal())



#Plotting intercept correlations 
ggcorrplot(cor_intercepts_basis,
           hc.order = TRUE,
           type = 'full',
           lab = TRUE,
           lab_size = 2.5,
           ggtheme = theme_minimal())

#Plotting slope correlations 
ggcorrplot(cor_slopes_basis,
           hc.order = TRUE,
           type = 'full',
           lab = TRUE,
           lab_size = 2.5,
           ggtheme = theme_minimal())


#######################

### Extracting intercepts and slopes for linear to test the small slope cor 

#Picture vocabulary (linear)

predict_picvocab_linear<-data.frame(predict(fit_linear_picvocab_constrained))

#testing slope correlations between linear and basis 

slopes_picvocab<- data.frame(predict_picvocab_linear$picvocab_linear_slope, predict_picvocab$picvocab_basis_slope)
cor(slopes_picvocab, use='pairwise.complete.obs')


### Testing for interindividual differences in the slope 
# Comparing models after fixing slope variance
# If the unconstrained variance model is better, the interindividual differences are meaningful


# Picture Vocabulary
# Basis model as it was

fit_basis_picvocab_constrained<- growth(basis_picvocab_constrained, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Basis model with fixed slope variance 

basis_picvocab_fixedvar<- '
picvocab_b_fixedvar_int =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_b_fixedvar_sl =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3

picvocab_b_fixedvar_sl~~0*picvocab_b_fixedvar_sl
'

fit_basis_picvocab_fixedvar<-growth(basis_picvocab_fixedvar, data=uncorrected_wm_visit_wide, missing='fiml')
summary(fit_basis_picvocab_fixedvar, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_picvocab_fixedvar, fit_basis_picvocab_constrained)

#the one with the freely estimated slope variance is significantly better 


#Flanker 
basis_flanker_fv <- ' 
flanker_basis_fv_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_fv_slope =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

flanker_basis_fv_slope~~0*flanker_basis_fv_slope
'
#fit model
fit_basis_flanker_fv<- growth(basis_flanker_fv, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_flanker_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_flanker_constrained, fit_basis_flanker_fv)

#the one with the freely estimated variance is significantly better 


#Pattern
basis_pattern_fv <- ' 
pattern_basis_intercept_fv =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_basis_slope_fv =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 


pattern_T1~~a*pattern_T1
pattern_T2~~a*pattern_T2
pattern_T3~~a*pattern_T3

pattern_basis_slope_fv~~0*pattern_basis_slope_fv
'
#fit model
fit_basis_pattern_fv<- growth(basis_pattern_fv, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_pattern_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


anova(fit_basis_pattern_constrained, fit_basis_pattern_fv)

#the one with the freely estimated variance is significantly better 

# Picture 

basis_picture_fv<- ' 
picture_intercept_fv =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope_fv =~ 0*picture_T1 + picture_T2 + 1*picture_T3 

picture_slope_fv~~0*picture_slope_fv
'
#fit model
fit_basis_picture_fv<- growth(basis_picture_fv, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_picture_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_picture_fv, fit_basis_picture)

# the one with the fixed slope variance is better but NOT signif 


#Reading 

basis_reading_fv <- ' 
reading_basis_intercept_fv =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope_fv =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


reading_T1~~a*reading_T1
reading_T2~~a*reading_T2
reading_T3~~a*reading_T3

reading_basis_slope_fv~~0*reading_basis_slope_fv
'
#fit model
fit_basis_reading_fv<- growth(basis_reading_fv, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_reading_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_reading_fv, fit_basis_reading_constrained)

#the one with the freely estimated slope variance is significantly better 

#Working memory 
basis_wm_fv <- ' 
wm_basis_intercept_fv =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope_fv =~ 0*wm_T1 + wm_T2 + 1*wm_T3 

wm_basis_slope_fv~~0*wm_basis_slope_fv
'

#fit model
fit_basis_wm_fv<- growth(basis_wm_fv, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_basis_wm_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_wm, fit_basis_wm_fv)

#the one with the freely estimated variance is significantly better 

