#reading data from cleaning script 
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")


#loading packages
library(lavaan)
library(ggplot2)
library(ggcorrplot)
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


#fit model
fit_basis_picvocab_constrained<- growth(basis_picvocab_constrained, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_basis_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#comparing model fit for the 2 models after constraining error variance
#(so that it wont be estimated uniquely -automatically- by growth function, we gain 2 df for each model)
anova(fit_basis_picvocab_constrained,fit_linear_picvocab_constrained)



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

picture_T1~~a1*picture_T1
picture_T2~~a2*picture_T2
picture_T3~~a3*picture_T3
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
reading_slope =~ 0*reading_T1 + 1*reading_T2 + 2*reading_T3 


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

#multiplying so that absolute values are not as small 
wm_multiplied<-mutate_if(wm_wide, is.numeric, ~ . * 100)

###Linear model (free error var because of HC) 

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


#####Extracting intercepts and slopes for each cognitive domain

#Picture vocabulary (basis)

predict_picvocab<-data.frame(predict(fit_basis_picvocab))

#Flanker (basis)

predict_flanker<-data.frame(predict(fit_basis_flanker))

#Pattern (basis)

predict_pattern<-data.frame(predict(fit_basis_pattern))

#Picture (linear)

predict_picture<-data.frame(predict(fit_linear_picture))

#Reading (basis)

predict_reading<-data.frame(predict(fit_basis_reading))

#Working memory (basis, unconstrained err var)

predict_wm<-data.frame(predict(fit_basis_wm))


#combining all predict() data frames

###nicks suggestion in order to join all predict columns based on ID 

#check that the number of rows match between the predict and each cognitive domains wide df 

dim() #to retrieve dimensions of an object 

#picture vocabulary 
dim(picvocab_uncorrected_wide)
dim(predict_picvocab)

#flanker 
dim(flanker_uncorrected_wide)
dim(predict_flanker)

#pattern
dim(pattern_uncorrected_wide)
dim(predict_pattern)

#picture
dim(picture_uncorrected_wide)
dim(predict_picture)

#reading
dim(reading_uncorrected_wide)
dim(predict_reading)

#working momory 
dim(wm_multiplied)
dim(predict_wm)

#since the numbers of rows match, extract the ID column from the wide cognitive domain df and assign it to the predict object (as a new column)


#code to add the ID column in a dataframe (since we checked that they have the same number of rows)

predict_picvocab$ID <- picvocab_uncorrected_wide$ID

predict_flanker$ID <- flanker_uncorrected_wide$ID

predict_pattern$ID <- pattern_uncorrected_wide$ID

predict_picture$ID <- picture_uncorrected_wide$ID

predict_reading$ID <- reading_uncorrected_wide$ID

predict_wm$ID <- wm_multiplied$ID


#I think full_join only works with 2 df
#using reduce instead 

#pass all dfs into a list 

list_scores = list(predict_picvocab, predict_flanker, predict_pattern, predict_picture, predict_reading, predict_wm)

#put the list in the reduce function 
predicted_scores_ID<- list_scores%>% reduce(full_join, by='ID')



#for some reason predicted_scores has more rows than each predict.
#Maybe some IDs did not do the cognitive tests but did the WM one? so the WM has both less and unique people 

dim(predicted_scores_ID)

#removing the ID column to run correlation  
predicted_scores<-subset(predicted_scores_ID, select = -ID)

#maybe make one for slopes, one for intercepts 
predicted_intercepts<-subset(predicted_scores, select=c(1,2,3,4,5,6))

predicted_slopes<-subset(predicted_scores, select = c(2,4,6,8,10,12))

predicted_picvocab_reading<-subset(predicted_scores, select=c(1,2,9,10))

#reordering columns 
predicted_scores<-predicted_scores[,c(1,3,5,7,9,11,2,4,6,8,10)]

#cor matrix
cor_predict <- cor(predicted_scores, use = 'pairwise.complete.obs')
  
cor_slopes<- cor(predicted_slopes, use='pairwise.complete.obs')

cor_intercepts<- cor(predicted_intercepts, use = 'pairwise.complete.obs')

cor_picvocab_reading<- cor(predicted_picvocab_reading, use='pairwise.complete.obs' )

#Plotting intercept and slope correlations 
ggcorrplot(cor_predict,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())


#reordering columns 
cor_predict<- cor_predict[ , c(1,3,5,7,9,2,4,6,8,10)]

#Plotting intercept correlations 
ggcorrplot(cor_intercepts,
           hc.order = TRUE,
           type = 'full',
           lab = TRUE,
           ggtheme = theme_minimal())

#Plotting slope correlations 
ggcorrplot(cor_slopes,
           hc.order = TRUE,
           type = 'full',
           lab = TRUE,
           ggtheme = theme_minimal())


#plotting picvocab and reading 
ggcorrplot(cor_picvocab_reading,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())


#######################

###
#####Extracting intercepts and slopes for linear to test the sad slopes 

#Picture vocabulary (linear)

predict_picvocab_linear<-data.frame(predict(fit_linear_picvocab))

#testing slope correlations between linear and basis 
slopes_picvocab<- data.frame(predict_picvocab_linear$picvocab_linear_slope, predict_picvocab$picvocab_basis_slope)
cor(slopes_picvocab, use='pairwise.complete.obs')


###Testing for interindividual differences in the slope 
#If the unconstrained variance model is better, the interindividual differences are meaningful


#Picture Vocabulary
#Basis model as it was
#fit model
fit_basis_picvocab_constrained<- growth(basis_picvocab_constrained, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_basis_picvocab_constrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Basis model with fixed variance 

basis_picvocab_fixedvar<- '
picvocab_b_fixedvar_int =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_b_fixedvar_sl =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3

picvocab_b_fixedvar_sl~~0*picvocab_b_fixedvar_sl
'
fit_basis_picvocab_fixedvar<-growth(basis_picvocab_fixedvar, data=picvocab_uncorrected_wide, missing='fiml')
summary(fit_basis_picvocab_fixedvar, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#Comparison between the 2 
anova(fit_basis_picvocab_fixedvar, fit_basis_picvocab_constrained)

#the one with the freely estimated variance is significantly better 


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
fit_basis_flanker_fv<- growth(basis_flanker_fv, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_basis_flanker_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_flanker, fit_basis_flanker_fv)

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
fit_basis_pattern_fv<- growth(basis_pattern_fv, data=pattern_uncorrected_wide,missing='fiml')
summary(fit_basis_pattern_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


anova(fit_basis_pattern, fit_basis_pattern_fv)

#the one with the freely estimated variance is significantly better 

#Picture 

linear_picture_fv<- ' 
picture_intercept_fv =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_slope_fv =~ 0*picture_T1 + 1*picture_T2 + 2*picture_T3 


picture_T1~~a*picture_T1
picture_T2~~a*picture_T2
picture_T3~~a*picture_T3

picture_slope_fv~~0*picture_slope_fv
'
#fit model
fit_linear_picture_fv<- growth(linear_picture_fv, data=picture_uncorrected_wide,missing='fiml')
summary(fit_linear_picture_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_linear_picture_fv, fit_linear_picture)

#the one with the freely estimated variance is significantly better 


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
fit_basis_reading_fv<- growth(basis_reading_fv, data=reading_uncorrected_wide,missing='fiml')
summary(fit_basis_reading_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_reading_fv, fit_basis_reading)

#the one with the freely estimated variance is significantly better 


#Working memory 
basis_wm_fv <- ' 
wm_basis_intercept_fv =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope_fv =~ 0*wm_T1 + wm_T2 + 1*wm_T3 

wm_basis_slope_fv~~0*wm_basis_slope_fv
'

#fit model
fit_basis_wm_fv<- growth(basis_wm_fv, data=wm_multiplied,missing='fiml')
summary(fit_basis_wm_fv, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

anova(fit_basis_wm, fit_basis_wm_fv)

#the one with the freely estimated variance is significantly better 


#####################################
####Try to compare basis models without the error variance constrained to see if something changes?

#specify model 
basis_picvocab_unconstrained <- ' 
picvocab_basis_intercept_c =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope_c =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3'


#fit model
fit_basis_picvocab_unconstrained<- growth(basis_picvocab_unconstrained, data=picvocab_uncorrected_wide,missing='fiml')
summary(fit_basis_picvocab_unconstrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#much less variance in the slope when variance is unconstrained 


basis_flanker_unconstrained <- ' 
flanker_basis_intercept =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_slope =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 
'
#fit model
fit_basis_flanker_unconstrained<- growth(basis_flanker_unconstrained, data=flanker_uncorrected_wide,missing='fiml')
summary(fit_basis_flanker_unconstrained, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

predict_flanker_unconstrained<-data.frame(predict(fit_basis_flanker_unconstrained))
predict_picvocab_unconstrained<-data.frame(predict(fit_basis_picvocab_unconstrained))

predicted_unconstrained<- data.frame(predict_flanker_unconstrained, predict_picvocab_unconstrained)

cor_unconstrained<- cor(predicted_unconstrained, use='pairwise.complete.obs')

ggcorrplot(cor_unconstrained,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           lab_size = 2.3,
           ggtheme = theme_minimal())

#no difference, no point 
#############################