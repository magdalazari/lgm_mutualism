############## Testing if the big models (4-6 domains) variance covariance estimates can be trusted 
### How? by seeing if the 2 methods correlate highly enough
### We already know that the correlation matrix of the predict estimates from the 4 domain model 
#(where they were estimated all at once) correlates 0.93 with the vcov of lavaan


rm(list=ls())
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")
uncorrected_wm_visit$working_mem<-uncorrected_wm_visit$working_mem*100
uncorrected_wm_visit_wide<-reshape(uncorrected_wm_visit, idvar="ID", timevar="eventname", direction = "wide")
colnames(uncorrected_wm_visit_wide)<-c("ID","visit_T1", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1","visit_T2", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "visit_T3", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")


# 4 domains, not individually estimated (what we tried on Thursday)

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
fit_no_wm_pattern_A<-growth(no_wm_pattern_model_A, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wm_pattern_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

predicted_values<-cor(lavPredict(fit_no_wm_pattern_A), use = 'pairwise.complete.obs')

approach_A_4<-as.vector(predicted_values)[lower.tri(predicted_values)]

approach_B_4<-as.vector(lavInspect(fit_no_wm_pattern_A, what='std')$psi)[lower.tri(lavInspect(fit_no_wm_pattern_A,what='std')$psi)]

cor(approach_A_4, approach_B_4) #0.9362215

### Wwhy not be satisfied with this? (I think) because putting them all together in 1 model is the issue.
#So by getting the estimations isolated and joining them just to compare, we can be a bit more confident about what is happening

###All 6 cognitive domains (giving warnings), all basis, 4/6 fixed error var 

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
fit_full_A<-growth(full_model_A, data=uncorrected_wm_visit_wide,missing='fiml', estimator="MLR")
summary(fit_full_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Fitting basis, fixed error variance individual models 

###Picture vocab

basis_picvocab_fixed<- ' 
picvocab_basis_intercept_f =~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_basis_slope_f =~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

picvocab_T1~~a*picvocab_T1
picvocab_T2~~a*picvocab_T2
picvocab_T3~~a*picvocab_T3
'
fit_basis_picvocab_fixed<- growth(basis_picvocab_fixed, data=uncorrected_wm_visit_wide,missing='fiml', estimator="MLR")
summary(fit_basis_picvocab_fixed, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Flanker 

basis_flanker_fixed <- ' 
flanker_basis_intercept_f =~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_basis_slope_f =~ 0*flanker_T1 + flanker_T2 + 1*flanker_T3 

flanker_T1~~b*flanker_T1
flanker_T2~~b*flanker_T2
flanker_T3~~b*flanker_T3
'
fit_basis_flanker_fixed<- growth(basis_flanker_fixed, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_basis_flanker_fixed, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###Pattern
basis_pattern_fixed <- ' 
pattern_basis_intercept_f =~ 1*pattern_T1 + 1*pattern_T2 + 1*pattern_T3
pattern_basis_slope_f =~ 0*pattern_T1 + pattern_T2 + 1*pattern_T3 


pattern_T1~~c*pattern_T1
pattern_T2~~c*pattern_T2
pattern_T3~~c*pattern_T3
'
fit_basis_pattern_fixed<- growth(basis_pattern_fixed, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_basis_pattern_fixed, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Picture

basis_picture <- ' 
picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope=~ 0*picture_T1 + picture_T2 + 1*picture_T3 
'
fit_basis_picture<- growth(basis_picture, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_basis_picture, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Reading 

basis_reading_fixed <- ' 
reading_basis_intercept_f =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_basis_slope_f =~ 0*reading_T1 + reading_T2 + 1*reading_T3 


reading_T1~~e*reading_T1
reading_T2~~e*reading_T2
reading_T3~~e*reading_T3
'
fit_basis_reading_fixed<- growth(basis_reading_fixed, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_basis_reading_fixed, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

###Working memory 

basis_wm<- ' 
wm_basis_intercept =~ 1*wm_T1 + 1*wm_T2 + 1*wm_T3
wm_basis_slope=~ 0*wm_T1 + wm_T2 + 1*wm_T3 
'
fit_basis_wm<- growth(basis_wm, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_basis_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


###################

#Making a predict for each domain

#Picture vocabulary 

predict_picvocab<-data.frame(predict(fit_basis_picvocab_fixed))

#Flanker 

predict_flanker<-data.frame(predict(fit_basis_flanker_fixed))

#Pattern 

predict_pattern<-data.frame(predict(fit_basis_pattern_fixed))

#Picture 

predict_picture<-data.frame(predict(fit_basis_picture))

#Reading 

predict_reading<-data.frame(predict(fit_basis_reading_fixed))

#Working memory 

predict_wm<-data.frame(predict(fit_basis_wm))

###################################################
#adding ID to join them

predict_picvocab$ID <-uncorrected_wm_visit_wide$ID

predict_flanker$ID <- uncorrected_wm_visit_wide$ID

predict_pattern$ID <- uncorrected_wm_visit_wide$ID

predict_picture$ID <- uncorrected_wm_visit_wide$ID

predict_reading$ID <- uncorrected_wm_visit_wide$ID

predict_wm$ID <- uncorrected_wm_visit_wide$ID

list_scores = list(predict_picvocab, predict_flanker, predict_pattern, predict_picture, predict_reading, predict_wm)

predicted_scores_ID<- list_scores%>% reduce(full_join, by='ID')

predicted_scores<-subset(predicted_scores_ID, select = -ID)

predicted_scores_cor<-cor(predicted_scores, use='pairwise.complete.obs')


#row and column proper order for correlations of predict

predicted_scores_cor<-predicted_scores_cor[c("flanker_basis_intercept_f", "flanker_basis_slope_f", "wm_basis_intercept", "wm_basis_slope",
                                     "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                     "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept_f", "pattern_basis_slope_f"),
                                   c("flanker_basis_intercept_f", "flanker_basis_slope_f", "wm_basis_intercept", "wm_basis_slope",
                                       "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                       "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept_f", "pattern_basis_slope_f" )]


full_model_matrix<-lavInspect(fit_full_A, what='std')$psi


#this is the information we want, I must summarize the output
cor(predicted_scores_cor, full_model_matrix)

approach_A<-as.vector(predicted_scores_cor)[as.vector(lower.tri(predicted_scores_cor))]
approach_B<-as.vector(lavInspect(fit_full_A, what='std')$psi)[lower.tri(lavInspect(fit_full_A,what='std')$psi)]

#It is 0.7491272 (approach A correlations are much stronger)
cor(approach_A, approach_B)
plot(approach_A, approach_B)


#how much do all of the parameters correlate with each other between 2 different estimations 
difference1<-predicted_scores_cor-full_model_matrix
correlation<-data.frame(cor(predicted_scores_cor, full_model_matrix))

#how much do slopes correlate between the 2 estimation methods?(looking at the diagonal)

correlation_slopes<-correlation[c(2,4,6,8,10,12), c(2,4,6,8,10,12)]
diag(as.matrix(correlation_slopes))

#How much do intercepts correlate between the 2 estimation methods? (looking at the diagonal)

correlation_intercepts<-correlation[c(1,3,5,7,9,11), c(1,3,5,7,9,11)]
diag(as.matrix(correlation_intercepts))

correlation_intercepts_slopes<-correlation[c(1,3,5,7,9,11), c(2,4,6,8,10,12)]
diag(as.matrix(correlation_intercepts_slopes))

correlation_intercepts_slopes2<-correlation[c(2,4,6,8,10,12), c(1,3,5,7,9,11)]
diag(as.matrix(correlation_intercepts_slopes2))

######### Doing the same but excluding working memory to check improvement 
list_scores_nowm = list(predict_picvocab, predict_flanker, predict_pattern, predict_picture, predict_reading)
predicted_scores_nowm_ID<- list_scores_nowm%>% reduce(full_join, by='ID')
predicted_scores_nowm<-subset(predicted_scores_nowm_ID, select = -ID)
predicted_scores_nowm_cor<-cor(predicted_scores_nowm, use='pairwise.complete.obs')


#row and column order for correlations of predict

predicted_scores_nowm_cor<-predicted_scores_nowm_cor[c("flanker_basis_intercept_f", "flanker_basis_slope_f",
                                             "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                             "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept_f", "pattern_basis_slope_f"),
                                           c("flanker_basis_intercept_f", "flanker_basis_slope_f",
                                             "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                             "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept_f", "pattern_basis_slope_f" )]
#model without wm
no_wm_model<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 +picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3

picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope=~ 0*picture_T1 + picture_T2 + 1*picture_T3 

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
fit_no_wm<-growth(no_wm_model, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_no_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

approach_A2<-as.vector(predicted_scores_nowm_cor)[as.vector(lower.tri(predicted_scores_nowm_cor))]
approach_B2<-as.vector(lavInspect(fit_no_wm, what='std')$psi)[lower.tri(lavInspect(fit_no_wm,what='std')$psi)]

#It is 0.6556383 
cor(approach_A2, approach_B2)

######### Doing the same but excluding working memory AND pattern to check improvement 

list_scores_nowmpat = list(predict_picvocab, predict_flanker, predict_picture, predict_reading)
predicted_scores_nowmpat_ID<- list_scores_nowmpat%>% reduce(full_join, by='ID')
predicted_scores_nowmpat<-subset(predicted_scores_nowmpat_ID, select = -ID)
predicted_scores_nowmpat_cor<-cor(predicted_scores_nowmpat, use='pairwise.complete.obs')


#row and column order for correlations of predict

predicted_scores_nowmpat_cor<-predicted_scores_nowmpat_cor[c("flanker_basis_intercept_f", "flanker_basis_slope_f",
                                                       "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                                       "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope"),
                                                     c("flanker_basis_intercept_f", "flanker_basis_slope_f",
                                                       "picvocab_basis_intercept_f", "picvocab_basis_slope_f", "reading_basis_intercept_f",
                                                       "reading_basis_slope_f","picture_basis_intercept", "picture_basis_slope" )]
#model without wm and pattern
no_wmpat_model<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

picvocab_intercept=~ 1*picvocab_T1 + 1*picvocab_T2 + 1*picvocab_T3
picvocab_slope=~ 0*picvocab_T1 + picvocab_T2 + 1*picvocab_T3 

reading_intercept =~ 1*reading_T1 + 1*reading_T2 + 1*reading_T3
reading_slope =~ 0*reading_T1 + reading_T2 + 1*reading_T3

picture_basis_intercept =~ 1*picture_T1 + 1*picture_T2 + 1*picture_T3
picture_basis_slope=~ 0*picture_T1 + picture_T2 + 1*picture_T3 

flanker_T1~~a*flanker_T1
flanker_T2~~a*flanker_T2
flanker_T3~~a*flanker_T3

picvocab_T1~~c*picvocab_T1
picvocab_T2~~c*picvocab_T2
picvocab_T3~~c*picvocab_T3

reading_T1~~d*reading_T1
reading_T2~~d*reading_T2
reading_T3~~d*reading_T3'

#fit model !Because picture error var is not fixed this gives an error! I cant estimate picvocab slope at all 
fit_no_wmpat<-growth(no_wmpat_model, data=uncorrected_wm_visit_wide,missing='fiml',estimator="MLR")
summary(fit_no_wmpat, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

approach_A3<-as.vector(predicted_scores_nowmpat_cor)[as.vector(lower.tri(predicted_scores_nowmpat_cor))]
approach_B3<-as.vector(lavInspect(fit_no_wmpat, what='std')$psi)[lower.tri(lavInspect(fit_no_wmpat,what='std')$psi)]

#It is 0.505397 
cor(approach_A3, approach_B3)

