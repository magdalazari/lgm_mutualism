
source("Data_cleaning.R")
source("basis_vs_linear.R")

### Trying to include all cognitive domains in 1 large model 
# and trying to find in what stage the negative v-cov matrix warning occurs 

#Model A: 1. int+sl, 2. fixed error var 

#Model D: 1. int+sl, 2. freed error variances 


###All 6 cognitive domains, all basis, 4/6 fixed error var 
# not positive definite v-cov matrix warning

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

### Removed picture
# not positive definite v-cov warning 

no_picture_A<- '
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
fit_nopicture_A<-growth(no_picture_A, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_nopicture_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


### Removed working memory and picture
# Different error: some estimated lv variances are negative. 

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
fit_no_wm_picture_A<-growth(no_wm_picture_model_A, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wm_picture_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

### Removed working memory and pattern 
# No warnings

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


# correlating predicted scores

predicted_no_wm_pattern<-cor(lavPredict(fit_no_wm_pattern_A), use = 'pairwise.complete.obs')

# plotting correlations in model with 4 domains and fixed error var 
ggcorrplot(predicted_no_wm_pattern,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           ggtheme = theme_minimal())


### Freeing err var: slope correlations higher with freed error var but non positive cov matrix 

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
fit_no_wm_pattern_D<-growth(no_wm_pattern_model_D, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wm_pattern_D, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

predicted_scores_D<-lavPredict(fit_no_wm_pattern_D)

predicted_cor_D<-cor(predicted_scores_D, use = 'pairwise.complete.obs')

# plotting correlations in model with 4 domains and free error var 

ggcorrplot(predicted_cor_D,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           ggtheme = theme_minimal())

########### Comparing estimations from predict() to estimations from lavaan

# extracting predict() estimations from big model (not individual domains)

### 6 domain model: 0.69, very low 

predict_full_6<-cor(lavPredict(fit_full_A), use = 'pairwise.complete.obs')

approach_A_full_6<-as.vector(predict_full_6)[lower.tri(predict_full_6)]

approach_B_full_6<-as.vector(lavInspect(fit_full_A, what='std')$psi)[lower.tri(lavInspect(fit_full_A,what='std')$psi)]

cor(approach_A_full_6, approach_B_full_6)

# 4 domain model, not individually estimated, 0.93

predicted_no_wm_pattern<-cor(lavPredict(fit_no_wm_pattern_A), use = 'pairwise.complete.obs')

approach_A_full_4<-as.vector(predicted_no_wm_pattern)[lower.tri(predicted_no_wm_pattern)]

approach_B_full_4<-as.vector(lavInspect(fit_no_wm_pattern_A, what='std')$psi)[lower.tri(lavInspect(fit_no_wm_pattern_A,what='std')$psi)]

cor(approach_A_full_4, approach_B_full_4) 


### We want to compare to the predict estimations for each domain (not from a whole model), so using df with joined predicts 

### All 6 cognitive domains, basis, 4/6 fixed error var 

# row and column correct order for correlations of predict

# the df we made for all the single model predicts
cor_predict_basis <- cor(predicted_scores_basis, use = 'pairwise.complete.obs')

cor_predict_basis_reordered<-cor_predict_basis[c("flanker_basis_intercept","flanker_basis_slope", "wm_basis_intercept", "wm_basis_slope",
                                                 "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                 "reading_basis_slope","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept", "pattern_basis_slope"),
                                               c("flanker_basis_intercept", "flanker_basis_slope", "wm_basis_intercept", "wm_basis_slope",
                                                 "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                 "reading_basis_slope","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept", "pattern_basis_slope")]

approach_A_6<-as.vector(cor_predict_basis_reordered)[as.vector(lower.tri(cor_predict_basis_reordered))]
approach_B_6<-as.vector(lavInspect(fit_full_A, what='std')$psi)[lower.tri(lavInspect(fit_full_A,what='std')$psi)]

#It is 0.57 
cor(approach_A_6, approach_B_6)
plot(approach_A_6, approach_B_6)

full_model_matrix<-lavInspect(fit_full_A, what='std')$psi

# how much do the parameters correlate with each other between 2 different estimations 

correlation_full_model<-data.frame(cor(cor_predict_basis_reordered, full_model_matrix))

estim_difference<-cor_predict_basis_reordered-full_model_matrix

#how much do slopes correlate between the 2 estimation methods?(looking at the diagonal)

correlation_full_model_slopes<-correlation_full_model[c(2,4,6,8,10,12), c(2,4,6,8,10,12)]
diag(as.matrix(correlation_full_model_slopes))

#How much do intercepts correlate between the 2 estimation methods? (looking at the diagonal)

correlation_full_model_intercepts<-correlation_full_model[c(1,3,5,7,9,11), c(1,3,5,7,9,11)]
diag(as.matrix(correlation_full_model_intercepts))

# how much do parameters correlate within each domain

# combination 1 
correlation_full_model_intercepts_slopes<-correlation_full_model[c(1,3,5,7,9,11), c(2,4,6,8,10,12)]
diag(as.matrix(correlation_full_model_intercepts_slopes))

# combination 2 
correlation_full_model_intercepts_slopes2<-correlation_full_model[c(2,4,6,8,10,12), c(1,3,5,7,9,11)]
diag(as.matrix(correlation_full_model_intercepts_slopes2))


######### Doing the same but excluding working memory to check improvement

#excluding wm from individual predict df 
list_scores_nowm = list(predict_picvocab, predict_flanker, predict_pattern, predict_picture, predict_reading)

predicted_scores_nowm_ID<- list_scores_nowm%>% reduce(full_join, by='ID')
predicted_scores_nowm<-subset(predicted_scores_nowm_ID, select = -ID)
cor_predict_basis_nowm<-cor(predicted_scores_nowm, use='pairwise.complete.obs')

#row and column order for correlations of predict

cor_predict_basis_nowm_reordered<-cor_predict_basis_nowm[c("flanker_basis_intercept", "flanker_basis_slope",
                                                       "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                       "reading_basis_slope","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept", "pattern_basis_slope"),
                                                     c("flanker_basis_intercept", "flanker_basis_slope",
                                                       "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                       "reading_basis_slope","picture_basis_intercept", "picture_basis_slope", "pattern_basis_intercept", "pattern_basis_slope" )]

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
fit_no_wm<-growth(no_wm_model, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

approach_A_5<-as.vector(cor_predict_basis_nowm_reordered)[as.vector(lower.tri(cor_predict_basis_nowm_reordered))]
approach_B_5<-as.vector(lavInspect(fit_no_wm, what='std')$psi)[lower.tri(lavInspect(fit_no_wm,what='std')$psi)]

#It is 0.65 (improved)
cor(approach_A_5, approach_B_5)

### Repeating after excluding working memory and pattern  

list_scores_nowmpat = list(predict_picvocab, predict_flanker, predict_picture, predict_reading)
predicted_scores_nowmpat_ID<- list_scores_nowmpat%>% reduce(full_join, by='ID')
predicted_scores_nowmpat<-subset(predicted_scores_nowmpat_ID, select = -ID)
cor_predict_basis_nowmpat<-cor(predicted_scores_nowmpat, use='pairwise.complete.obs')


#row and column order for correlations of predict

cor_predict_basis_nowmpat_reordered<-cor_predict_basis_nowmpat[c("flanker_basis_intercept", "flanker_basis_slope",
                                                             "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                             "reading_basis_slope","picture_basis_intercept", "picture_basis_slope"),
                                                           c("flanker_basis_intercept", "flanker_basis_slope",
                                                             "picvocab_basis_intercept_c", "picvocab_basis_slope_c", "reading_basis_intercept",
                                                             "reading_basis_slope","picture_basis_intercept", "picture_basis_slope" )]
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
fit_no_wmpat<-growth(no_wmpat_model, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wmpat, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

approach_A4<-as.vector(cor_predict_basis_nowmpat_reordered)[as.vector(lower.tri(cor_predict_basis_nowmpat_reordered))]
approach_B4<-as.vector(lavInspect(fit_no_wmpat, what='std')$psi)[lower.tri(lavInspect(fit_no_wmpat,what='std')$psi)]

#It is 0.5 (decreased)
cor(approach_A4, approach_B4)


##### Comparing Standard SEM approach estimates to SAM approach estimates 


### All 6 cognitive domains, basis, 4/6 fixed error var

# Fit model using standard SEM
fit_full_A<-growth(full_model_A, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_full_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# Fit model using SAM
fit_full_A_sam<-sam(full_model_A, data=uncorrected_wm_visit_wide, missing='fiml',cmd='growth')
summary(fit_full_A_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


### Model without WM, basis, 4/5 fixed error var

# fit model using standard SEM 
fit_no_wm<-growth(no_wm_model, data=uncorrected_wm_visit_wide,missing='fiml')
summary(fit_no_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


# fit model using SAM
fit_no_wm_sam<-sam(no_wm_model, data=uncorrected_wm_visit_wide, missing='fiml',cmd='growth')
summary(fit_no_wm_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

# standard model estimations look normal but still give negative v-cov error 
# For now choosing this model, it correlated the highest between predict and lavaan 


######## SAM 

# getting parameter estimates from SAM model

df_sam <- summary(fit_no_wm_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)$pe

# renaming std.all, keeping relevant col

colnames(df_sam)[11] <- "SAM_cor"
df_sam <- df_sam[, c("lhs","op","rhs","SAM_cor")]

# keeping only the correlations

df_sam <- df_sam[df_sam$op == "~~",] 

# removing unnecessary var

df_sam <- df_sam[!df_sam$lhs == df_sam$rhs,] 

# adding logical columns to make a group col

df_sam$intercept_dummy <- (str_detect(df_sam$lhs, "intercept") + str_detect(df_sam$rhs, "intercept")) ==2
df_sam$slope_dummy <- (str_detect(df_sam$lhs, "slope") + str_detect(df_sam$rhs, "slope")) ==2
df_sam$within_task <- str_sub(df_sam$lhs, end = 7) == str_sub(df_sam$rhs, end = 7)

# making a column describing which relationship it is
# empty for now 

df_sam$cor_group <- rep(NA, dim(df_sam)[1])

# if intercept_dummy = 1(true), row gets named "int_int" etc 

df_sam$cor_group[df_sam$intercept_dummy == 1] <- rep("int_int", length(df_sam$cor_group[df_sam$intercept_dummy == 1]))
df_sam$cor_group[df_sam$slope_dummy == 1] <- rep("slope_slope", length(df_sam$cor_group[df_sam$slope_dummy == 1]))
df_sam$cor_group[is.na(df_sam$cor_group)] <- rep("int_slope", length(df_sam$cor_group[is.na(df_sam$cor_group)]))

# getting within domain int~~slope using within_task dummy & int_slope

df_sam$cor_group[df_sam$within_task == 1] <- rep("within", length(df_sam$cor_group[df_sam$within_task==1]))

# removing unnecessary

df_sam<-df_sam[, c("lhs", "rhs","cor_group", "SAM_cor")]

### Plotting all SAM estimations

# Density plot

ggplot(df_sam, aes(SAM_cor, fill = cor_group)) +
  geom_density(alpha = .7) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Raincloud plots 

ggplot(df_sam, aes(cor_group, SAM_cor, fill = cor_group)) +
  ggrain::geom_rain() +
  theme_minimal(base_size = 14)


ggplot(df_sam, aes(1, SAM_cor, fill = cor_group, color = cor_group)) +
  geom_rain(alpha = .5, 
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list( #nudging boxplots so that they dont overlap
              position = ggpp::position_dodgenudge(x = .1, width = 0.1), width = 0.1 
            )) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2')+
  coord_flip()

# Descriptives for all SAM estimations 

psych::describe.by(df_sam$SAM_cor, df_sam$cor_group)


######## Normal

# parameter estimates for normally estimated  model

df_nowm_normal <- summary(fit_no_wm, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)$pe

# renaming std.all 

colnames(df_nowm_normal)[11] <- "Normal_cor"
df_nowm_normal <- df_nowm_normal[, c("lhs","op","rhs","Normal_cor")]

# keeping only the correlations
df_nowm_normal <- df_nowm_normal[df_nowm_normal$op == "~~",] 

# removing unnecessary var
df_nowm_normal <- df_nowm_normal[!df_nowm_normal$lhs == df_nowm_normal$rhs,] 

# logical columns to make a group col

df_nowm_normal$intercept_dummy <- (str_detect(df_nowm_normal$lhs, "intercept") + str_detect(df_nowm_normal$rhs, "intercept")) ==2
df_nowm_normal$slope_dummy <- (str_detect(df_nowm_normal$lhs, "slope") + str_detect(df_nowm_normal$rhs, "slope")) ==2
df_nowm_normal$within_task <- str_sub(df_nowm_normal$lhs, end = 7) == str_sub(df_nowm_normal$rhs, end = 7)

# empty column on which relationship it is

df_nowm_normal$cor_group <- rep(NA, dim(df_nowm_normal)[1])

# if intercept_dummy = 1(true), row gets named "int_int"

df_nowm_normal$cor_group[df_nowm_normal$intercept_dummy == 1] <- rep("int_int", length(df_nowm_normal$cor_group[df_nowm_normal$intercept_dummy == 1]))
df_nowm_normal$cor_group[df_nowm_normal$slope_dummy == 1] <- rep("slope_slope", length(df_nowm_normal$cor_group[df_nowm_normal$slope_dummy == 1]))
df_nowm_normal$cor_group[is.na(df_nowm_normal$cor_group)] <- rep("int_slope", length(df_nowm_normal$cor_group[is.na(df_nowm_normal$cor_group)]))

# getting within domain int~~slope using within_task dummy & int_slope

df_nowm_normal$cor_group[df_nowm_normal$within_task == 1] <- rep("within", length(df_nowm_normal$cor_group[df_nowm_normal$within_task==1]))
df_nowm_normal<-df_nowm_normal[, c("lhs", "rhs","cor_group", "Normal_cor")]

############# Checking for sign flipping between SAM and Standard estimations

# I'd get T for sign flips

(df_nowm_normal$Normal_cor < 0) + (df_sam$SAM_cor < 0) == 1 

absolute_difference<- abs(df_nowm_normal$Normal_cor-df_sam$SAM_cor)
mean(absolute_difference)
sd(absolute_difference)


###### Plotting all normal estimations

# Density plot

ggplot(df_nowm_normal, aes(Normal_cor, fill = cor_group)) +
  geom_density(alpha = .7) +
  theme_minimal(base_size = 13)

# Raincloud plot 

ggplot(df_nowm_normal, aes(cor_group, Normal_cor, fill = cor_group)) +
  ggrain::geom_rain() +
  theme_minimal(base_size = 14)


# Descriptives 

psych::describe.by(df_nowm_normal$Normal_cor, df_nowm_normal$cor_group)

###################################################################

### Directly comparing estimations from standard and SAM approaches

# data frame with both types of estimations 

both_methods<-bind_cols(df_nowm_normal, df_sam$SAM_cor)
colnames(both_methods)<-c("lhs", "rhs", "cor_group", "Normal_cor", "SAM_cor")

# this gives us the relationship

summary(lm(Normal_cor ~ SAM_cor, data = both_methods))

# standardizing the variables to get the correlation

summary(lm(as.numeric(scale(Normal_cor)) ~ as.numeric(scale(SAM_cor)), data = both_methods))
cor(both_methods$Normal_cor, both_methods$SAM_cor)

# same values

# now we can get how much each point deviates from the line using the residuals

residuals <- as.numeric(summary(lm(Normal_cor ~ SAM_cor, data = both_methods))$residuals)

max(residuals)

both_methods$residuals <- residuals

both_methods[order(both_methods$residuals),] # negative error
both_methods[order(both_methods$residuals, decreasing = T),] # positive error

# looking at case 2 in the decreasing table above shows a bit of an issue
# we made the SAM vector have a mean shift of 1, therefor even that the estiamtions are almost the same
# 6.5 normal an 6.6 in SAM it has a HIGH residual; since the line is fitting to everyone with a mean shift
# this is not bad, nor good but shows the different info you get by looking at residuals

## Plotting both methods

# linear slope

plot<-ggplot(both_methods, aes(x = Normal_cor, y = SAM_cor, color =cor_group,)) +
  geom_point() +
  geom_abline(intercept=0, slope=1)
plot+labs(x="Standard Correlations", y="SAM Correlations")

# plot with a line for each type of param (extra)
ggplot(both_methods, aes(x = Normal_cor, y = SAM_cor, color = cor_group)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(method = "lm", se = FALSE)

### Comparing just the intercepts between the 2 methods  

# subsetting intercept-intercept

intercepts2<-both_methods[(both_methods$cor_group == "int_int"),]

summary(lm(Normal_cor ~ SAM_cor, data = intercepts2))

# standardizing the variables to get the correlation

summary(lm(as.numeric(scale(Normal_cor)) ~ as.numeric(scale(SAM_cor)), data = intercepts2))
cor(intercepts2$Normal_cor, intercepts2$SAM_cor)

# how much each point deviates from the line using the residuals

residuals <- as.numeric(summary(lm(Normal_cor ~ SAM_cor, data = intercepts2))$residuals)
max(residuals)
intercepts2$residuals <- residuals

intercepts2[order(intercepts2$residuals),] # negative error
intercepts2[order(intercepts2$residuals, decreasing = T),] # positive error

# Int-int plot, 2 methods 
plot_intercepts<-ggplot(intercepts2, aes(x = Normal_cor, y = SAM_cor, color =cor_group)) +
  geom_point(color="pink") +
  geom_abline(intercept=0, slope=1) 
plot_intercepts+labs(x="Standard Correlations", y="SAM Correlations")+theme_bw(base_size = 20)

############################
# Converting to long to plot 
long_intercepts <- intercepts2%>% 
  pivot_longer(
    cols = "Normal_cor":"SAM_cor", 
    names_to = "estimation_method",
    values_to = "value")


ggplot(long_intercepts, aes(1, value, fill = estimation_method, )) +
  geom_rain(alpha = .5, 
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list( #nudging boxplots so that they dont overlap
              position = ggpp::position_dodgenudge(x = .1, width = 0.1), width = 0.1 
            )) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2')+
  coord_flip()

ggplot(long_intercepts, aes(x = value, group = estimation_method, 
                            fill = estimation_method, xlabel= "Correlation", color = estimation_method)) + 
  geom_density(alpha = 0.3)
############################

### Comparing the slopes between the 2 methods  

# subsetting slope-slope

slopes2<-both_methods[(both_methods$cor_group == "slope_slope"),]

summary(lm(Normal_cor ~ SAM_cor, data = slopes2))

summary(lm(as.numeric(scale(Normal_cor)) ~ as.numeric(scale(SAM_cor)), data = slopes2))
cor(slopes2$Normal_cor, slopes2$SAM_cor)

# how much each point deviates from the line

residuals <- as.numeric(summary(lm(Normal_cor ~ SAM_cor, data = slopes2))$residuals)
max(residuals)
slopes2$residuals <- residuals
slopes2[order(slopes2$residuals),] # negative error
slopes2[order(slopes2$residuals, decreasing = T),] # positive error

# linear slope

plot_slopes<-ggplot(slopes2, aes(x = Normal_cor, y = SAM_cor)) +
  geom_point(color="darkcyan") +
  geom_abline(intercept=0, slope=1) 
plot_slopes+labs(x="Standard Correlations", y="SAM Correlations")+theme_bw(base_size=20)

### Comparing intercept-slope between the 2 methods  

# subsetting intercept-slope between domains 

int_slope2<-both_methods[(both_methods$cor_group == "int_slope"),]

summary(lm(Normal_cor ~ SAM_cor, data = int_slope2))

summary(lm(as.numeric(scale(Normal_cor)) ~ as.numeric(scale(SAM_cor)), data = int_slope2))
cor(int_slope2$Normal_cor, int_slope2$SAM_cor)

# now we can get how much each point deviates from the line using the residuals

residuals <- as.numeric(summary(lm(Normal_cor ~ SAM_cor, data = int_slope2))$residuals)
max(residuals)
int_slope2$residuals <- residuals

int_slope2[order(int_slope2$residuals),] # negative error
int_slope2[order(int_slope2$residuals, decreasing = T),] # positive error

# linear slope

plot<-ggplot(int_slope2, aes(x = Normal_cor, y = SAM_cor)) +
  geom_point(color="forestgreen") +
  geom_abline(intercept=0, slope=1) 
plot+labs(x="Standard Correlations", y="SAM Correlations")+theme_bw(base_size = 20)

### Correlation matrices for SAM estimations 

### Slope - slope 

cor_values_slopes<-c(0.02570020, 0.10277584, 0.04448163, 0.36005104, 0.93488578, 0.13340386,0.03601241, 0.21768095, 0.03030064, 0.04729478)

# creating a matrix
cor_matrix_slope <- matrix(NA, nrow = 5, ncol = 5)

cor_matrix_slope[lower.tri(cor_matrix_slope)] <- cor_values_slopes

# transposing/flipping matrix 
cor_matrix_slope[upper.tri(cor_matrix_slope)] <- t(cor_matrix_slope)[upper.tri(cor_matrix_slope)]

colnames(cor_matrix_slope)<-c("Flanker", "Picvocab","Reading","Picture","Pattern")
row.names(cor_matrix_slope)<-c("Flanker", "Picvocab","Reading","Picture","Pattern")

ggcorrplot(cor_matrix_slope,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           colors = c("aliceblue", "cyan", "darkcyan"),
           lab_size = 5,
           ggtheme = theme_minimal())

### Int - Int 
cor_values_intercepts<-c( 0.3508317, 0.3746573,0.2818775,0.4942867,0.7080654,0.3387791,0.2567058, 0.3257554,0.2582253, 0.2640245)

cor_matrix_intercept <- matrix(NA, nrow = 5, ncol = 5)

cor_matrix_intercept[lower.tri(cor_matrix_intercept)] <- cor_values_intercepts
cor_matrix_intercept[upper.tri(cor_matrix_intercept)] <- t(cor_matrix_intercept)[upper.tri(cor_matrix_intercept)]

colnames(cor_matrix_intercept)<-c("Flanker", "Picvocab","Reading","Picture","Pattern")
row.names(cor_matrix_intercept)<-c("Flanker", "Picvocab","Reading","Picture","Pattern")

ggcorrplot(cor_matrix_intercept,
           hc.order = TRUE,
           type = 'lower',
           lab = TRUE,
           colors = c("blue", "white", "deeppink4"),
           lab_size = 5,
           ggtheme = theme_minimal())
