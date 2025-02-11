library(lavaan)
library(ggplot2)
library(ggcorrplot)
library(tidyverse)
library(ggrain)


rm(list=ls())
uncorrected_wm_visit <- readRDS("uncorrected_wm_visit.rds")
uncorrected_wm_visit$working_mem<-uncorrected_wm_visit$working_mem*100
uncorrected_wm_visit_wide<-reshape(uncorrected_wm_visit, idvar="ID", timevar="eventname", direction = "wide")
colnames(uncorrected_wm_visit_wide)<-c("ID","visit_T1", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1","visit_T2", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "visit_T3", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")

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

#fit model like usual 
fit_full_A<-growth(full_model_A, data=uncorrected_wm_visit_wide,missing='fiml', estimator="MLR")
summary(fit_full_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#fit model using SAM
fit_full_A_sam<-sam(full_model_A, data=uncorrected_wm_visit_wide, missing='fiml',cmd='growth')
summary(fit_full_A_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

### When comparing SAM to normal full model estimations, the normal (fit_full_A) gave unrealistic estimations 


### Model without WM (normal model estimations look good)


no_wm_A<- '
flanker_intercept=~ 1*flanker_T1 + 1*flanker_T2 + 1*flanker_T3
flanker_slope=~ 0*flanker_T1 +flanker_T2 + 1*flanker_T3 

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

#fit model like usual 
fit_no_wm_A<-growth(no_wm_A, data=uncorrected_wm_visit_wide,missing='fiml', estimator="MLR")
summary(fit_no_wm_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)


#fit model using SAM
fit_no_wm_sam<-sam(no_wm_A, data=uncorrected_wm_visit_wide, missing='fiml',cmd='growth')
summary(fit_no_wm_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

### SAM 
#code to get parameter estimates for SAM model

df_sam <- summary(fit_no_wm_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)$pe

# renaming std.all 
colnames(df_sam)[11] <- "SAM_cor"

# keeping relevant columns 
df_sam <- df_sam[, c(1,2,3,11)]

# keeping only the correlations
df_sam <- df_sam[df_sam$op == "~~",] 

# removing unnecessary cov
df_sam <- df_sam[!df_sam$lhs == df_sam$rhs,] 

# adding logical columns to make a group col

df_sam$intercept_dummy <- (str_detect(df_sam$lhs, "intercept") + str_detect(df_sam$rhs, "intercept")) ==2
df_sam$slope_dummy <- (str_detect(df_sam$lhs, "slope") + str_detect(df_sam$rhs, "slope")) ==2
df_sam$within_task <- str_sub(df_sam$lhs, end = 7) == str_sub(df_sam$rhs, end = 7)


# making a col on which relationship it is

# making it empty for now, just to create it 

df_sam$cor_group <- rep(NA, dim(df_sam)[1])

# if intercept_dummy = 1(true), row gets named "int_int"

df_sam$cor_group[df_sam$intercept_dummy == 1] <- rep("int_int", length(df_sam$cor_group[df_sam$intercept_dummy == 1]))

df_sam$cor_group[df_sam$slope_dummy == 1] <- rep("slope_slope", length(df_sam$cor_group[df_sam$slope_dummy == 1]))

df_sam$cor_group[is.na(df_sam$cor_group)] <- rep("int_slope", length(df_sam$cor_group[is.na(df_sam$cor_group)]))

# getting within domain int~~slope using within_task dummy & int_slope

df_sam$cor_group[df_sam$within_task == 1] <- rep("within", length(df_sam$cor_group[df_sam$within_task==1]))


df_sam<-df_sam[, c("lhs", "rhs","cor_group", "SAM_cor")]


# column with the type of relation (intercept, slope & intercept/slope) "type_cor" in df


# Density plot of SAM estimations 

ggplot(df_sam, aes(SAM_cor, fill = cor_group)) +
  geom_density(alpha = .7) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Raincloud plot for SAM estimations
ggplot(df_sam, aes(cor_group, SAM_cor, fill = cor_group)) +
  ggrain::geom_rain() +
  theme_minimal(base_size = 20)


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


psych::describe.by(df_sam$SAM_cor, df_sam$cor_group)

### Normal
#code to get parameter estimates for normally estimated  model

df_nowm_normal <- summary(fit_no_wm_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)$pe

# renaming std.all 
colnames(df_nowm_normal)[11] <- "Normal_cor"

# keeping relevant columns 
df_nowm_normal <- df_nowm_normal[, c(1,2,3,11)]

# keeping only the correlations
df_nowm_normal <- df_nowm_normal[df_nowm_normal$op == "~~",] 

# removing unnecessary cov
df_nowm_normal <- df_nowm_normal[!df_nowm_normal$lhs == df_nowm_normal$rhs,] 

# adding logical columns to make a group col

df_nowm_normal$intercept_dummy <- (str_detect(df_nowm_normal$lhs, "intercept") + str_detect(df_nowm_normal$rhs, "intercept")) ==2
df_nowm_normal$slope_dummy <- (str_detect(df_nowm_normal$lhs, "slope") + str_detect(df_nowm_normal$rhs, "slope")) ==2
df_nowm_normal$within_task <- str_sub(df_nowm_normal$lhs, end = 7) == str_sub(df_nowm_normal$rhs, end = 7)


# making a col on which relationship it is

# making it empty for now, just to create it 

df_nowm_normal$cor_group <- rep(NA, dim(df_nowm_normal)[1])

# if intercept_dummy = 1(true), row gets named "int_int"

df_nowm_normal$cor_group[df_nowm_normal$intercept_dummy == 1] <- rep("int_int", length(df_nowm_normal$cor_group[df_nowm_normal$intercept_dummy == 1]))

df_nowm_normal$cor_group[df_nowm_normal$slope_dummy == 1] <- rep("slope_slope", length(df_nowm_normal$cor_group[df_nowm_normal$slope_dummy == 1]))

df_nowm_normal$cor_group[is.na(df_nowm_normal$cor_group)] <- rep("int_slope", length(df_nowm_normal$cor_group[is.na(df_nowm_normal$cor_group)]))

# getting within domain int~~slope using within_task dummy & int_slope

df_nowm_normal$cor_group[df_nowm_normal$within_task == 1] <- rep("within", length(df_nowm_normal$cor_group[df_nowm_normal$within_task==1]))


df_nowm_normal<-df_nowm_normal[, c("lhs", "rhs","cor_group", "Normal_cor")]



############# checking sign flipping 

# If there are sign flips, I'd get T 

(df_nowm_normal$Normal_cor < 0) + (df_sam$SAM_cor < 0) == 1 

absolute_difference<- abs(df_nowm_normal$Normal_cor- df_sam$SAM_cor)
mean(absolute_difference)
sd(absolute_difference)

# column with the type of relation (intercept, slope & intercept/slope) "type_cor" in df

# Density plot of normal estimations 

ggplot(df_nowm_normal, aes(Normal_cor, fill = cor_group)) +
  geom_density(alpha = .7) +
  theme_minimal(base_size = 20)

# Raincloud plot for normal estimations
ggplot(df_nowm_normal, aes(cor_group, Normal_cor, fill = cor_group)) +
  ggrain::geom_rain() +
  theme_minimal(base_size = 20)

psych::describe.by(df_nowm_normal$Normal_cor, df_nowm_normal$cor_group)

ggplot(long_intercepts, aes(Normal_cor, SAM_cor, fill = cor_group)) +
  theme_minimal(base_size = 20)

ggplot(long_intercepts,aes(estimation_method,value, group=estimation_method, fill=estimation_method))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  geom_smooth(method = 'lm')+
  labs(x='Estimation method', y='Correlation strength')


# flanker_intercept  pattern_intercept     int_int  0.63  0.49

# flanker_slope      picture_slope slope_slope  0.23  0.04

#picvocab_slope      reading_slope 0.55  0.93

# flanker_slope      pattern_slope slope_slope  0.78  0.36

# picvocab_slope      reading_slope slope_slope  0.55  0.93

# picvocab_slope      picture_slope slope_slope  0.61  0.13

# picture_intercept      picture_slope   int_slope -0.09 -0.41

#pattern_intercept      pattern_slope   int_slope -0.13 -0.40

###################################################################

ggplot(both_methods, aes(x = Normal_cor, y = SAM_cor))+
  geom_smooth(method = "lm", se = FALSE) + 
  geom_point( aes(x = Normal_cor, y = SAM_cor, color = cor_group)) +
  theme_minimal()


summary(lm(Normal_cor ~ SAM_cor, data = both_methods))

# this gives us the relationship, if we standardize the variables we get the correlation
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


# lets put a linear slope

plot<-ggplot(both_methods, aes(x = Normal_cor, y = SAM_cor, color =cor_group, labs(x="Standard Correlations", y="SAM Correlations"))) +
  geom_point() +
  geom_abline(intercept=0, slope=1) #+
#  xlim(0,1) + these don't work cause they're out of range i think
#  ylim(0,1)

# Changing label names 

plot+labs(x="Standard Correlations", y="SAM Correlations")


ggplot(both_methods, aes(x = Normal_cor, y = SAM_cor, color = cor_group)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  geom_smooth(method = "lm", se = FALSE)

# this one is a bit much, but just showing it to you
##############################################################

summary(lm(Normal_cor ~ SAM_cor, data = intercepts2))

# this gives us the relationship, if we standardize the variables we get the correlation
summary(lm(as.numeric(scale(Normal_cor)) ~ as.numeric(scale(SAM_cor)), data = intercepts2))
cor(intercepts2$Normal_cor, intercepts2$SAM_cor)

# same values

# now we can get how much each point deviates from the line using the residuals

residuals <- as.numeric(summary(lm(Normal_cor ~ SAM_cor, data = intercepts2))$residuals)

max(residuals)


intercepts2$residuals <- residuals

intercepts2[order(intercepts2$residuals),] # negative error

intercepts2[order(intercepts2$residuals, decreasing = T),] # positive error

# looking at case 2 in the decreasing table above shows a bit of an issue
# we made the SAM vector have a mean shift of 1, therefor even that the estiamtions are almost the same
# 6.5 normal an 6.6 in SAM it has a HIGH residual; since the line is fitting to everyone with a mean shift
# this is not bad, nor good but shows the different info you get by looking at residuals


# lets put a linear slope

plot<-ggplot(intercepts2, aes(x = Normal_cor, y = SAM_cor, color =cor_group, labs(x="Standard Correlations", y="SAM Correlations"))) +
  geom_point() +
  geom_abline(intercept=0, slope=1) 
# Changing label names 

plot+labs(x="Standard Correlations", y="SAM Correlations")



###########################################
# data frame with both types of estimations 

both_methods<-bind_cols(df_nowm_normal, df_sam$SAM_cor)
colnames(both_methods)<-c("lhs", "rhs", "cor_group", "Normal_cor", "SAM_cor")

#subsetting intercept-intercept

intercepts2<-both_methods[(both_methods$cor_group == "int_int"),]

# subseting for each type of estimation
intercept_sam<-df_sam[(df_sam$cor_group == "int_int"),]

intercept_std<-df_nowm_normal[(df_nowm_normal$cor_group=="int_int"),]

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

ggplot(intercepts, aes(1, SAM_cor, fill = cor_group, )) +
  geom_rain(alpha = .5, 
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list( #nudging boxplots so that they dont overlap
              position = ggpp::position_dodgenudge(x = .1, width = 0.1), width = 0.1 
            )) +
  theme_classic() +
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2')+
  coord_flip()

ggplot(intercepts2$Normal_cor, intercepts2$...5, aes(intercepts2)) +
  geom_density(alpha = .7) +
  theme_minimal(base_size = 20)


#subsetting slope-slope

slopes<-both_methods[(both_methods$cor_group == "slope_slope"),]

#subsetting intercept-slope between domains 

int_slope_between<-both_methods[(both_methods$cor_group == "int_slope"),]

#subsetting intercept-intercept

int_slope_within<-both_methods[(both_methods$cor_group == "int_slope_within"),]











######################################################


###Removing pattern to test again


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
fit_no_wm_pattern_A<-growth(no_wm_pattern_model_A, data=full_scores_wide,missing='fiml')
summary(fit_no_wm_pattern_A, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

#fit model using SAM
fit_no_wm_pattern_A_sam<-sam(no_wm_pattern_model_A, data=uncorrected_wm_visit_wide, missing='fiml',cmd='growth')
summary(fit_no_wm_pattern_A_sam, fit.measures = TRUE, rsquare = TRUE, standardized = TRUE)

