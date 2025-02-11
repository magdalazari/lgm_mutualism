library(lavaan)
library(GGally)
library(summarytools)
library(visdat)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(ggrain)
library(reshape2)

### Script #1, cleaning and visualizing data 


# Loading data from files 
data_all<-read.csv('nc_y_nihtb.csv')

# changing time point names  
data_all<- data_all%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

##### Checking for correlations between raw and uncorrected scores, reporting T_1

### Picture: 0.97 cor

# uncorrected 

picture_uncorrected<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_picture_uncorrected)
colnames(picture_uncorrected)<-c('ID','eventname','picture_uncorrected')
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")


# raw

picture_raw<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_picture_rawscore)
colnames(picture_raw)<-c('ID','eventname','picture_raw')
picture_raw_wide<-reshape(picture_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_picture<-merge(picture_uncorrected_wide, picture_raw_wide)

cor_picture<- cor_picture[,2:7] 
cor(cor_picture, use='pairwise.complete.obs')


### Flanker: 0.35 cor

# uncorrected

flanker_uncorrected<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_flanker_uncorrected)
colnames(flanker_uncorrected)<-c('ID','eventname','flanker_uncorrected')
flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

# raw 

flanker_raw<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_flanker_rawscore)
colnames(flanker_raw)<-c('ID','eventname','flanker_raw')
flanker_raw_wide<-reshape(flanker_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_flanker<-merge(flanker_uncorrected_wide, flanker_raw_wide)

cor_flanker<- cor_flanker[,2:7] 
cor(cor_flanker, use='pairwise.complete.obs')


### Pattern: 0.99 cor

# uncorrected

pattern_uncorrected<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_pattern_uncorrected)
colnames(pattern_uncorrected)<-c('ID','eventname','pattern_uncorrected')
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")


# raw 

pattern_raw<-data.frame(data_all$src_subject_id, data_all$eventname, data_all$nihtbx_pattern_rawscore)
colnames(pattern_raw)<-c('ID','eventname','pattern_raw')
pattern_raw_wide<-reshape(pattern_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_pattern<-merge(pattern_uncorrected_wide, pattern_raw_wide)

cor_pattern<- cor_pattern[,2:7] 
cor(cor_pattern, use='pairwise.complete.obs')


### going for uncorrected scores 

# subsetting uncorrected  

uncorrected_scores<- 
  data_all %>% 
  select(matches("src_subject_id|eventname|uncorrected")) %>%
  select(!c(nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected,nihtbx_fluidcomp_uncorrected,nihtbx_cryst_uncorrected,nihtbx_totalcomp_uncorrected))



###Number of NA/task 

# picture sequence
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picture_uncorrected, ~sum(is.na(.))))

# picture vocabulary 
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picvocab_uncorrected, ~sum(is.na(.))))

# flanker
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_flanker_uncorrected, ~sum(is.na(.))))

# reading
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_reading_uncorrected, ~sum(is.na(.))))

# pattern
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_pattern_uncorrected, ~sum(is.na(.))))


### Percentage of NA for all 

uncorrected_scores %>% 
group_by(eventname) %>% 
summarise(across(c(nihtbx_picture_uncorrected, nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_reading_uncorrected, nihtbx_pattern_uncorrected), 
~ sum(is.na(.)) / n() * 100))


# visualizing NA  

vis_dat(uncorrected_scores, warn_large_data= FALSE) 


### Data frames for the 2 remaining tasks 

#Picture Vocabulary 

picvocab_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_picvocab_uncorrected)
colnames(picvocab_uncorrected)<-c('ID','eventname','picvocab_uncorrected')

#Reading 
reading_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_reading_uncorrected)
colnames(reading_uncorrected)<-c('ID','eventname','reading_uncorrected')

### Making everything wide 

picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

### Spaghetti plots for each task 

# Picture vocabulary 

ggplot(picvocab_uncorrected,aes(eventname,picvocab_uncorrected, group=ID))+
  geom_point(alpha=0.3, col='darkgreen')+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Vocabulary Score')

# Flanker 

ggplot(flanker_uncorrected,aes(eventname,flanker_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="red")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Flanker Score')

# Pattern

ggplot(pattern_uncorrected,aes(eventname,pattern_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="orange")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Pattern Score')

# Picture

ggplot(picture_uncorrected,aes(eventname,picture_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="darkred")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Score')

# Reading (looks linear without the outlier)

ggplot(reading_uncorrected,aes(eventname,reading_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="green")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Reading Score')


### Handling outlier for reading (noticed in spaghetti plot)

#how much is the max value? 180
max(reading_uncorrected_wide$reading_uncorrected.T_2, na.rm = T)


#how many standard deviations above the mean is the max value? 12.62135
max(scale(reading_uncorrected_wide$reading_uncorrected.T_2), na.rm=T)


#where is the (first) maximum value? 3782 (not correct!! sometimes happens in a changed dataframe)
which.max(reading_uncorrected_wide$reading_uncorrected.T_2)

#Removing outlier from first dataframe 
uncorrected_scores[uncorrected_scores==180]<-NA




### Raincloud plots 

# Picture  vocabulary

#comparing time points side by side (no overlapping)
ggplot(uncorrected_scores, aes(eventname, nihtbx_picvocab_uncorrected, fill = eventname)) +
  geom_rain(alpha = .5, 
          boxplot.args.pos = list( #making them less crammed 
            width = 0.05, position = position_nudge(x = 0.13)),
          violin.args.pos = list(
            side = "r",
            width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

# Flanker (looks weird & ceiling)
ggplot(uncorrected_scores, aes(eventname, nihtbx_flanker_uncorrected, fill = eventname)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list(  
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  xlab('flanker_uncorrected')+
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

# Pattern 
ggplot(uncorrected_scores, aes(eventname, nihtbx_pattern_uncorrected, fill = eventname)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list(  
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()


# Picture 
ggplot(uncorrected_scores, aes(eventname, nihtbx_picture_uncorrected, fill = eventname)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list( 
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()


# Reading
ggplot(uncorrected_scores, aes(eventname, nihtbx_reading_uncorrected, fill = eventname)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list( 
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

### Correlations between timepoints for each task

# Picture Vocabulary: 0.7-0.76

ggpairs(picvocab_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkgreen")),
        diag=list(continuous="bar"))


#Flanker: 0.35-0.48

ggpairs(flanker_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="red")),
        diag=list(continuous="bar"))

#Pattern: 0.48-0.53

ggpairs(pattern_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="orange")),
        diag=list(continuous="bar"))

#Picture: 0.37-0.43

ggpairs(picture_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkred")),
        diag=list(continuous="bar"))


#Reading 0.69-0.76

ggpairs(reading_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="green")),
        diag=list(continuous="bar"))


############Working memory###########

working_memory_all<-read.csv('mri_y_tfmr_nback_beh_workingmem.csv')

working_memory_all<-working_memory_all%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

# subsetting ID, timepoint and rate of correct responses
rate_of_correct<- 
  working_memory_all%>% 
  select(matches("src_subject_id|eventname|tfmri_nb_all_beh_ctotal_rate"))

# multiplying so that absolute values are not as small 

rate_of_correct_scaled<-mutate_if(rate_of_correct, is.numeric, ~ . * 10)

#joining uncorrected and wm 
uncorrected_wm<-full_join(uncorrected_scores, rate_of_correct_scaled, by=join_by('src_subject_id','eventname'))

#number of NA for working memory 
uncorrected_wm %>% 
  group_by(eventname) %>% 
  summarise(across(tfmri_nb_all_beh_ctotal_rate, ~sum(is.na(.))))

#percentage of NA for working memory 
uncorrected_wm %>% 
  group_by(eventname) %>% 
  summarise(across(tfmri_nb_all_beh_ctotal_rate, 
                   ~ sum(is.na(.)) / n() * 100))


# Spaghetti plot 

ggplot(rate_of_correct_scaled,aes(eventname,tfmri_nb_all_beh_ctotal_rate, group=src_subject_id))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Working memory score')


# Raincloud plot

ggplot(rate_of_correct_scaled, aes(eventname, tfmri_nb_all_beh_ctotal_rate, fill = eventname)) +
  geom_rain(alpha = .5, 
            boxplot.args.pos = list( 
              width = 0.05, position = position_nudge(x = 0.13)),
            violin.args.pos = list(
              side = "r",
              width = 0.7, position = position_nudge(x = 0.2))) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

# Wide

rate_of_correct_scaled_wide<-reshape(rate_of_correct_scaled, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

# corr between time points: 0.395-0.483
ggpairs(rate_of_correct_scaled_wide[,2:4],lower=list(continuous=wrap("smooth", colour="pink")),
        diag=list(continuous="bar"))



########visit_type#########

visit_type<-read.csv('visit_type.csv')

visit_type<-
  visit_type%>%
  select(matches('src_subject_id|eventname|visit_type'))

# renaming time points 

visit_type<-visit_type%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))


### Joining with uncorrected scores and wm 
# full_join joined visit types for every ABCD measurement point 
# I must only keep the visit type for the 3 measurement times in the uncorrected_wm_visit, so left_join

uncorrected_wm_visit<-
  left_join(uncorrected_wm, visit_type, by=join_by('src_subject_id','eventname'))



#### Renaming and reordering final  

#reordering columns 
uncorrected_wm_visit<- select(uncorrected_wm_visit, src_subject_id, eventname, visit_type, nihtbx_picvocab_uncorrected, 
                              nihtbx_flanker_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, tfmri_nb_all_beh_ctotal_rate)

#renaming columns 
colnames(uncorrected_wm_visit)<-c('ID','eventname','visit_type', 'pic_vocab','flanker', 'pattern', 'picture','reading','working_mem')

# wide 
uncorrected_wm_visit_wide<-reshape(uncorrected_wm_visit, idvar="ID", timevar="eventname", direction = "wide")
colnames(uncorrected_wm_visit_wide)<-c("ID","visit_T1", "picvocab_T1", "flanker_T1","pattern_T1", "picture_T1", "reading_T1", "wm_T1","visit_T2", "picvocab_T2", "flanker_T2","pattern_T2", "picture_T2", "reading_T2", "wm_T2", "visit_T3", "picvocab_T3", "flanker_T3","pattern_T3", "picture_T3", "reading_T3", "wm_T3")


# saving data frames in files  

saveRDS(uncorrected_wm_visit, file = "uncorrected_wm_visit.rds")
saveRDS(uncorrected_wm_visit_wide, file = "uncorrected_wm_visit_wide.rds")





