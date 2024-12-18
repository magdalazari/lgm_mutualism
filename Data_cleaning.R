library(lavaan)
library(GGally)
library(summarytools)
library(visdat)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(ggrain)
library(reshape2)

data_all<-read.csv('nc_y_nihtb.csv')

#checking for correlation between raw and uncorrected (report T_1)

#picture: 0.97
picture_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_picture_uncorrected)
colnames(picture_uncorrected)<-c('ID','eventname','picture_uncorrected')
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

picture_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_picture_rawscore)
colnames(picture_raw)<-c('ID','eventname','picture_raw')
picture_raw_wide<-reshape(picture_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_picture<-merge(picture_uncorrected_wide, picture_raw_wide)

cor_picture<- cor_picture[,2:7] 
cor(cor_picture, use='pairwise.complete.obs')

#flanker: 0.35
flanker_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_flanker_uncorrected)
colnames(flanker_uncorrected)<-c('ID','eventname','flanker_uncorrected')
flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

flanker_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_flanker_rawscore)
colnames(flanker_raw)<-c('ID','eventname','flanker_raw')
flanker_raw_wide<-reshape(flanker_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_flanker<-merge(flanker_uncorrected_wide, flanker_raw_wide)

cor_flanker<- cor_flanker[,2:7] 
cor(cor_flanker, use='pairwise.complete.obs')

#pattern: 0.99
pattern_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_pattern_uncorrected)
colnames(pattern_uncorrected)<-c('ID','eventname','pattern_uncorrected')
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

pattern_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_pattern_rawscore)
colnames(pattern_raw)<-c('ID','eventname','pattern_raw')
pattern_raw_wide<-reshape(pattern_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_pattern<-merge(pattern_uncorrected_wide, pattern_raw_wide)

cor_pattern<- cor_pattern[,2:7] 
cor(cor_pattern, use='pairwise.complete.obs')


#########Uncorrected##########

#Subsetting uncorrected scores 
uncorrected_scores<- 
  data_raw_all %>% 
  select(matches("src_subject_id|eventname|uncorrected")) %>%
  select(!c(nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected,nihtbx_fluidcomp_uncorrected,nihtbx_cryst_uncorrected,nihtbx_totalcomp_uncorrected))


uncorrected_scores<- uncorrected_scores%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))


###Handling outlier for reading (noticed in spaghetti plot)

#how much is the max value? 180
max(reading_uncorrected_wide$reading_uncorrected.T_2, na.rm = T)


#how many standard deviations above the mean is the max value? 12.62135
max(scale(reading_uncorrected_wide$reading_uncorrected.T_2), na.rm=T)


#where is the (first) maximum value? 3782 (not correct?)
which.max(reading_uncorrected_wide$reading_uncorrected.T_2)

#Removing outlier from first dataframe 

uncorrected_scores[uncorrected_scores==180]<-NA



###Number of NA

#picture seq
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picture_uncorrected, ~sum(is.na(.))))

#picture vocabulary 
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picvocab_uncorrected, ~sum(is.na(.))))

#flanker
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_flanker_uncorrected, ~sum(is.na(.))))

#reading
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_reading_uncorrected, ~sum(is.na(.))))

#pattern
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_pattern_uncorrected, ~sum(is.na(.))))


### Percentage of NA 
uncorrected_scores %>% 
group_by(eventname) %>% 
summarise(across(c(nihtbx_picture_uncorrected, nihtbx_picvocab_uncorrected, nihtbx_flanker_uncorrected, nihtbx_reading_uncorrected, nihtbx_pattern_uncorrected), 
~ sum(is.na(.)) / n() * 100))
#do the calculations to double check ?

#try this  
ct %>% 
group_by(Species) %>%
mutate(logi_missing = is.na(Sepal.Length)) %>%
summarise(m = mean(Sepal.Length, na.rm = T), n = n(), count_missing = sum(logi_missing))


#visualizing NA  
vis_dat(uncorrected_scores, warn_large_data= FALSE) 



#Renaming columns for raw and flanker (not useful anymore)


data_raw <- data_raw %>%
  mutate(eventname = case_when(
  eventname == "baseline_year_1_arm_1" ~ "T_1",
  eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
  eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

flanker_raw<- flanker_raw%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

###Data frame for each task 

#Picture Vocabulary 

picvocab_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_picvocab_uncorrected)
colnames(picvocab_uncorrected)<-c('ID','eventname','picvocab_uncorrected')

#Flanker 

flanker_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_flanker_uncorrected)
colnames(flanker_uncorrected)<-c('ID','eventname','flanker_uncorrected')

#Pattern 

pattern_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_pattern_uncorrected)
colnames(pattern_uncorrected)<-c('ID','eventname','pattern_uncorrected')

#Picture 

picture_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_picture_uncorrected)
colnames(picture_uncorrected)<-c('ID','eventname','picture_uncorrected')

#Reading 
reading_uncorrected<-data.frame(uncorrected_scores$src_subject_id, uncorrected_scores$eventname, uncorrected_scores$nihtbx_reading_uncorrected)
colnames(reading_uncorrected)<-c('ID','eventname','reading_uncorrected')


#Spaghetti plot for each task 

#Picture vocabulary 
ggplot(picvocab_uncorrected,aes(eventname,picvocab_uncorrected, group=ID))+
  geom_point(alpha=0.3, col='darkgreen')+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Vocabulary Score')

#Flanker 
ggplot(flanker_uncorrected,aes(eventname,flanker_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="red")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Flanker Score')

#Pattern
ggplot(pattern_uncorrected,aes(eventname,pattern_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="orange")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Pattern Score')

#Picture
ggplot(picture_uncorrected,aes(eventname,picture_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="darkred")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Score')

#Reading (looks linear without the outlier)
ggplot(reading_uncorrected,aes(eventname,reading_uncorrected, group=ID))+
  geom_point(alpha=0.3, colour="green")+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Reading Score')


#Raincloud plots 

#Picture  vocabulary

ggplot(uncorrected_scores, aes(1, nihtbx_picvocab_uncorrected, fill = eventname, color = eventname)) +
  geom_rain(alpha = .5, rain.side = 'l', #flipping plot to left
            boxplot.args = list(color = "black", outlier.shape = NA),
            boxplot.args.pos = list( #nudging boxplots so that they dont overlap
            position = ggpp::position_dodgenudge(x = .1, width = 0.1), width = 0.1)) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  scale_color_brewer(palette = 'Dark2') 

#comparing timepoints side by side (no overlapping)
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

#Flanker (looks weird at T_2 and T_3)
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

#Pattern (looks ok)
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


#Picture (ceiling effects in T_3?)
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


#Reading
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

###Correlations between timepoints for each task

#Picture Vocabulary: 0.7-0.76

picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(picvocab_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkgreen")),
        diag=list(continuous="bar"))


#Flanker: 0.35-0.48

flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

flanker_uncorrected_wide<-flanker_uncorrected_wide%>%
mutate(eventname = case_when(
eventname == "baseline_year_1_arm_1" ~ "T_1",
eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))
  
ggpairs(flanker_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="red")),
        diag=list(continuous="bar"))

#Pattern: 0.48-0.53

pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(pattern_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="orange")),
        diag=list(continuous="bar"))

#Picture: 0.37-0.43

picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(picture_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkred")),
        diag=list(continuous="bar"))


#Reading 0.69-0.76

reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(reading_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="green")),
        diag=list(continuous="bar"))


############Working memory###########

working_memory_all<-read.csv('mri_y_tfmr_nback_beh_workingmem.csv')

#subsetting ID, timepoint and rate of correct responses
rate_of_correct<- 
  working_memory_all%>% 
  select(matches("src_subject_id|eventname|tfmri_nb_all_beh_ctotal_rate"))

#joining uncorrected and wm 
uncorrected_wm<-full_join(uncorrected_scores, rate_of_correct, by=join_by('src_subject_id','eventname'))
#when eventname had different values between dataframes I used to get all NA for wm 


#number of NA for working memory 
uncorrected_wm %>% 
  group_by(eventname) %>% 
  summarise(across(tfmri_nb_all_beh_ctotal_rate, ~sum(is.na(.))))

#percentage of NA for working memory 
uncorrected_wm %>% 
  group_by(eventname) %>% 
  summarise(across(tfmri_nb_all_beh_ctotal_rate, 
                   ~ sum(is.na(.)) / n() * 100))


#renaming timepoints for WM
rate_of_correct<-rate_of_correct%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))


#spaghetti plot for wm  
ggplot(rate_of_correct,aes(eventname,tfmri_nb_all_beh_ctotal_rate, group=src_subject_id))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Working memory score')


#raincloud plot
ggplot(rate_of_correct, aes(eventname, tfmri_nb_all_beh_ctotal_rate, fill = eventname)) +
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

#make wide 
rate_of_correct_wide<-reshape(rate_of_correct, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

#relationship between timepoints 0.395-0.483
ggpairs(rate_of_correct_wide[,2:4],lower=list(continuous=wrap("smooth", colour="pink")),
        diag=list(continuous="bar"))



########visit_type#########
visit_type<-read.csv('visit_type.csv')


#subsetting 
visit_type<-
  visit_type%>%
  select(matches('src_subject_id|eventname|visit_type'))

#joining with uncorrected wm 
#full_join joined visit types for every measurement point, did not want that 
#I must only keep the visit type for the 3 measurement times in the uncorrected_wm_visit, so left_join
uncorrected_wm_visit<-
  left_join(uncorrected_wm, visit_type, by=join_by('src_subject_id','eventname'))



####Renaming/reordering final data frame 

#reordering columns 
uncorrected_wm_visit<- select(uncorrected_wm_visit, src_subject_id, eventname, visit_type, nihtbx_picvocab_uncorrected, 
                              nihtbx_flanker_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, tfmri_nb_all_beh_ctotal_rate)

#renaming columns 
colnames(uncorrected_wm_visit)<-c('ID','eventname','visit_type', 'pic_vocab','flanker', 'pattern', 'picture','reading','working_mem')


#renaming time points (not necessary, I did it above)
uncorrected_wm_visit<-uncorrected_wm_visit%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))


#saving dataframe to file to make it readable by a different script 
saveRDS(uncorrected_wm_visit, file = "uncorrected_wm_visit.rds")



###############Little man task###############

little_man<-read.csv('nc_y_lmt.csv')

#subsetting correct percentage of all 32 trials 
little_man<-
  little_man%>%
  select(matches('src_subject_id|eventname|lmt_scr_perc_correct'))

#number of NA  
little_man %>% 
  group_by(eventname) %>% 
  summarise(across(lmt_scr_perc_correct, ~sum(is.na(.))))

#percentage of NA  
little_man %>% 
  group_by(eventname) %>% 
  summarise(across(lmt_scr_perc_correct, 
                   ~ sum(is.na(.)) / n() * 100))

table(little_man$eventname)

#spaghetti plot

little_man<-little_man%>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "T_1",
    eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
    eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

ggplot(little_man,aes(eventname,lmt_scr_perc_correct, group=src_subject_id))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Little man score')

#simple plot with regression line

ggplot(little_man, aes(eventname, lmt_scr_perc_correct, group=src_subject_id)) +
  geom_point(color = "blue", alpha = 0.7) +  # Add points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  #regression line
  theme_minimal()

#without regression line
ggplot(little_man, aes(eventname, lmt_scr_perc_correct, group=src_subject_id)) +
  geom_point(color = "blue", alpha = 0.7) +  # Add points
  theme_minimal()


#raincloud plot
ggplot(little_man, aes(eventname, lmt_scr_perc_correct, fill = eventname)) +
  geom_rain(alpha = .5, 
           ) +
  theme_classic() +
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

#relationship between timepoints: T_1 has low correlations with the res
little_man_wide<-reshape(little_man, idvar = "src_subject_id", timevar = "eventname", direction = "wide")
ggpairs(little_man_wide[,2:4],lower=list(continuous=wrap("smooth", colour="pink")),
        diag=list(continuous="bar"))

##########raw_scores##############

#cleaning: subset raw scores by keywords+remove unnecessary variables 
raw_scores<- 
  data_raw_all %>% 
  select(matches("src_subject_id|eventname|raw|rs")) %>%
  select(!c(nihtbx_list_rawscore, nihtbx_cardsort_rawscore,nihtbx_fluidcomp_rs,nihtbx_cryst_rawscore,nihtbx_totalcomp_rawscore))



