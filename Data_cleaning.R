library(lavaan)
library(GGally)
library(summarytools)
library(visdat)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(ggrain)

data_raw_all<-read.csv('nc_y_nihtb.csv')

#checking for correlation between raw and uncorrected

#picture
picture_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_picture_uncorrected)
colnames(picture_uncorrected)<-c('ID','eventname','picture_uncorrected')
picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

picture_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_picture_rawscore)
colnames(picture_raw)<-c('ID','eventname','picture_raw')
picture_raw_wide<-reshape(picture_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_picture<-merge(picture_uncorrected_wide, picture_raw_wide)

cor_picture<- cor_picture[,2:7] 
cor(cor_picture, use='pairwise.complete.obs')

#flanker 
flanker_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_flanker_uncorrected)
colnames(flanker_uncorrected)<-c('ID','eventname','flanker_uncorrected')
flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

flanker_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_flanker_rawscore)
colnames(flanker_raw)<-c('ID','eventname','flanker_raw')
flanker_raw_wide<-reshape(flanker_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_flanker<-merge(flanker_uncorrected_wide, flanker_raw_wide)

cor_flanker<- cor_flanker[,2:7] 
cor(cor_flanker, use='pairwise.complete.obs')

#pattern
pattern_uncorrected<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_pattern_uncorrected)
colnames(pattern_uncorrected)<-c('ID','eventname','pattern_uncorrected')
pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

pattern_raw<-data.frame(data_raw$src_subject_id, data_raw$eventname, data_raw$nihtbx_pattern_rawscore)
colnames(pattern_raw)<-c('ID','eventname','pattern_raw')
pattern_raw_wide<-reshape(pattern_raw, idvar = "ID", timevar = "eventname", direction = "wide")

cor_pattern<-merge(pattern_uncorrected_wide, pattern_raw_wide)

cor_pattern<- cor_pattern[,2:7] 
cor(cor_pattern, use='pairwise.complete.obs')

#if Nick says the correlations are high enough, I change raw to uncorrected 
#flanker is very weird, the others are good 
#maybe research that 


#########Uncorrected##########

#cleaning: subset uncorrected scores 
uncorrected_scores<- 
  data_raw_all %>% 
  select(matches("src_subject_id|eventname|uncorrected")) %>%
  select(!c(nihtbx_list_uncorrected, nihtbx_cardsort_uncorrected,nihtbx_fluidcomp_uncorrected,nihtbx_cryst_uncorrected,nihtbx_totalcomp_uncorrected))



#number of NA for picture uncorrected #the uncorrected has 3 more missing values 
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picture_uncorrected, ~sum(is.na(.))))

#number of NA for picvocab 
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_picvocab_uncorrected, ~sum(is.na(.))))

#number of NA for flanker
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_flanker_uncorrected, ~sum(is.na(.))))

#number of NA for reading
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_reading_uncorrected, ~sum(is.na(.))))

#number of NA for pattern
uncorrected_scores %>% 
  group_by(eventname) %>% 
  summarise(across(nihtbx_pattern_uncorrected, ~sum(is.na(.))))


#percentage of NA for all uncorrected 
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


#visualizing NA for uncorrected 
vis_dat(uncorrected_scores, warn_large_data= FALSE) 



#Subsetting and renaming columns 

#changing eventname labels (will do after I merge the datasets)
#uncorrected_scores <- uncorrected_scores %>%
 # mutate(eventname = case_when(
   # eventname == "baseline_year_1_arm_1" ~ "T_1",
    #eventname == "2_year_follow_up_y_arm_1" ~ "T_2",
   # eventname == "4_year_follow_up_y_arm_1" ~ "T_3"))

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


#Spaghetti plots for each task 

#Picture vocabulary 
ggplot(picvocab_uncorrected,aes(eventname,picvocab_uncorrected, group=ID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Vocabulary Score')

#Flanker 
ggplot(flanker_uncorrected,aes(eventname,flanker_uncorrected, group=ID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Flanker Score')

#Pattern
ggplot(pattern_uncorrected,aes(eventname,pattern_uncorrected, group=ID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Pattern Score')

#Picture
ggplot(picture_uncorrected,aes(eventname,picture_uncorrected, group=ID))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Picture Score')

#Reading
ggplot(reading_uncorrected,aes(eventname,reading_uncorrected, group=ID))+
  geom_point(alpha=0.5)+
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
  theme(axis.title.y = element_blank())+
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  coord_flip()

#plotting the raw Flanker  (also looks very weird)
ggplot(flanker_raw, aes(eventname, flanker_raw, fill = eventname)) +
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





#Reading (1 outliar)
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


#Converting long to wide 

#Picture Vocabulary 

#long to wide 
picvocab_uncorrected_wide<-reshape(picvocab_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

#visualise relationships between scores at 3 timepoints
ggpairs(picvocab_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkgreen")),
        diag=list(continuous="bar"))


#Flanker #low correlations between timepoints 

flanker_uncorrected_wide<-reshape(flanker_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(flanker_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="red")),
        diag=list(continuous="bar"))


#Pattern 

pattern_uncorrected_wide<-reshape(pattern_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(pattern_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="orange")),
        diag=list(continuous="bar"))

#Picture 

picture_uncorrected_wide<-reshape(picture_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")

ggpairs(picture_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="darkred")),
        diag=list(continuous="bar"))


#Reading 

reading_uncorrected_wide<-reshape(reading_uncorrected, idvar = "ID", timevar = "eventname", direction = "wide")
ggpairs(reading_uncorrected_wide[,2:4],lower=list(continuous=wrap("smooth", colour="green")),
        diag=list(continuous="bar"))


############Working memory###########

working_memory<-read.csv('mri_y_tfmr_nback_beh_workingmem.csv')

test_uncorrected<-uncorrected_scores

#subsetting ID, timepoint and rate of correct responses
working_memory<- 
  working_memory %>% 
  select(matches("src_subject_id|eventname|tfmri_nb_all_beh_ctotal_rate"))

#joining uncorrected and wm 
uncorrected_wm<-full_join(test_uncorrected, working_memory, by=join_by('src_subject_id','eventname'))
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

#these give me different results, does that mean that some people have been scanned at timepoints when they did not do any tests?
table(uncorrected_scores$eventname)
table(uncorrected_wm$eventname)


#spaghetti plot for wm #ceiling effects/looks nonlinear
ggplot(working_memory,aes(eventname,tfmri_nb_all_beh_ctotal_rate, group=src_subject_id))+
  geom_point(alpha=0.5)+
  geom_line(alpha=0.5)+
  labs(x='Eventname', y='Working memory score')

#raincloud plot
ggplot(working_memory, aes(eventname, tfmri_nb_all_beh_ctotal_rate, fill = eventname)) +
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

#relationship between timepoints 
working_memory_wide<-reshape(working_memory, idvar = "src_subject_id", timevar = "eventname", direction = "wide")
ggpairs(working_memory_wide[,2:4],lower=list(continuous=wrap("smooth", colour="pink")),
        diag=list(continuous="bar"))


########visit_type#########
visit_type<-read.csv('visit_type.csv')


#subsetting 
visit_type<-
  visit_type%>%
  select(matches('src_subject_id|eventname|visit_type'))

#joining with uncorrected wm
#I have joined visit types for every measurement point, did not want that 
#I must only keep the visit type for the  3 measurement times in the uncorrected_wm_visits 
uncorrected_wm_visit<-
  left_join(uncorrected_wm, visit_type, by=join_by('src_subject_id','eventname'))

  
#reordering columns 
uncorrected_wm_visit<- select(uncorrected_wm_visit, src_subject_id, eventname, visit_type, nihtbx_picvocab_uncorrected, 
nihtbx_flanker_uncorrected, nihtbx_pattern_uncorrected, nihtbx_picture_uncorrected, nihtbx_reading_uncorrected, tfmri_nb_all_beh_ctotal_rate)

##########raw_scores##############

#cleaning: subset raw scores by keywords+remove unnecessary variables 
raw_scores<- 
  data_raw_all %>% 
  select(matches("src_subject_id|eventname|raw|rs")) %>%
  select(!c(nihtbx_list_rawscore, nihtbx_cardsort_rawscore,nihtbx_fluidcomp_rs,nihtbx_cryst_rawscore,nihtbx_totalcomp_rawscore))


##################TRANSFER############################

#removing columns containing: Dimensional Change Card Sort & List Sorting Working Memory & Fluid & Total Composite & Administration Method
#too long though (also started removing some date, language, version tests but did not finish)
extra<-c('nihtbx_list_date','nihtbx_list_language','nihtbx_list_uncorrected','nihtbx_list_agecorrected','nihtbx_list_v','nihtbx_list_rawscore','nihtbx_list_theta','nihtbx_list_itmcnt','nihtbx_list_cs','nihtbx_list_fc',
         'nihtbx_cardsort_date','nihtbx_cardsort_language','nihtbx_cardsort_uncorrected','nihtbx_cardsort_agecorrected','nihtbx_cardsort_v','nihtbx_cardsort_rawscore','nihtbx_cardsort_theta','nihtbx_cardsort_itmcnt','nihtbx_cardsort_cs','nihtbx_cardsort_fc',
         'neurocog_working_mem___1','neurocog_working_mem___2','neurocog_working_mem___3',
         'neurocog_card___1','neurocog_card___2','neurocog_card___3',
         'nihtbx_totalcomp_rawscore','nihtbx_totalcomp_theta','nihtbx_totalcomp_itmcnt','nihtbx_totalcomp_cs','nihtbx_totalcomp_fc',
         'nihtbx_fluidcomp_rs','nihtbx_fluidcomp_theta','nihtbx_fluidcomp_itmcnt','nihtbx_fluidcomp_cs','nihtbx_fluidcomp_fc', 
         'neurocog_card___1','neurocog_card___2','neurocog_card___3','neurocog_flanker___1','neurocog_flanker___2','neurocog_flanker___3','neurocog_pattern___1','neurocog_pattern___2','neurocog_pattern___3','neurocog_pic_sequence___1','neurocog_pic_sequence___2',
         'neurocog_pic_sequence___3','neurocog_pic_vocab___1','neurocog_pic_vocab___2','neurocog_pic_vocab___3','neurocog_reading___1','neurocog_reading___2','neurocog_reading___3','neurocog_working_mem___1','neurocog_working_mem___2','neurocog_working_mem___3',
         'nihtbx_picvocab_language','nihtbx_picvocab_date','nihtbx_picvocab_v','nihtbx_flanker_date','nihtbx_flanker_language','nihtbx_flanker_v','nihtbx_pattern_date','nihtbx_pattern_language','nihtbx_pattern_v',)

data_raw_84<- data_raw_all[,!(names(data_raw_all) %in% extra)]


#code for selecting by keywords but produces dataframes and I don't know how to remove them
DCCS<-select(data_raw_all, contains("cardsort"),contains("list"))
LSWM<-select(data_raw_all, contains("list"))
language<-select(data_raw_all, contains("language"))


#maybe I can do the opposite and subset the ones I actually want to keep 
#subsetting ID, measurement time, uncorrected scores
data_raw_uncorrected <- select(data_raw_84, matches("src_subject_id|eventname|uncorrected"))

#if I do it in this order, I dont get the same result (matches does not work?)
raw_scores<- 
  data_raw_all %>% 
  select(!c(nihtbx_list_rawscore, nihtbx_cardsort_rawscore,nihtbx_fluidcomp_rs,nihtbx_cryst_rawscore,nihtbx_totalcomp_rawscore))%>%
select(matches("src_subject_id|eventname|raw|rs")) 
#this one does not work (but not same as the above)
raw_scores<- select(data_raw_all, -nihtbx_list_rawscore, -nihtbx_cardsort_rawscore, matches("src_subject_id|eventname|raw|rs"))

#the Fluid and Total Composite Scores could not be calculated for the follow-ups (2 fluid tests not administered) so removing them too?)
#descriptives
view(dfSummary(data_raw_uncorrected$nihtbx_totalcomp_rawscore)) #100% missing
view(dfSummary(data_raw_uncorrected$nihtbx_fluidcomp_uncorrected))  #56% missing
view(dfSummary(data_raw_uncorrected$nihtbx_totalcomp_uncorrected)) #56% missing???



