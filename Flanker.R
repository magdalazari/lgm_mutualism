############# Scatterplot of raw and uncorrected flanker for T_1

setwd("C:/Users/Kiwi/surfdrive/Shared/abcd-data-release-5.1/core/neurocognition")

#dataframe for raw and uncorrected (long)
flanker_raw_uncorrected<-full_join(flanker_uncorrected, flanker_raw , by=join_by('ID','eventname'))

#cor_flanker is the same but wide, cringe. I think I can do it from this one if I include ID and drop
#some unnecessary columns 

#subset only T_1 (or any timepoint)
cor_flanker_test<-
  cor_flanker[c("ID", "flanker_uncorrected.T_1", "flanker_raw.baseline_year_1_arm_1")]

plot(cor_flanker_test)

ggplot(cor_flanker_test, aes(x = flanker_uncorrected.T_1, y = flanker_raw.baseline_year_1_arm_1, group=ID)) +
  geom_point(color = "blue", alpha = 0.7) +  # Add points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line?????
  labs(title = "Raw vs. Uncorrected Scores (First Wave)",
       x = "Uncorrected Score",
       y = "Raw Score",
       color = "Subject") +
  theme_minimal()



#Millisecond Flanker 
flanker_inc<- read.csv('nc_y_flkr.csv')

############# Michaels suggestion:

# 1. subset rows from year 2 2. subset again keeping only the score columns 3. convert to numeric

#nicks code
as.numeric(scale(performance)) + as.numeric(scale(RT)) == uncorrected


#subset only T_2 in the Millisecond
year_2<-flanker_inc%>%
  filter(eventname=='2_year_follow_up_y_arm_1')


#subseting column for proportions of correct
proportion_correct<-
  year_2%>% select(flkr_scr_propcorrect)

#making proportion numeric #did not work prior to using unlist, has to be numeric and was dataframe

proportion_correct<-as.numeric(unlist(proportion_correct))


#subseting column for reaction time 
reaction_time<-
  year_2%>% select(flkr_scr_meanrt)

#making reaction time numeric 
reaction_time<-as.numeric(unlist(reaction_time))

#subsetting only T_2 for flanker uncorrected 

flanker_uncor_T_2<- flanker_uncorrected %>% filter(eventname=='T_2')

#subsetting coumn with scores 
flanker_uncor_T_2_scores<-flanker_uncor_T_2%>% select(flanker_uncorrected)
                                                      

#I get a bunch of falses or NA's, the subjects do not match between the 2 datasets 
scale(proportion_correct)+ scale(reaction_time)==flanker_uncor_T_2_scores


#maybe I should do it for a few subjects/manually?
#I will try to join the datasets by subjects and then seperate them again? I dont know what to do, maybe read flanker 

#as.numeric=> converts an object into numeric data type (check what it was before)
#scale=> Scaling is a technique for comparing data that isn’t measured in the same way. The normalizing of a dataset using the mean value and standard deviation
#I can't find a difference between standardizing and the scale function


