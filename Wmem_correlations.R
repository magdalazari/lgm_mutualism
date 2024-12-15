#######Comparing Working memory measures 


#comparing 1.rate of correct responses 2.RT of 0back 3.RT of 2 back to T_1 of standard measures (NIH toolbox List and Ravens WISC)


#########CHECKING CORRELATIONS BETWEEN 3 FMRIS TO LIST 

#Rate of correct responses to List 

#Subsetting list 
list_uncorrected<-
  data_all%>%
  select(matches('src_subject_id|eventname|nihtbx_list_uncorrected'))

#long to wide list 
list_uncorrected_wide<-reshape(list_uncorrected, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

cor_rate_of_correct_list<-merge(rate_of_correct_wide, list_uncorrected_wide)
cor_rate_of_correct_list<-cor_rate_of_correct_list[,2:7] 
cor(cor_rate_of_correct_list, use='pairwise.complete.obs')

#results: T_1 0.36 correlation between LIST and RATE OF CORRECT RESPONSES 

#Reaction time in 0 back to List 

#subset reaction time for 0 
reaction_time_0<- 
  working_memory_all%>% 
  select(matches("src_subject_id|eventname|tfmri_nb_all_beh_c0b_mrt"))

#make wide 
reaction_time_0_wide<-reshape(reaction_time_0, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

cor_reaction_time_0_list<-merge(reaction_time_0_wide, list_uncorrected_wide)
cor_reaction_time_0_list<-cor_reaction_time_0_list[,2:7] 
cor(cor_reaction_time_0_list, use='pairwise.complete.obs')

#results: T1 -0.2 correlation between LIST and AVERAGE REACTION TIME in 0 back (similar in t_4)

#Reaction time in 2 back to List 

#subset reaction time for 2 
reaction_time_2<- 
  working_memory_all%>% 
  select(matches("src_subject_id|eventname|tfmri_nb_all_beh_c2b_mrt"))

#make wide 
reaction_time_2_wide<-reshape(reaction_time_2, idvar = "src_subject_id", timevar = "eventname", direction = "wide")

cor_reaction_time_2_list<-merge(reaction_time_2_wide, list_uncorrected_wide)
cor_reaction_time_2_list<-cor_reaction_time_2_list[,2:7] 
cor(cor_reaction_time_2_list, use='pairwise.complete.obs')

#results: 0.02 correlation between LIST and REACTION TIME IN 2 BACK, -0.09 in t4

#########CHECKING CORRELATIONS BETWEEN 3 FMRIS TO MATRIX 

wisc_all<-read.csv('nc_y_wisc.csv')

#subsetting total raw score
wisc_matrices<-
  wisc_all%>%
  select(matches('src_subject_id|eventname|pea_wiscv_trs'))

wisc_matrices_wide<-reshape(wisc_matrices, idvar = "src_subject_id", timevar = "eventname", direction = "wide")


#RATE of correct responses to Matrices
cor_rate_of_correct_matrices<-merge(rate_of_correct_wide, wisc_matrices_wide)
cor_rate_of_correct_matrices<-cor_rate_of_correct_matrices[,2:5] 
cor(cor_rate_of_correct_matrices, use='pairwise.complete.obs')

#results: 0.36 correlation between RATE OF CORRECT and MATRICES  

#Reaction time in 0 back to matrices 

cor_reaction_time_0_matrices<-merge(reaction_time_0_wide, wisc_matrices_wide)
cor_reaction_time_0_matrices<-cor_reaction_time_0_matrices[,2:5] 
cor(cor_reaction_time_0_matrices, use='pairwise.complete.obs')

#results: -0.15 between MATRICES and reaction time in 0 back 

#Reaction time in 2 back to matrices 

cor_reaction_time_2_matrices<-merge(reaction_time_2_wide, wisc_matrices_wide)
cor_reaction_time_2_matrices<-cor_reaction_time_2_matrices[,2:5] 
cor(cor_reaction_time_2_matrices, use='pairwise.complete.obs')

#results: 0.08 between MATRICES and reaction time 2 back 


