Data from: 
A. ABCD neurocognition (Shared\abcd-data-release-5.1\core\neurocognition\nc_y_nihtb) -> NIH Toolbox (5 tests administered at baseline, 2nd and 4th year, all of them administered in English). Included: 
1. Picture Vocabulary (component of the Crystallized Composite Score)
2. Flanker Inhibitory Control & Attention (component of the Fluid Composite Score) (remote assessments used a replicated Flanker task -Inquisit platform-)
3. Picture Sequence Memory (component of the Fluid Composite Score)
4. Pattern Comparison Processing Speed (component of the Fluid Composite Score)
5. Oral Reading Recognition (component of the Crystallized Composite Score)

B. ABCD imaging (Shared\abcd-data-release-5.1\core\imaging\mri_y_tfmr_nback_beh_workingmem- tfmri_nb_all_beh_ctotal_rate)
Rate of correct responses in fMRI n-back working memory task 

Number of participants included/event: baseline: 11868, 2-year follow-up:	10973, 4-year follow-up: 4754.

Notes:
-Remote assessments in the 2-year and 4-year follow-up protocols used a Flanker task using the Inquisit system from Millisecond. This task was designed to mimic the NIH Toolbox Flanker task as closely as possible.

-In T_2 follow-up assessment it was not possible to administer NIH Toolbox Pattern Comparison Processing Speed
task remotely and we did not administer it. -> so people who are remote did not participate in  T2, bias?


Data cleaning steps/notes:
1. Only 3 tests had raw scores available, so checked for the correlations between raw and uncorrected (the 2 types that ABCD recommends) at T_1.
results: picture: 0.97, pattern: 0.99, flanker: 0.35 (because raw uses only accuray, uncorrected includes rt for people scoring more than 80%, mentioned in manual and obvious when plotted, since 80% of 40 trials = 32 correct responses.

3. Flanker explained: If 20 correct responses are "guaranteed" (not included in the raw score) the remaining 12 (32 - 20) becomes the point at which the uncorrected score starts incorporating reaction times for accuracy higher than 80%, as participants have a baseline of 20 correct answers, so achieving 80% accuracy (32 correct) requires 12 additional correct responses, and this cutoff reflects on the graph, where 12 on the raw score axis aligns with the transition where reaction time likely starts influencing scores, Once you achieve the 12 additional correct answers (so hit 80% accuracy), reaction times start being factored into the score (which adds new types of variation)

3. Compared 3 wm tasks (tfmri_nb_all_beh_ctotal_rate(=rate of correct responses), tfmri_nb_all_beh_c0b_mrt (=Average reaction time for all correct responses to 0 back stimuli during run 1 and run 2), tfmri_nb_all_beh_c2b_mrt (=Average reaction time for all correct responses to 2 back stimuli during run 1 and run 2) to List from NIHtb and ravens matrices and the highest correlations were between rate of correct response for both list and WISC (0.36), so this measure was selected.
   
4. NDAR_INVA31C7WYJ was an outlier (180) in T_2 reading, but the rest of their scores looked normal, so I replaced with NA. 


Model specifiication and fit:

1. Specidied a linear and basis model for each cognitive domain seperately (intercept and slope)

2. Having only 3 timepoints means that the basis model has 0 degrees of freedom, making it saturated/just-identified. To make model comparison possible, we constrained the residual error variances within each domain (*a for the 3 timepoints) ("growth" by default uniquely estimates each one) and got 2 df more.

3. Due to negative value in working memory output, freed error variance. 

4. Anova() showed that basis was better for all of the domains except picture, but after freeing the error variances, model comparison showed that basis was best (no fit indices but model comparison is still informative).

5. So we work with: 6 domains, all basis models, all error variances fixed except for working memory and pattern. 

6. Used predict() to extract intercept and slope estimates for each participant for each cognitive domain/model seperately, joiined them into a data frame and correlated them.

7. Result: very small correlations in slope-slope (the int-int were better). To troubleshoot, we compared the basis model (with constrained error var) to a basis model that in addition had the slope variance fixed to 0 (testing for interindividual differences in the slope). Anova showed that the one with the freely estimated slope variance was better for all domains, so the slopes interindividual diff were meaningful. #much less variance in the slope when variance is unconstrained 

Next problem: 
1) lavaan std.all and predict() correlations are not very high. 



2) Covariance matrix is not positive for all 6 domains 
We tried: including only complete cases, removing domains (if we remove wm and pattern they seem okay)

I fixed the slope variance in 0 and then 1 for 1 task and the model with the freely estimated is significantly better in both cases (within task)
 freely estimated slope variance is better for all tasks (that means there is meaningful interindividual differences in trajectories across cognitive tasks)

 ###########################################################
 Before selecting A and B, I tried:
 -the Little man task was also administered in baseline, 2nd and 4th year, but: when administered in the baseline assessment a customized program designed by ABCD was used, whereas in (all) the 2-year and 4-year follow-up assessments a task presented in the Inquisit system from Millisecond was used. Testing: not great correlation between timepoints, esp 1-2 (.016), but 1-3 was good (.47). Generally weird to plot.


