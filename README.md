Data from: 
A. ABCD neurocognition (Shared\abcd-data-release-5.1\core\neurocognition\nc_y_nihtb) -> NIH Toolbox (5 tests administered at baseline, 2nd and 4th year, administered in English). Included: 
1. Picture Vocabulary (component of the Crystallized Composite Score)
2. Flanker Inhibitory Control & Attention (component of the Fluid Composite Score) (remote assessments used a replicated Flanker task -Inquisit platform-)
3. Picture Sequence Memory (component of the Fluid Composite Score)
4. Pattern Comparison Processing Speed (component of the Fluid Composite Score)
5. Oral Reading Recognition (component of the Crystallized Composite Score)

B. ABCD imaging (Shared\abcd-data-release-5.1\core\imaging\mri_y_tfmr_nback_beh_workingmem- tfmri_nb_all_beh_ctotal_rate)
Rate of correct responses in fMRI n-back working memory task 

Number of participants included/event: baseline: 11868, 2-year follow-up:	10973, 4-year follow-up: 4754.

Notes on tasks:
1. NIHToolbox: 6 types of scores (ABCD recommends using uncorrected or raw scores for longitudinal analyses): 
-age corrected (compares the score of the test-taker to those in the NIH Toolbox nationally representative normative sample at the same age).
-uncorrected (uses a standard score metric. It compares the performance of the test-taker to those in the entire NIH Toolbox nationally representative normative sample, regardless of age/any other variable. May be of interest when monitoring performance over time).
-raw (not available for 2/5 tests, picture vocabulary and reading). 
-theta (calculated for each participant; it represents their relative overall ability/performance. Very similar to a z-score.
-computed(They only explain it for the Flanker test (the Flanker Computed score provides a way of gauging raw improvement or decline from Time 1 to Time 2). 
-fully-corrected T-score (compare the score of the test-taker to those in the NIH Toolbox nationally representative normative sample, while adjusting for demographic variables (include age, gender, race/ethnicity,educational attainment) collected during the NIH Toolbox national norming study. They provide this score to allow for comparison within a narrower grouping).

2. The Fluid Intelligence and Total Composite scores were not calculated for every wave because they include 2 tests that were not followed up.

2. Flanker: -Remote assessments in the 2-year and 4-year follow-up protocols used a Flanker task using the Inquisit system from Millisecond. This task was designed to mimic the NIH Toolbox Flanker task as closely as possible. Not all assessments happened remote/hybrid for the last 2 waves, so some subjects have one version and some the other. 
- Low correlations between raw and uncorrected Flanker: Raw scores represent just accuracy, uncorrected include rt for people scoring more than 80% (to manage ceiling eff), mentioned in manual and obvious when plotted, since 80% of 40 trials = 32 correct responses.
If 20 correct responses are "guaranteed" (not included in the raw score) the remaining 12 (32 - 20) becomes the point at which the uncorrected score starts incorporating reaction times for accuracy higher than 80%, as participants have a baseline of 20 correct answers, so achieving 80% accuracy (32 correct) requires 12 additional correct responses, and this cutoff reflects on the graph, where 12 on the raw score axis aligns with the transition where reaction time likely starts influencing scores, Once you achieve the 12 additional correct answers (so hit 80% accuracy), reaction times start being factored into the score (which adds new types of variation).

-In T_2 follow-up assessment it was not possible to administer NIH Toolbox Pattern Comparison Processing Speed
task remotely and it was not administered -> so people who are remote did not have a score in pattern T2


Data cleaning steps/notes:

1. Only 3 tests had raw scores available, so checked for the within test correlations between raw and uncorrected (the 2 types that ABCD recommends) at T_1.
results: picture: 0.97, pattern: 0.99, flanker: 0.35 


3. Compared 3 wm tasks (tfmri_nb_all_beh_ctotal_rate(=rate of correct responses), tfmri_nb_all_beh_c0b_mrt (=Average reaction time for all correct responses to 0 back stimuli during run 1 and run 2), tfmri_nb_all_beh_c2b_mrt (=Average reaction time for all correct responses to 2 back stimuli during run 1 and run 2) to List from NIHtb and ravens matrices and the highest correlations were between rate of correct response for both list and ravens, (0.36 for T_1), so this measure was selected.
   
4. NDAR_INVA31C7WYJ was an outlier (180) in T_2 reading, but the rest of their scores looked normal, so I replaced with NA. 


Model specifiication and fit:

1. Specified a linear and basis model for each cognitive domain seperately to select ideal (first just an intercept and slope, free err var).

2. Basis was better for all except Picture Seq Memory. 

3. Having only 3 timepoints means that the basis model has 0 degrees of freedom, making it saturated/just-identified. To make model comparison possible, we constrained the residual error variances within each domain for both linear and basis (*a for the 3 timepoints) ("growth" by default uniquely estimates each one) and got 2 df more.

4. Compared Picture seq linear and basis again with freed error var and basis was better. 

5. Due to negative value in the v-cov of working memory (H case, the standardized covariance between int and slope was below -1), a. rescaled (*100) b.freed error variance.

6. Anova() showed that basis was better for all of the domains (free error var for 2/6 domains), so worked with: 6 domains, all basis models, all error variances fixed except for working memory and picture. 

8. Used predict() to extract intercept and slope estimates for each participant for each cognitive domain/model seperately, joiined them into a data frame and correlated them. Result: very low correlations between slope-slope for every pair of tasks (the int-int were higher). To better understand what is happening (we expected higher correlations), we compared the basis model (with constrained error var) to a basis model that in addition had the slope variance fixed to 0 (testing for interindividual differences in the slope). Anova() showed that the one with the freely estimated slope variance was better for all domains, so the slopes interindividual diff were meaningful. Less variance in the slope when variance is unconstrained.

9. Next, to examine what is going on we combined models from individual domains in one (tried fluid-fluid and cryst-cryst: flanker-working memory, picvocab-reading), aiming to combine all 6 domains in 1 model. 

Problems after that: 

1) lavaan std.all and predict() correlations are very different, esp for slopes. So we tried (troubleshooting script):
- Reading and Picvocav: made both linear and basis model, keeping only complete cases. Compared the std.all estimations to the individual predict estimations and they were still different. 

3) Covariance matrix is not positive for all 6 domains 
We tried: including only complete cases, removing domains (if we remove wm and pattern they seem okay)

I fixed the slope variance in 0 and then 1 for 1 task and the model with the freely estimated is significantly better in both cases (within task)
 freely estimated slope variance is better for all tasks (that means there is meaningful interindividual differences in trajectories across cognitive tasks)


At some point tried a model with correlated error variances within time points (picvocab_T1~~flanker_T1) but there are better ways to approach it probably.

 ###########################################################
 Before selecting A and B, I tried:
 -the Little man task was also administered in baseline, 2nd and 4th year, but: when administered in the baseline assessment a customized program designed by ABCD was used, whereas in (all) the 2-year and 4-year follow-up assessments a task presented in the Inquisit system from Millisecond was used. Testing: not great correlation between timepoints, esp 1-2 (.016), but 1-3 was good (.47). Generally weird to plot.


