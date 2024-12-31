Data from: 
A. ABCD neurocognition (Shared\abcd-data-release-5.1\core\neurocognition\nc_y_nihtb) -> NIH Toolbox (5 tests administered at baseline, 2nd and 4th year, all of them administered in English). Included: 
1. Picture Vocabulary (component of the Crystallized Composite Score)
2. Flanker Inhibitory Control & Attention (component of the Fluid Composite Score) (remote assessments used a replicated Flanker task -Inquisit platform-)
3. Picture Sequence Memory (component of the Fluid Composite Score)
4. Pattern Comparison Processing Speed (component of the Fluid Composite Score)
5. Oral Reading Recognition (component of the Crystallized Composite Score)

B. ABCD imaging (Shared\abcd-data-release-5.1\core\imaging\mri_y_tfmr_nback_beh_workingmem- tfmri_nb_all_beh_ctotal_rate)
Rate of correct responses in fMRI working memory task 


Number of participants included/event: baseline: 11868, 2-year follow-up:	10973, 4-year follow-up: 4754.


Notes:
-the Little man task was also administered in baseline, 2nd and 4th year, but: when administered in the baseline assessment a customized program designed by ABCD was used, whereas in (all) the 2-year and 4-year follow-up assessments a task presented in the Inquisit system from Millisecond was used. Testing: not great correlation between timepoints, esp 1-2 (.016), but 1-3 was good (.47). Generally weird to plot.

-Remote assessments in the 2-year and 4-year follow-up protocols used a Flanker task using the Inquisit system from Millisecond. This task was designed to mimic the NIH Toolbox Flanker task as closely as possible.


Data cleaning steps/notes:
1. Only 3 tests had raw scores available, so checked for the correlations between raw and uncorrected (the 2 types that ABCD recommends) at T_1.
results: picture: 0.97, pattern: 0.99, flanker: 0.35 (because raw uses only accuray, uncorrected includes rt for people scoring more than 80%, mentioned in manual and obvious when plotted, since 80% of 40 trials = 32 correct responses.
If 20 correct responses are "guaranteed" (not included in the raw score) the remaining 12 (32 - 20) becomes the point at which the uncorrected score starts incorporating reaction times for accuracy higher than 80%, as participants have a baseline of 20 correct answers, so achieving 80% accuracy (32 correct) requires 12 additional correct responses, and this cutoff reflects on the graph, where 12 on the raw score axis aligns with the transition where reaction time likely starts influencing scores, Once you achieve the 12 additional correct answers (so hit 80% accuracy), reaction times start being factored into the score (which adds new types of variation)

2. Compared 3 wm tasks (tfmri_nb_all_beh_ctotal_rate(=rate of correct responses), tfmri_nb_all_beh_c0b_mrt (=Average reaction time for all correct responses to 0 back stimuli during run 1 and run 2), tfmri_nb_all_beh_c2b_mrt (=Average reaction time for all correct responses to 2 back stimuli during run 1 and run 2) to List from NIHtb and ravens matrices and the highest correlations were between rate of correct response for both list and WISC (0.36), so this measure was selected.
3. NDAR_INVA31C7WYJ was an outlier (180) in T_2 reading, but the rest of their scores looked normal, so I replaced with NA. 


Model specifiication and fit (for constrained):
Assuming data was missing completely at random so we use FIML to impute missing data.
Having only 3 timepoints means that the basis model has 0 degrees of freedom, the model is saturated/just-identified. To make model comparison possible, we constrained the residual error variances (*a) for the 3 timepoints (obs variables) ("growth" by default uniquely estimates each one) and got 2 df more. 

Picture vocabulary 
Linear: CFI: 0.998  TLI: 0.998   RMSEA: 0.029   SRMR: 0.019
Basis: CFI: 0.998  TLI: 0.998   RMSEA: 0.028   SRMR: 0.018
anova: basis has lower AIC and BIC and significant χ2 diff value, which means that the basis model fits significantly better and the extra parameter estimation is worth it.

Basis is better (indices and comparison): mean score at baseline 
mean rate of change 
between-person variability at baseline 
between-person variability in rate of change 
how does baseline associate with rate of development?
how much of the variance is not explained by the latent factor?

Flanker 
Linear: CFI: 0.904  TLI: 0.904   RMSEA: 0.085   SRMR: 0.036
Basis: CFI: 0.969  TLI: 0.953   RMSEA: 0.059   SRMR: 0.035
anova: basis has lower AIC and BIC and fits significantly better 

Basis is better (indices and comparison) 

Pattern
Linear: CFI: 0.966  TLI: 0.966   RMSEA: 0.057   SRMR: 0.065
Basis: CFI: 0.997  TLI: 0.996   RMSEA: 0.020   SRMR: 0.010
anova: basis has lower AIC and BIC and fits significantly better 

Basis is better (indices and comparison)

Picture
Linear: CFI: 0.884  TLI: 0.884   RMSEA: 0.101   SRMR: 0.059
Basis: CFI: 0.332  TLI: -0.002   RMSEA: 0.297   SRMR: 0.182
anova: linear has lower AIC and BIC, non significant chi sq difference  

Linear ia better (indices are not good, but lower chsq diff. This means that both models fit equally well, so the freely estimated parameters could be fixed and the nested model could be accepted) 

Reading 
Linear: CFI: 0.420  TLI: 0.420   RMSEA: 0.457   SRMR: 0.333
Basis: CFI: 0.995  TLI: 0.992   RMSEA: 0.053   SRMR: 0.026
anova: basis has lower AIC and BIC, basis fits significantly better

Basis is better (much better indices and comparison)

Working memory
The slope variance was negative/0 (Heywood case), so 1. wm*100 2. freed error variance constraints.
Basis is better than linear. 
