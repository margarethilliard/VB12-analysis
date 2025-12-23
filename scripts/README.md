## Scripts for analyses and visualizations in FL100 VB12 x microbiome project

### Main Text:
- Figure 1 A-D: boxplots.R
- Figure 1 E: partial_and_kendall_correlations.R
- Figure 2 A-B: pathway_completion_plot.R
- Figure 3-4: GLM_SCFA_analysis.R
- Figure 5: plasma_B12_model_performance_plot.R
- Figure 6: plasma_B12_shap_plots.R
- Figure 7: pathway_shap_plots.R & SCFA_model_performance_plots.R

### Supplemental Materials: 
- Supplemental Table 1: participant_characteristics.R
- Supplemental Table 2: alpha_diversity_analysis.R & beta_diversity_analysis.R
- Supplemental Figure 2-3: partial_and_kendall_correlations.R
- Supplemental Figure 4: venn_diagram.R
- Supplemental Figure 5: differential_abundance_analysis.R
- Supplemental Figure 6-7: plasma_B12_shap_plots.R
- Supplemental Figure 8: SCFA_model_performance_plots.R

### Notes:
- Most scripts source the merged and cleaned dataframes using "get_data.R" which is a great way to save time, be consistent, and make the analysis slightly more portable. Highly recommend.
- R analyses and visualizations were run locally on a MacBook Pro (Apple M3 Max chip).
- CONSORT diagram was made manually in PowerPoint. 
