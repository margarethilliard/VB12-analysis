## Scripts for analyses and visualizations in FL100 VB12 x microbiome project

### Main text figures:
- Figure 1 A: sorted_bars.R
- Figure 1 B: venn_diagrams.R
- Figure 1 C-D: boxplots.R
- Figure 2 A-B: boxplots.R
- Figure 2 C: partial_correlations.R
- Figure 3 A-B: pathway_completion_plot_medium_quality_MAGs.R
- Figure 4-5: GLM_fecal_SCFA_analysis.R
- Figure 6 A: SCFA_model_performance_plots.R
- Figure 6 B: pathway_shap_plots.R 

### Supplemental materials: 
- Supplemental Table 1: participant_characteristics.R
- Supplemental Table 2: participant_characteristics.R
- Supplemental Table 3: participant_characteristics.R
- Supplemental Table 6: GLM_fecal_SCFA_analysis.R
- Supplemental Table 7: GLM_plasma_SCFA_analysis.R 
- Supplemental Table 8: alpha_diversity_analysis.R & beta_diversity_analysis.R
- Supplemental Table 9: supplementary_shap_table.R
- Supplemental Figure 2: partial_correlations.R
- Supplemental Figure 3: methionine_B12_correlations.R
- Supplemental Figure 4: SCFA_model_performance_plots.R

### Notes:
- Most scripts source the merged and cleaned data frames using "get_data.R" which is a great way to save time, be consistent, and make the analysis slightly more portable. Highly recommend.
- R analyses and visualizations were run locally on a MacBook Pro (Apple M3 Max chip). 
- CONSORT diagram was made manually in PowerPoint, where the format was largely derived from Sarah Blecksmith's paper [linked here](https://pubmed.ncbi.nlm.nih.gov/40902732/)
- Supplemental Table 3 was modified from Iva Veseli's Github repo [linked here](https://github.com/ivagljiva/custom_biosynthesis_modules/blob/main/USER_MODULES/modules/B12_01)
