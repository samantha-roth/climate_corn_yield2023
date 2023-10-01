# climate_corn_yield2023

Create matrix of covariates from METDATA and NISS data (folder: yield/data_prep)

1. create_df_metdata.R and create_timeloc_df.R
2. pwl_model_setup.R
3. normalize_pwl_data.R
4. reorder_by_year.R
5. fpw_multyr_generateSamples.R

Create matrix of covariates from TopoWX and NISS data (folder: yield/data_prep/TopoWX)
1. for i in 1986 to 2016, createTmats_i.R
2. combineTmatsAllYrs.R
3. getGS_TWX.R
4. create_df_TWX_gs2.R and create_prpr2_TWX_gs2.R 
5. normalize_pwl_TWX2.R
6. reorder_by_year_TWX2.R
7. computeWtTsums2.R
8. fpw_multyr_generateSamples_TWX2.R
