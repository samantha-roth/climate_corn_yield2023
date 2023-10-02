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

Fit CI model using METDATA temperatures (folder: yield/Bayesian/CI)
1. SRMeanModel.R (fit model)
2. CI_preds.R (make predictions)
3. CI_spatialresults.R

Fit CI model using TopoWX temperatures (folder: yield/Bayesian/CI)
1. SRMeanModel_TWX.R (fit model)
2. CI_preds_TWX.R (make predictions)
3. CI_spatialresultsTWX.R

Fit PICAR models (folder: yield/PICAR/timeConstant/noCIs)
1. AIC_PICAR_model.R and BIC_PICAR_model.R
2. AIC_PICAR_preds.R and BIC_PICAR_preds.R (make predictions)

Compare performance of PICAR to CI model (folder: yield/compareModels)
1. compareRMSE.R
2. computeWAIC.R, computeWAIC_AICPICAR.R, computeWAIC_BICPICAR.R
3. compareWAICs.R
4. plotPerformanceComparisons.R
5. comparePosteriors.R

Compare performance of CI model with METDATA TO CI model with TopoWX data (folder: yield/compareModels/compareMETtoTWX)
1. computeWAIC_TWX.R
2. comparePosteriors.R

Detrend observations
1. Based on CI model: SRMeanModel_dt.R (folder: yield/Bayesian/CI)
2. Based on BIC model: BIC_PICAR_model_dt.R (folder: yield/PICAR/timeConstant/noCIs)

Process climate projections (folder: yield/data_prep)
1. processFutureNoDelta.R via processFutureNoDelta.pbs
2. processOtherFutureInfo.R
3. processCornData.R

Evaluate future yield projections (folder: yield/prediction)

1. CIModel_predictions.R via CIModel_predictions.pbs and  BICPICARModel_predictions.R via BICPICARModel_predictions.pbs
2. CIModel_thinnedpreds.R via CIModel_thinnedpreds.pbs and BICPICARModel_thinnedpreds.R via BICPICARModel_thinnedpreds.pbs
3. CI_getCMIP5ParQs.R via CI_getCMIP5ParQs.pbs and BICPICAR_getCMIP5ParQs.R via BICPICAR_getCMIP5ParQs.pbs
4. plotFutureYearMeanswUncertainty.R
5. BICspatialPlotFuturePreds.R and CIspatialPlotFuturePreds.R

