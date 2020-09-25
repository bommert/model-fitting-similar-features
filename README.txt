# Analyses on Simulated Data
1. sd_experiments.R: Run on high performance compute cluster. 
   You need to submit the jobs yourself with specifications suitable for your compute cluster.
   The following files must be in the working directory:
    - sd_data_generation.R
	- sd_false_features.R
	- RLearner_classif_L0Learn.R
2. sd_results.R: Run on high performance compute cluster after all computations have finished. 
3. sd_tuning.R: Run to perform the tuning. 
   The following file must be in the working directory:
    - tuning_funs.R
	- sd_results.RData (created by sd_results.R)
4. sd_evaluation.R: Run to obtain the plots.
   The following file must be in the working directory:
    - sd_results_tuned.RData (created by sd_tuning.R)

# Analyses on Real Data
1. rd_datasets.R: Run to download and preprocess the datasets.
2. rd_experiments.R: Run on high performance compute cluster.
   You need to submit the jobs yourself with specifications suitable for your compute cluster.
   The following files must be in the working directory:
	- RLearner_classif_L0Learn.R
	- rins.RData (created by datasets.R)
    - simmats.RData (created by datasets.R)
3. rd_results.R: Run on high performance compute cluster after all computations have finished. 
4. rd_tuning.R: Run to perform the tuning. 
   The following files must be in the working directory:
    - tuning_funs.R
	- rd_results.RData (created by rd_results.R)
5. rd_evaluation.R: Run to obtain the plots.
   The following file must be in the working directory:
    - rd_results_tuned.RData (created by rd_tuning.R)

