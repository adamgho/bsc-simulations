#!/bin/bash

# Generate DAGs

Rscript run_scripts/DAGs1000_nx30_nh30_probconnect04.R

# Generate data

Rscript run_scripts/alltargets_data alltargets_params_complete.txt
Rscript run_scripts/singletargets_data singletargets_params_complete.txt

# Process data to generate tpr_fpr files

sh run_scripts/alltargets_tpr_fpr.sh
sh run_scripts/singletargets_tpr_fpr.sh

# Add ROC points and AUC points.

Rscript run_scripts/alltargets_AUC.R
Rscript run_scripts/singletargets_AUC.R

# Save plots

Rscript run_scripts/generate_plots.R