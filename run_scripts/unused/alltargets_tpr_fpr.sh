#!/bin/bash

# Generates tpr_fpr files

Rscript run_scripts/alltargets_OLS.R
Rscript run_scripts/alltargets_POLS.R
Rscript run_scripts/alltargets_DPOLS.R
# Rscript run_scripts/alltargets_ICP.R