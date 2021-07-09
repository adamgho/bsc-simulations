#!/bin/bash

# Generates tpr_fpr files.

Rscript run_scripts/singletargets_OLS.R
Rscript run_scripts/singletargets_POLS.R
Rscript run_scripts/singletargets_DPOLS.R
Rscript run_scripts/singletargets_ICP.R