#!/bin/bash

## Adds AUC for both alltargets and singletargets based on tpr_fpr files

Rscript run_scripts/alltargets_AUC.R
Rscript run_scripts/singletargets_AUC.R
