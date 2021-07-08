#!/bin/bash

# The three steps must be run in this order, but any of the steps can be
# run in any order (e.g you can split the alltargets_data.sh script into
# as many pieces as you have cores, by splitting the alltargets_params
# files in more pieces).

## 1. First the data must be generated.

Rscript run_scripts/alltargets_data 1 2 3 4
Rscript run_scripts/singletargets_data 1 2 3 4

## 2. Now the tpr_fpr files must be generated.

sh run_scripts/alltargets_tpr_fpr.sh
sh run_scripts/singletargets_tpr_fpr.sh

## 3. Add ROC points and AUC points.

Rscript run_scripts/alltargets_AUC.R
Rscript run_scripts/singletargets_AUC.R

## 4. Save plots (TODO)