#!/bin/bash

# The three steps must be run in this order, but any of the steps can be
# run in any order (e.g you can split the alltargets_data.sh script into
# as many pieces as you have cores).

## 1. First the data must be generated.

Rscript run_scripts/alltargets_data 1 2 3 4

## 2. Now the tpr_fpr files must be generated.
# Again these can be run in any order.

sh run_scripts/alltargets_tpr_fpr.sh

## 3. Finally add ROC points and AUC points.

Rscript tools/alltargets_AUC.R