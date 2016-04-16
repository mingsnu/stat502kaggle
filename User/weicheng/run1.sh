#!/bin/bash

# Instructions for new Condo Cluster users:
#  To use this script:
#   1) Save this script as a file named myscript on condo
#   2) On condo, Issue                   
#       qsub  myscript    to submit the job 
#        Use qstat -a to see job status, 
#         Use qdel jobname to delete one of your jobs
#         jobnames are of the form 1234.condo

###########################################
# Output goes to file BATCH_OUTPUT.
# Error output goes to file BATCH_ERRORS.
# If you want the output to go to another file, change BATCH_OUTPUT 
# or BATCH_ERRORS in the following lines to the full path of that file. 

#PBS  -o BATCH_OUTPUT1 
#PBS  -e BATCH_ERRORS1

#PBS -lnodes=1:ppn=16:compute,walltime=12:00:00

# Change to directory from which qsub command was issued 
cd $PBS_O_WORKDIR

Rscript xgboost_condo1.R