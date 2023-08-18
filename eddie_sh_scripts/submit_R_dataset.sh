#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N CNN_datasets              
#$ -cwd                  
#$ -l h_rt=24:00:00 
#$ -l h_vmem=4G
#$ -t 1-1000
#$ -e data07Jul.e
#$ -o data07Jul.o

#  These options are:
#  job name: -N
#  use the current working directory: -cwd
#  runtime limit of 5 minutes: -l h_rt
#  memory limit of 1 Gbyte: -l h_vmem
# Initialise the environment modules
# source ~/.bashrc
# source /etc/profile.d/modules.sh
# module load anaconda
# source R
. /etc/profile.d/modules.sh
module load R/3.5.3
Rscript dataset.R ${SGE_TASK_ID}
