#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N Tfold_HA2N3
#$ -cwd                  
#$ -l h_rt=500:00:00 
#$ -l h_vmem=8G
#$ -t 5-7
#$ -e sub10fold_HA2N3.e
#$ -o sub10fold_HA2N3.o

#  These options are:
#  job name: -N
#  use the current working directory: -cwd
#  runtime limit of 48h: -l h_rt
#  memory limit of 1 Gbyte: -l h_vmem
# Initialise the environment modules
 source ~/.bashrc
# source /etc/profile.d/modules.sh
# module load anaconda
# module load python3
# conda activate msc-ml
 start_ml
# source activate /home/s2442072/Christina/msc-ml/ml_zxy

python3 10fold3.py ${SGE_TASK_ID}.HA2N.csv

