#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N ffold-HA1S
#$ -cwd                  
#$ -l h_rt=48:00:00 
#$ -l h_vmem=8G
#$ -t 1-1000
#$ -e subkfold_newHA1.e
#$ -o subkfold_newHA1.o

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

python3 kfold_new.py ${SGE_TASK_ID}.HA1S.csv

