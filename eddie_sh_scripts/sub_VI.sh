#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N VI-HA1S
#$ -cwd                  
#$ -l h_rt=15:00:00 
#$ -l h_vmem=6G
#$ -t 1-1000
#$ -e sub_HA1S_reVI.e
#$ -o sub_HA1S_reVI.o

#  These options are:
#  job name: -N
#  use the current working directory: -cwd
#  runtime limit of 5 minutes: -l h_rt
#  memory limit of 1 Gbyte: -l h_vmem
# Initialise the environment modules
 source ~/.bashrc
# source /etc/profile.d/modules.sh
# module load anaconda
# module load python3
# conda activate msc-ml
 start_ml
# source activate /home/s2442072/Christina/msc-ml/ml_zxy

python3  HA1S_VI_repair.py ${SGE_TASK_ID}.HA1S.csv

