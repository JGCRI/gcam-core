#!/bin/zsh
#SBATCH -t 48:00:00
#SBATCH -N 1
 
job=$SLURM_JOB_NAME
 
 
echo 'Library config:'
ldd ./gcam.exe
 
date
time ./gcam.exe -Cconfiguration_$job.xml -Llog_conf.xml
date