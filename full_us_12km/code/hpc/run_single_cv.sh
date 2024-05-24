#!/bin/bash
#SBATCH --job-name=_oOUSOo_
#SBATCH --nodes=1
#SBATCH --partition=chang
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=32gb
#SBATCH --array=1-10
#SBATCH -o ../../output/logs/cv_logs/grm-cv-us-%A_%a.out
module purge
module load R

Rscript -e "print('${SLURM_ARRAY_TASK_ID}'); source('full_data_fit.R'); full_us_cv(matern.nu=as.numeric(Sys.getenv('PARAM1')), cv=Sys.getenv('PARAM2'), fit.i=as.integer('${SLURM_ARRAY_TASK_ID}'))"
