#!/bin/bash
#SBATCH --job-name=_oOUSOo_
#SBATCH --nodes=1
#SBATCH --partition=chang
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16gb
#SBATCH --array=1-10
#SBATCH -o ../../output/logs/grm-full-us-%A_%a.out
module purge
module load R

Rscript -e "print(.libPaths()); source('full_data_fit.R'); full_us_cv(matern.nu=as.numeric(Sys.getenv('PARAM1')), cv=Sys.getenv('PARAM2'), fit.id=as.integer('${SLURM_ARRAY_TASK_ID}')"

