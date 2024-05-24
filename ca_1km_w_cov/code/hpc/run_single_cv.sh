#!/bin/bash
#SBATCH --job-name=_<oCAo>_
#SBATCH --nodes=1                  # node count
#SBATCH --partition=chang
#SBATCH --ntasks=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1          # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=8G                   # total memory per node (4 GB per cpu-core is default)
#SBATCH --array=1-10
#SBATCH -o ../../output/logs/cv_logs/cv-ca-%A_%a.out  # output file format (%A is jobID, %a is task index)

module purge
module load R

Rscript -e "print('${SLURM_ARRAY_TASK_ID}'); source('full_fit.R'); full_cv(matern.nu=as.numeric(Sys.getenv('PARAM1')), cv=Sys.getenv('PARAM2'), fit.i=as.integer('${SLURM_ARRAY_TASK_ID}'))"

