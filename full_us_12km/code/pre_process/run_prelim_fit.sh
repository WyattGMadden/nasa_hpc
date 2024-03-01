#!/bin/bash
#SBATCH --job-name=_oOUSOo_     # create a short name for your job
#SBATCH --nodes=1                  # node count
#SBATCH --partition=chang
#SBATCH --ntasks=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1          # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --array=1-3
#SBATCH --mem=32                   # total memory per node (4 GB per cpu-core is default)
#SBATCH -o pre-process-full-us-%A_%a.out  # output file format (%A is jobID, %a is task index)
module purge
module load R

Rscript -e "source('prelim_fit.R'); fit_mat(${SLURM_ARRAY_TASK_ID})"
