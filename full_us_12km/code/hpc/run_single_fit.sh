#!/bin/bash
#SBATCH --job-name=_oOUSOo_     # create a short name for your job
#SBATCH --nodes=1                  # node count
#SBATCH --partition=chang
#SBATCH --ntasks=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1          # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=32gb                   # total memory per node (4 GB per cpu-core is default)
#SBATCH -o ../../output/logs/fit_logs/grm-full-us-%A_%a.out  # output file format (%A is jobID, %a is task index)
module purge
module load R

Rscript -e "print(.libPaths()); source('full_data_fit.R'); full_us_fit(matern.nu=as.numeric(Sys.getenv('PARAM1')), cv=Sys.getenv('PARAM2'))"

