#!/bin/bash
#SBATCH --job-name=_<oCAo>_
#SBATCH --nodes=1                  # node count
#SBATCH --partition=chang
#SBATCH --ntasks=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1          # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=8G                   # total memory per node (4 GB per cpu-core is default)
#SBATCH -o ../../output/logs/grm-ca-%A_%a.out  # output file format (%A is jobID, %a is task index)

module purge
module load R

Rscript -e "source('full_fit.R'); full_fit(matern.nu=0.5, cv='ordinary')"

