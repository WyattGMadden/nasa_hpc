#!/bin/bash
#SBATCH --job-name=bayesgrmfullgrid     # create a short name for your job
#SBATCH --nodes=1                  # node count
#SBATCH --partition=week-long-cpu
#SBATCH --ntasks=1                 # total number of tasks across all nodes
#SBATCH --cpus-per-task=1          # cpu-cores per task (>1 if multi-threaded tasks)
#SBATCH --mem=4G                   # total memory per node (4 GB per cpu-core is default)
#SBATCH -o grm-just-ca-%A_%a.out  # output file format (%A is jobID, %a is task index)

module purge
module load R

Rscript -e "source('grid_data_fit_initial.R'); full_grid_run(matern.nu=as.numeric(Sys.getenv('PARAM1')), cv=Sys.getenv('PARAM2'))"

