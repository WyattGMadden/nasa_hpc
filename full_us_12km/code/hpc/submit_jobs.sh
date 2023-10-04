#!/bin/bash

tail -n +2 ../data/job_params_full_grid.csv | while IFS=',' read -r param1 param2
do
    sbatch --export=PARAM1="$param1",PARAM2="$param2" run_single_job.sh
done

