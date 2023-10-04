#!/bin/bash

tail -n +2 ../../output/data/job_params.csv | while IFS=',' read -r param1 param2
do
    sbatch --export=PARAM1="$param1",PARAM2="$param2" run_single_job.sh
done

