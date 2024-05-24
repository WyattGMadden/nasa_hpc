#!/bin/bash

tail -n +2 ~/../../projects/hhchang/wmadden/nasa_hpc/full_us_12km/data/created/job_params.csv | while IFS=',' read -r param1 param2
do
    sbatch --export=PARAM1="$param1",PARAM2="$param2" run_single_cv.sh
done

