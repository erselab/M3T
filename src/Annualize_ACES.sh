#!/bin/bash

#SBATCH --partition=extended-28core
#SBATCH --job-name=Annualizing_p1
#SBATCH --output=Annualizing_12.out
#SBATCH --error=Annualizing_12.err
#SBATCH --exclusive
#SBATCH -N 2
#SBATCH --ntasks-per-node=1
#SBATCH --time=6-10:00:00
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=kristian.hajny@stonybrook.edu

module purge
module load slurm R netcdf gdal gnu-parallel

# the --exclusive to srun makes srun use distinct CPUs for each job step
# -N1 -n1 allocates a single core to each task
#jobrun_code="srun --exclusive -N1 -n1"

# --delay .2 prevents overloading the controlling node
# -j is the number of tasks parallel runs
# --joblog makes parallel create a log of tasks that it has already run
# --resume makes parallel use the joblog to resume from where it has left off
# the combination of --joblog and --resume allow jobs to be resubmitted if
# necessary and continue from where they left off
# if resume is activated, the file runtask.log has to be removed in order to run a
# different batch of task or it will think that they have finished already
# I have that version commented here but you could just uncomment it
# parallel="parallel --delay .2 -j 2 --joblog runtask.log --resume"
#parallel_code="parallel --delay 0.2 -j 2"

#sector_list=(2 3 7 8 10 11)
#sector_list=(2 3)
#sector_list=(7 8)
#sector_list=(10 11)
#sector_list=(2)

# this runs the parallel command we want
# in this case, we are running a script named /path/to/scripts/runsinglecorejob.sh
# parallel uses ::: to separate options. Here ${seq 1 1 2} is a sequence from 1 by 1 to 2
# so parallel will run the command passing the numbers 1 through 2
# via argument {1}
parallel --delay 0.2 -j 2 "Rscript /gpfs/projects/ShepsonGroup/khajny/Scripts/Annualize_Vulcan.R {1}" ::: $(seq 1 1 2)
#parallel --delay 0.2 -j 2 "Rscript /gpfs/projects/ShepsonGroup/khajny/Scripts/Annualize_Vulcan.R {1}" ::: $(seq 3 1 4)



