#!/bin/bash
#SBATCH --account=def-mxm
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --mem=8000M
#SBATCH --output=./o1.out
#SBATCH --error=./o1.out
#SBATCH --job-name=build_sbt

module load java/1.8.0_192 
module load sbt 
module load mycplex

sbt pack
