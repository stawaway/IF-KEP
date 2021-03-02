# prerequisites
- gurobi (and gurobipy)
- python 3
- numpy
- pandas
- matplotlib
- sbt
- scala
- java


# Data
The Canadian dataset is contained in the folder CanadianInstances
The US dataset is contained in the folder PortoInstances
The type information files containt information relative to the PRA

The cleaned up graphs are contained in the folder src/data
These are the files used to run the experiments

# CP implementation
The code for the CP implementation is contained in the folder all_solutions

# MIP implementation
The code for the MIP implementation is contained in the folder src


# List of experiments and files
1. Enumerate all solutions
   
   ./all_solutions/propagator/oscar-propagator/target/pack/bin/enum-kep --kep-file input-file [--cycle-limit 3] [--time-limit time] [--LP-prop bool] [--edge-prop bool] [--output-file filename]

2. Enumerate all solutions hierarchical

   ./all_solutions_hierarchical/propagator/oscar-propagator/target/pack/bin/enum-kep --kep-file input-file [--cycle-limit 3] [--time-limit time] [--LP-prop bool] [--edge-prop bool] [--output-file filename]

3. Enumerate all solutions relaxed

   python src/kep_mip_relaxed.py input-file output-file

4. Find projected solutions
  
   python src/experiments/all_solution_cp/preprocess.py [input-file] [output-file]

4. Find the alpha value for instances

   python src/alpha.py --loss=l2

5. Runtime plot (Figure 3)

   ipython src/analysis/plot_performance_profiles.ipynb

6. Number of solutions (table 2)

   python src/experiments/all_solution_cp/num_solutions.py

7. Loss plots (figure 4)

   python src/experiments/all_solution_cp/plot.py

8. Alpha and OPT values for instances (table 3)

   python src/alpha.py

9. New patients after relaxation (Figure 5)

   python src/new_patients.py
