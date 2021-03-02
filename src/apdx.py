import numpy as np
import pandas as pd
import argparse
import os
from fair_solver import fair_l1_solution, fair_maxmin_solution, fair_l2_solution 
from random_solver import process_solutions


def uniform(solution_file, num_patients):
    with open(solution_file, 'r') as f:
        lines = f.readlines()
        num_solutions = len(lines)

        return [1.0 / num_solutions]


def compute_properties(graph_file, solution_file, num_patients, fair_alg):
    with open(graph_file, 'r') as f:
        pra_idx = f.readline().strip('\n').split('\t').index('%PRA')

        pra_dict = dict()

        for i in range(num_patients):
            pra_dict[i] = float(f.readline().strip('\n').split('\t')[pra_idx])

    solutions = process_solutions(solution_file)
    num_solutions = len(solutions)
    solution_prob_dict = fair_alg(solution_file, num_patients)

    # hard-to-match avg probability
    hard_to_match = [sum([solution_prob_dict[sid] for sid, sval in enumerate(solutions) if i in sval]) for i in pra_dict.keys() if pra_dict[i] >= 0.8]
    num_hard_to_match = len([i for i, val in pra_dict.items() if val >= 0.8])
    hardpct = sum(hard_to_match) / num_hard_to_match if num_hard_to_match > 0 else np.nan 
    
    # easy-to-match avg probability
    easy_to_match = [sum([solution_prob_dict[sid] for sid, sval in enumerate(solutions) if i in sval]) for i in pra_dict.keys() if pra_dict[i] < 0.8]
    num_easy_to_match = len([i for i, val in pra_dict.items() if val < 0.8])
    easypct = sum(easy_to_match) / num_easy_to_match if num_easy_to_match > 0 else np.nan 
    
    return solutions, solution_prob_dict, pra_dict, easypct, hardpct


def patients_in_sols(solutions, solution_prob_dict, pra_dict):
    num_V = len(pra_dict.keys())
    patients = set()
    easy_patients = set()
    hard_patients = set([v for v in pra_dict.keys() if pra_dict[v] >= 0.8])
    val = {}
    easy_val = {}
    hard_val = {}
    length = len(solutions[0])
    
    relaxation = 0
    for sid, solution in enumerate(solutions):
        if len(solution) < length and solution_prob_dict[sid] > 0.0:
            relaxation_ = relaxation
            relaxation = length - len(solution)
            
            if relaxation_ == 0:
                easy_patients = set([v for v in patients if pra_dict[v] < 0.8])

            for i in range(relaxation_, relaxation):
                val[i] = len(patients)
                easy_val[i] = len([v for v in patients if pra_dict[v] < 0.8])
                hard_val[i] = len([v for v in patients if pra_dict[v] >= 0.8])
            length = len(solution)
        if solution_prob_dict[sid] > 0.0:
            patients = patients.union(set(solution))
    for j in range(relaxation, 4):
        val[j] = len(patients)
        easy_val[j] = len([v for v in patients if pra_dict[v] < 0.8])
        hard_val[j] = len([v for v in patients if pra_dict[v] >= 0.8])
    
    structural_val = {i: val[i] - easy_val[0] - hard_val[i] for i in range(0,4)}
    num_structural = len(patients.difference(easy_patients).difference(hard_patients))

    # create pandas series
    val = pd.Series(val)
    easy_val = pd.Series(easy_val)
    hard_val = pd.Series(hard_val)
    structural_val = pd.Series(structural_val)
    num_structural = num_structural

    return val, easy_val, hard_val, structural_val, num_structural


def instances_profile(loss, sizes):
    if loss == 'l1':
        fair_alg = fair_l1_solution
    elif loss == 'maxmin':
        fair_alg = fair_maxmin_solution
    elif loss == 'l2':
        fair_alg = fair_l2_solution
    patients_relaxed = {}
    pct_in_sol = {}

    methods = ['relaxed', 'group_fairness']
    for method in methods:
        if method == 'uniform':
            fair_alg = uniform
        
        for size in sizes:
            easypcts = []
            hardpcts = []
            vals = []
            easy_vals = []
            hard_vals = []
            structural_vals = []
            instances = []

            for i in range(1,51):
                file_id = 2 * (i + (size - 20) // 10 * 50)
                graph_file = '../PortoInstances/{}-instance-{}-type-information.input'.format(size, i)
                solution_file = 'experiments/all_solution_cp/solutions/preprocess_{}/s{}.txt'.format(method, file_id)

                if not os.path.isfile(graph_file):
                    continue
                elif not os.path.isfile(solution_file):
                    continue
                instances.append(file_id)
               
                solutions, solution_prob_dict, pra_dict, easypct, hardpct = \
                        compute_properties(graph_file, solution_file, size, fair_alg)

                val, easy_val, hard_val, structural_val, num_structural = patients_in_sols(solutions, solution_prob_dict, pra_dict)
                vals.append(val)
                easy_vals.append(easy_val)
                hard_vals.append(hard_val)
                structural_vals.append(structural_val)
                easypcts.append(easypct)
                hardpcts.append(hardpct)


            pct_in_sol_ = pd.DataFrame(data={'easy':easypcts, 'hard':hardpcts}, index=instances)
            if size not in pct_in_sol:
                pct_in_sol[size] = pct_in_sol_
            else:
                pct_in_sol[size] = pct_in_sol[size].join(pct_in_sol_, how='inner', rsuffix='_'+method)

            all_df = pd.DataFrame(vals, index=instances, columns=vals[0].index)
            easy_df = pd.DataFrame(easy_vals, index=instances, columns=easy_vals[0].index)
            hard_df = pd.DataFrame(hard_vals, index=instances, columns=hard_vals[0].index)
            structural_df = pd.DataFrame(structural_vals, index=instances, columns=structural_vals[0].index)

            patients_relaxed_ = pd.concat({'All': all_df, 'easy': easy_df, 'hard': hard_df, 'structural': structural_df}, join='inner', axis=1)
            if size not in patients_relaxed:
                patients_relaxed[size] = patients_relaxed_
            else:
                patients_relaxed[size] = patients_relaxed[size].join(patients_relaxed_, how='inner', rsuffix='_'+method) 

    # normalization step
    for size in sizes:
        patients_relaxed[size] = patients_relaxed[size].div(patients_relaxed[size]['All', 3], axis=0)
   
    return patients_relaxed, pct_in_sol


def formatter(x):
    return f'({x})'


def compute_statistics(patients_relaxed, pct_in_sol) :
    sizes = list(pct_in_sol.keys())

    df_1 = pd.concat([patients_relaxed[size].mean() for size in sizes], axis=1, keys=sizes) * 100
    df_1 = df_1.round(2)
    df_2 = pd.concat([pct_in_sol[size].mean() for size in sizes], axis=1, keys=sizes) * 100
    df_2 = df_2.round(2)
    
    print(df_1.to_latex(), '\n')
    print(df_2.to_latex(), '\n')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--loss', type=str, help="The loss that is optimized to get probability distribution")
    args = parser.parse_args()
    patients_relaxed_dict, pct_in_sol_dict = instances_profile(args.loss, range(20,70,10))
    
    compute_statistics(patients_relaxed_dict, pct_in_sol_dict)
