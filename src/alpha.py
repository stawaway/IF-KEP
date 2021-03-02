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

        return [1 / num_solutions for _ in range(num_solutions)]


def compute_properties(graph_file, solution_file, num_patients, fair_alg):
    with open(graph_file, 'r') as f:
        pra_idx = f.readline().strip('\n').split('\t').index('%PRA')

        pra_dict = dict()

        for i in range(num_patients):
            pra_dict[i] = float(f.readline().strip('\n').split('\t')[pra_idx])

    solutions = process_solutions(solution_file)
    num_solutions = len(solutions)
    solution_prob_dict = fair_alg(solution_file, num_patients)
    hard_to_match = [sum([1 for i in sol if pra_dict[i] >= 0.8]) for sol in solutions]
    expectation = sum([hard_to_match[i] * solution_prob_dict[i] for i in range(num_solutions)])
    num_hard_to_match = len([i for i, val in pra_dict.items() if val >= 0.8])
    alpha = expectation / num_hard_to_match if num_hard_to_match > 0 else 0.0
    
    return alpha, np.mean([len(sol) for sol in solutions])


def instances_profile(loss, sizes=[20,30,40,50,60,70]):
    if loss == 'l1':
        fair_alg = fair_l1_solution
    elif loss == 'maxmin':
        fair_alg = fair_maxmin_solution
    elif loss == 'l2':
        fair_alg = fair_l2_solution
    df = {}

    methods = ['base_', 'group_fairness', 'first_best', 'uniform']
    for method in methods:
        if method == 'uniform':
            fair_alg = uniform
        
        for size in sizes:
            alphas = []
            opts = []
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
                
                alpha, opt = compute_properties(graph_file, solution_file, size, fair_alg)

                alphas.append(alpha)
                opts.append(opt)


            col_names = pd.MultiIndex.from_product([['alpha', 'OPT'], [method]])
            df_ = pd.DataFrame(data=np.transpose([alphas, opts]), index=instances, columns=col_names)
            if size not in df:
                df[size] = df_
            else:
                df[size] = df[size].join(df_, how='inner')


    return df


def formatter(x):
    return f'({x})'


def compute_statistics(df, val):
    sizes = list(df.keys())

    if val != 'OPT' and val != 'alpha':
        raise ValueError("This property is not supported")

    index = list(dict.fromkeys([method for _, method in df[sizes[0]].columns]))
    data = {}
    for size in sizes:
        data[(size, 'mean')] = df[size][val].mean().round(3).apply(str)
        data[(size, '(std)')] = df[size][val].std().round(3).apply(formatter)
    df = pd.DataFrame(data=data,
            index=index, columns=data.keys())
    return df


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('--loss', type=str, help="The loss that is optimized to get probability distribution")
    args = parser.parse_args()
    d = instances_profile(args.loss, [20, 30, 40, 50, 60, 70])
    
    print(compute_statistics(d, 'alpha').to_latex(), '\n')
    print(compute_statistics(d, 'OPT').to_latex())
