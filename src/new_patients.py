from matplotlib import pyplot as plt
from matplotlib.ticker import MaxNLocator
import numpy as np
import pandas as pd
import os


def get_solutions(filename, typefilename, size):
    with open(filename, 'r') as f:
        lines = f.readlines()
    with open(typefilename, 'r') as f:
        pra_idx = f.readline().strip('\n').split('\t').index('%PRA')
        pra_dict = {}
        for i in range(size):
            patient_info = list(f.readline().strip('\n').split('\t'))
            pra_dict[i] = float(patient_info[pra_idx])
    solutions = []
    for line in lines:
        solution = list(map(int, line.strip('\n').split(' ')))
        solutions.append(solution)

    return solutions, pra_dict


def patients_in_sols(solutions, pra_dict):
    num_V = len(pra_dict.keys())
    patients = set()
    val = {}
    hard_val = {}
    length = len(solutions[0])
    
    relaxation = 0
    for solution in solutions:
        if len(solution) < length:
            relaxation_ = relaxation
            relaxation = length - len(solution)
            for i in range(relaxation_, relaxation):
                val[i] = len(patients) / num_V
                hard_val[i] = len([v for v in patients if pra_dict[v] >= 0.8]) / num_V
            length = len(solution)
        patients = patients.union(set(solution))
    for j in range(relaxation, 4):
        val[j] = len(patients) / num_V
        hard_val[j] = len([v for v in patients if pra_dict[v] >= 0.8]) / num_V
    return val, hard_val


def plot(values, hard_values):
    for size,val in values.items():
        val = val / val[3]
        hval = hard_values[size]
        hval = hval / val[3] 
        fig, ax = plt.subplots()

        ax.bar(range(len(val)), val*100, width=0.35, label="PRA\u003c80%")
        for x, y in enumerate(val):
            y *= 100
            ax.text(x-0.2, y+0.02, str(round(y,2)), fontsize=25.5)
        ax.bar(range(len(val)), hval*100, width=0.35, label="PRA\u226580%")
        for x, y in enumerate(hval):
            y *= 100
            ax.text(x-0.2, y+0.02, str(round(y,2)), fontsize=25.5)

        ax.set_xticks(range(len(val)))
        ax.set_xticklabels(["OPT"]+[f"OPT-{i}" for i in range(1, len(val))], fontsize=25.5)
        #ax.set_xlabel("Relaxation", size='xx-large')
        plt.setp(ax.get_yticklabels(), fontsize=25.5)
        ax.set_ylabel("Average %  patients included", fontsize=17)
        ax.set_ylim(0,100)
        ax.set_title(f"Graph size {size}", fontsize=25.5, pad=25)
        fig.tight_layout()
        ax.legend(bbox_to_anchor=(1.0, 1.0), loc='upper left', fontsize=25.5)

        fig.savefig(f"experiments/all_solution_cp/plots/new_patients_{size}.png", bbox_inches='tight', dpi=300)


def main():
    values = {}
    hard_values = {}

    for size in [20,30,40,50,60,70]:
        instance = []
        instance_hard = []
        index = []
        for i in range(1, 51):
            jobid = 2 * (i + (size - 20) // 10 * 50)
            filename = f"experiments/all_solution_cp/solutions/preprocess_greedy_relax/s{jobid}.txt"
            typefilename = f'../PortoInstances/{size}-instance-{i}-type-information.input'
            if not os.path.isfile(filename):
                continue
            index.append(jobid)
            
            solutions, pra_dict = get_solutions(filename, typefilename, size)
            patients, hard_patients = patients_in_sols(solutions, pra_dict)
            instance.append(patients)
            instance_hard.append(hard_patients)

        df = pd.DataFrame(data=instance, index=index, columns=range(4))
        df_h = pd.DataFrame(data=instance_hard, index=index, columns=range(4))

        values[size] = df.mean()
        hard_values[size] = df_h.mean()
    
    plot(values, hard_values)


if __name__ == "__main__":
    main()
