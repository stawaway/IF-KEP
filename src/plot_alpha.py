from alpha import main
from matplotlib import pyplot as plt


def plot():
    inst_dicts = {}
    for method in ["maxmin", "l1", "l2"]:
        inst_dicts[method] = main(method)
    fig = plt.figure()
    ax = fig.subplots()

    ax.set_xlabel("Graph size")
    ax.set_ylabel("Expected number of hard-to-match patients in a solution")

    for key, inst_dict in inst_dicts.items():
        x = [20,30,40,50,60,70]
        y = [df['base_', 'alpha'].mean() for size, df in inst_dict.items()]

        ax.plot(x,y, label=key)

    ax.legend()
    fig.savefig("experiments/all_solution_cp/alpha_plot.png")


if __name__ == "__main__":
    plot()

