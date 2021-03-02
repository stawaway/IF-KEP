#!/usr/bin/env python

import sys, time, argparse

"""
Python 3
REQUIREMENTS: GUROBI
This code determines a solution that maximizes the number of transplants given a directed graph
where cycles of length at most K are allowed and chains of length at most L are allowed.
"""
# READ INSTANCE
# INPUT
# filename - it is a string
# OUTPUT
# G - it is a incidence list; a dictionary
# num_V - number of nodes
# Nb_arcs - number of arcs
# altruistic_list - list of altruistic nodes
def read_kep(filename):
    #read file in the 'standard' kep format
    f = open(filename)
    num_V, num_E = map(int,f.readline().split()) # number of nodes, number of edges
    # nodes are labeled from 0 to n
    G = {i:[] for i in range(num_V)}
    Nb_arcs = 0
    for _ in range(num_E):
        v1, v2, w = map(int,f.readline().split()) # there is an arc v1 to v2; ARC's weights are being ignored (i.e., they are 1)
        G[v1].append(v2)
        Nb_arcs = Nb_arcs +1
    num_Altruistic = int(eval(f.readline())) # number of altruistic donors
    altruistic_list = []
    for _ in range(num_Altruistic):
        altruistic_list.append(int(eval(f.readline())))
    f.close()
    return G, num_V, Nb_arcs, altruistic_list

def read_pra(filename):
    f = open(filename)
    head = f.readline().split('\t')
    col = head.index('%PRA')
    pra_list = []

    for idx, line in enumerate(f):
        pra = float(line.strip('\n').split('\t')[col])
        pra_list.append(pra)

    f.close()

    return pra_list


# Compute all the cycles c of length less or equal to K and save the respective length wc
# make cycles to start in the node with smallest label
def normalize(cycle):
    cmin = min(cycle)
    while cycle[0] != cmin:
        v = cycle.pop(0)
        cycle.append(v)

def all_cycles(cycles, path, node, tovisit, adj,K):
    global spc
    for i in adj[node]:
        if i in path:
            j = path.index(i)
            cycle = path[j:]+[node]
            normalize(cycle)
            cycles.add(tuple(cycle))
        if i in tovisit:
            if K-1 > 0:
                all_cycles(cycles,path+[node],i,tovisit-set([i]),adj,K-1)
    # spc = spc[:-4]
    return cycles


# INPUT
# adj - incidence list; a dictionary
# K - maximumsize for cycles length
# OUTPUT
# cycles - set of tuples of size K
def get_all_cycles(adj,K):
    tovisit = set(adj.keys())
    visited = set([])
    cycles = set([])
    for i in tovisit:
        tmpvisit = set(tovisit)
        tmpvisit.remove(i)
        first = i
        all_cycles(cycles,[],first,tmpvisit,adj,K)
    return cycles

# Input
# cycles - List of cycles
# adj - incidence list; a dictionary
# OUTPUT
# back-arcs for each cycle; a dictionary
def get_back_arcs(cycles, adj):
    arcs = []
    for cycle in cycles:
        K = len(cycle)
        if K <= 2:
            arcs.append(0)
            continue
        cycle_arcs = 0
        for i,node in enumerate(cycle):
            for j in adj[node]:
                id = (i+ 1) % K
                if j in cycle and cycle[id] != j:
                    cycle_arcs += 1
        arcs.append(cycle_arcs)
    return arcs


from gurobipy import *
# INPUT
# G - incidence list; a dictionary
# K - maximum size for cycles length
# L - maximum length of chain size for cycles length
# altruistic_list - list of altruistic nodes
# OUTPUT
# Optimal value
# running time (seconds)
# model
# variables X
# variables Z
def solve_KEP(G,K,L=0,altruistic_list=[], pra_list=[]):
    setParam("OutputFlag", 0)
    # compute all cycles of length at most 3
    Cycles_k = list(get_all_cycles(G,K))
    # find the # of back-arcs for each 3-cycle
    back_arcs = get_back_arcs(Cycles_k, G)
    # create model
    m = Model("Deterministic KEP")
    # create the variables associated with cycles
    X = {i+1: m.addVar(obj = len(c), vtype="B",name="X"+str(i+1)) for i,c in enumerate(Cycles_k)}
    m.update()
    # to understand the dictionary below see how chains can be considered in "Position-Indexed Formulations for Kidney Exchange"
    K_dic = {(i,j):[1] if i in altruistic_list else range(2,L+1) for i in G.keys() for j in G[i]}
    # create variables for chains
    Z = {(i,j,l): m.addVar(obj = 1, vtype="B",name="z"+str(i)+"_"+str(j)+"_"+str(l)) for i,j in K_dic.keys() for l in K_dic[(i,j)]}
    m.update()
    for v in G.keys():
        # WRONG constraint each vertex donates at most one kidney: not enough for correctness!
        #m.addConstr(quicksum(X[i+1]  for i,c in enumerate(Cycles_k) if v in c)+quicksum(Z[(v,j,l)] for j in G[v] for l in K_dic[(v,j)])<= 1)
        # each vertex receives at most one kidney: correct!
        m.addConstr(quicksum(X[i+1]  for i,c in enumerate(Cycles_k) if v in c)+quicksum(Z[(j,v,l)] for j in G.keys() if v in G[j] for l in K_dic[(j,v)])<= 1)
        m.update()
        if v not in altruistic_list:
            for l in range(1,L):
                m.addConstr(quicksum(Z[(j,v,l)] for j in G.keys() if v in G[j] and l in K_dic[(j,v)])>= quicksum(Z[(v,j,l+1)] for j in G[v]))
                m.update()
        else:
            m.addConstr(quicksum(Z[(v,j,1)] for j in G[v])<= 1)
            m.update()
    m.ModelSense = -1 # maximize
    m.update()
    m.optimize()
    # add constraints to maximize # of cycles
    m.addConstr(quicksum([len(c) * X[i+1] for i,c in enumerate(Cycles_k)]) >= m.ObjVal - 3.0)
    m.setObjective(quicksum([X[i+1] for i,c in enumerate(Cycles_k)]))
    m.ModelSense = -1 # maximize
    m.update()
    m.optimize()
    # add constraints to max the # of back-arcs
    m.addConstr(quicksum([X[i+1] for i,c in enumerate(Cycles_k)]) >= m.ObjVal)
    m.setObjective(quicksum([back_arcs[i] * X[i+1] for i,c in enumerate(Cycles_k)]))
    m.ModelSense = -1 # maximize
    m.update()
    m.optimize()
    m.addConstr(quicksum([back_arcs[i] * X[i+1] for i,c in enumerate(Cycles_k)]) >= m.ObjVal)
    m.setObjective(quicksum([sum(map(lambda x: 1 if pra_list[x] >= 0.8 else 0.0, c)) * X[i+1] for i,c in enumerate(Cycles_k)]))
    m.update()
    m.setParam("PoolSolutions", 2000000000)
    m.setParam("PoolSearchMode", 2)
    m.setParam("PoolGap", 0)
    m.reset(clearall=0.5)
    m.optimize()
    #print("\n###############################################")
    #print("# Optimal solution for KEP #")
    #print("###############################################")
    solutions = []
    for i in range(m.SolCount):
        print(i)
        m.setParam("SolutionNumber", i)
        sol = m.Xn
        solutions.append([item for idx,c in enumerate(Cycles_k) for item in c if sol[idx] > 0.5]) 
        # solutions.append(m.Xn[:len(Cycles_k)])

    return m.ObjVal,m.Runtime, m, X, Z, solutions


def save_to_file(filename, solutions):
    """
    Method to save solutions to file
    :param filename: String giving the filename
    :param solutions: The list of solutions to save to file
    """
    with open(filename, 'w') as f:
        lines = [list(map(str, solution)) for solution in solutions]
        lines = [" ".join(line) + "\n" for line in lines]
        f.writelines(lines)


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument('filename')
    parser.add_argument('prafile')
    parser.add_argument('outfile')
    parser.add_argument('--cycle-limit', type=int, default=3)
    parser.add_argument('--chain-limit', type=int, default=3)
    args = parser.parse_args()

    G, num_V, Nb_arcs, altruistic_list = read_kep(args.filename)
    pra_list = read_pra(args.prafile)

    start_time = time.time()
    obj, _, _, _, _, sols = solve_KEP(G, args.cycle_limit,
            args.chain_limit, altruistic_list, pra_list)

    save_to_file(args.outfile, sols)
    
    runtime = time.time() - start_time    
    print('runtime: %g'%runtime)
    print('number of solutions %d'%len(sols))

