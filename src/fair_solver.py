#!/usr/bin/env python
# coding: utf-8

# In[1]:


from __future__ import print_function, division
from gurobipy import Model, GRB, quicksum, LinExpr
import os, operator
from random_solver import process_solutions


# In[2]:


def fair_maxmin_solution(solutions, num_patients):
    model = Model('maximin_fair')
    model.params.outputflag = 0
    solutions = process_solutions(solutions)
    solution_size = len(solutions)
    solution_dict = {}

    dict_array = []
    solution_vars = []
    for i in range(solution_size):
        p = model.addVar(lb=0, ub=1, vtype=GRB.CONTINUOUS)
        solution_dict[i] = p
        solution_vars.append(p)
    solution_exprt = quicksum(solution_vars)
    model.addConstr(solution_exprt, GRB.EQUAL, 1.0)
    
    node_prob_dict = {}
    for i in range(solution_size):
        for item in solutions[i]:
            if int(item) in node_prob_dict.keys():
                node_prob_dict[int(item)].append(i)
            else:
                node_prob_dict[int(item)]=[i]
        
    y = model.addVar(lb=0, ub=1.0, vtype=GRB.CONTINUOUS)
    
    for key, value in node_prob_dict.items():
        tmp = []
        for solution in value:
            tmp.append(solution_dict[solution])
        expr = quicksum(tmp) 
        #expr_constant = float(len(value))
        model.addConstr(expr, GRB.GREATER_EQUAL, y)
        
    
    
    model.update()

    model.setObjective(y, GRB.MAXIMIZE)
    
    model.update()

    try:
        model.optimize()
    except e:
        print(e)
        
    if (model.status == GRB.Status.OPTIMAL):
        solution_prob_dict = {}   
        for key,value in solution_dict.items():
            solution_prob_dict[key] = value.x
        
        
        return solution_prob_dict
    else:
        return {}
    


# In[3]:


def fix_solution(solutions):
    i = 0
    patient_dict = {}
    fixed_solutions = []
    for j in range(len(solutions)):
        solution = solutions[j]
        fixed_solution = []
        for t in range(len(solution)):
            item = solution[t]
            if item in patient_dict.keys():
                fixed_solution.append(patient_dict[item])
            else:
                patient_dict[item] = i
                fixed_solution.append(i)
                i+=1
        fixed_solutions.append(fixed_solution)
    return fixed_solutions, len(patient_dict.keys())


# In[4]:


def fair_l1_solution(old_solutions, num_patients):

    solutions = process_solutions(old_solutions)
    num_solutions = len(solutions)

    
    patient_to_solutions = [[] for _ in range(num_patients)]
    for s, solution in enumerate(solutions):
        for p in solution:
            patient_to_solutions[p].append(s)


    model = Model()
    model.params.OutputFlag = 0


    solution_probs = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_solutions)]
    model.addConstr(quicksum(solution_probs), GRB.EQUAL, 1.0)

    patient_probs = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_patients)]

    for i in range(num_patients):
        expr = quicksum(solution_probs[j] for j in patient_to_solutions[i])
        model.addConstr(patient_probs[i], GRB.EQUAL, expr)

    m = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
    model.addConstr(quicksum(patient_probs), GRB.EQUAL, num_patients * m)

    distance_to_mean = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_patients)]
    for i in range(num_patients):
        model.addConstr(distance_to_mean[i], GRB.GREATER_EQUAL, patient_probs[i] - m)
        model.addConstr(distance_to_mean[i], GRB.GREATER_EQUAL, m - patient_probs[i])

    model.setObjective(quicksum(distance_to_mean), GRB.MINIMIZE)
    model.optimize()

    #return [solution_probs[i].x for i in range(num_solutions)]
    
    if (model.status == GRB.Status.OPTIMAL):
        solution_prob_dict = {}   
        for i in range(num_solutions):
            solution_prob_dict[i] = solution_probs[i].x
        
        
        return solution_prob_dict
    else:
        return {}
   
    


# In[5]:


def fair_l2_solution(solutions, num_patients):
    
    solutions = process_solutions(solutions)
    num_solutions = len(solutions)

    patient_to_solutions = [[] for _ in range(num_patients)]
    for s, solution in enumerate(solutions):
        for p in solution:
            patient_to_solutions[p].append(s)


    model = Model()
    model.params.OutputFlag = 0


    solution_probs = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_solutions)]
    model.addConstr(quicksum(solution_probs), GRB.EQUAL, 1.0)

    patient_probs = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_patients)]

    for i in range(num_patients):
        expr = quicksum(solution_probs[j] for j in patient_to_solutions[i])
        model.addConstr(patient_probs[i], GRB.EQUAL, expr)

    m = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
    model.addConstr(quicksum(patient_probs), GRB.EQUAL, num_patients * m)

    distance_to_mean = [model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS) for _ in range(num_patients)]
    for i in range(num_patients):
        model.addConstr(distance_to_mean[i], GRB.GREATER_EQUAL, (patient_probs[i] - m)*(patient_probs[i] - m))
        #model.addConstr(distance_to_mean[i], GRB.GREATER_EQUAL, m - patient_probs[i])

    model.params.QCPDual = 0
    model.setObjective(quicksum(distance_to_mean), GRB.MINIMIZE)
    model.optimize()

    return [solution_probs[i].x for i in range(num_solutions)]
    
    if (model.status == GRB.Status.OPTIMAL):
        solution_prob_dict = {}   
        for i in range(num_solutions):
            solution_prob_dict[i] = value.x
        
        
        return solution_prob_dict
    else:
        return {}
   


# In[6]:


def fair_l1_solution_old(solutions, jobid):
    model = Model('l1_fair%s'%(jobid))
    model.params.outputflag = 0
    variable_size = len(solutions)
    solution_dict = {}

    dict_array = []

    solution_vars = []
    for i in range(variable_size):
        p = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
        solution_dict[i] = p
        solution_vars.append(p)
    solution_exprt = quicksum(solution_vars)
    model.addConstr(solution_exprt, GRB.EQUAL, 1.0)
    
    prob_dict = {}
    
    for i in range(len(solutions)):
        for item in solutions[i]:
            if item in prob_dict.keys():
                prob_dict[item].append(i)
            else:
                prob_dict[item]=[i]
    
    
    vertex_size = len(prob_dict.keys())
    prob_var_dict = {}
    sum_var = []
    for key, value in prob_dict.items():
        tmp = []
        for solution in value:
            tmp.append(solution_dict[int(solution)])
            sum_var.append(solution_dict[int(solution)])
        expr = quicksum(tmp)    
        prob_var_dict[key] = tmp
        
        
    sum_expr = quicksum(sum_var)  
    avg_constant = float(1)/float(variable_size)
    avg_exp = LinExpr(avg_constant*sum_expr)

    
    distance_dict = {}
    
    distances = []
    for key in prob_dict.keys():
        d = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
        tmp = prob_var_dict[key]
        w_expr = quicksum(tmp)   
        diff_expr_1 = w_expr - avg_exp
        diff_expr_2  = avg_exp - w_expr
        model.addConstr(d, GRB.GREATER_EQUAL,diff_expr_1)
        model.addConstr(d, GRB.GREATER_EQUAL,diff_expr_2)
        distances.append(d)
        
    obj_expr = quicksum(distances)

    model.setObjective(obj_expr, GRB.MINIMIZE)
    
    model.update()

    try:
        model.optimize()
    except e:
        print(e)
        
    if (model.status == GRB.Status.OPTIMAL):
        solution_prob_dict = {}   
        for key,value in solution_dict.items():
            solution_prob_dict[key] = value.x
        
        
        return solution_prob_dict
    else:
        return {}


# In[7]:


def fair_l2_solution_old(solutions, jobid):
    model = Model('l2_fair%s'%(jobid))
    model.params.outputflag = 0
    variable_size = len(solutions)
    solution_dict = {}

    dict_array = []

    solution_vars = []
    for i in range(variable_size):
        p = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
        solution_dict[i] = p
        solution_vars.append(p)
    solution_exprt = quicksum(solution_vars)
    model.addConstr(solution_exprt, GRB.EQUAL, 1.0)
    
    prob_dict = {}
    
    for i in range(len(solutions)):
        for item in solutions[i]:
            if item in prob_dict.keys():
                prob_dict[item].append(i)
            else:
                prob_dict[item]=[i]
    
    
    vertex_size = len(prob_dict.keys())
    prob_var_dict = {}
    sum_var = []
    for key, value in prob_dict.items():
        tmp = []
        for solution in value:
            tmp.append(solution_dict[int(solution)])
            sum_var.append(solution_dict[int(solution)])
        expr = quicksum(tmp)    
        prob_var_dict[key] = tmp
        
        
    sum_expr = quicksum(sum_var)  
    avg_constant = float(1)/float(variable_size)
    avg_exp = LinExpr(avg_constant*sum_expr)

    
    distance_dict = {}
    
    distances = []
    for key in prob_dict.keys():
        d = model.addVar(lb=0.0, ub=1.0, vtype=GRB.CONTINUOUS)
        tmp = prob_var_dict[key]
        w_expr = quicksum(tmp)   
        diff_expr = w_expr - avg_exp
        
        model.addQConstr(d, GRB.GREATER_EQUAL,diff_expr*diff_expr)
        
        distances.append(d)
        
    obj_expr = quicksum(distances)

    model.setObjective(obj_expr, GRB.MINIMIZE)
    
    model.update()

    try:
        model.optimize()
    except e:
        print(e)
        
    if (model.status == GRB.Status.OPTIMAL):
        solution_prob_dict = {}   
        for key,value in solution_dict.items():
            solution_prob_dict[key] = value.x
        
        
        return solution_prob_dict
    else:
        return {}


# In[8]:


'''solution_file = '../../../experiments/all_solution_cp/solutions/s2.txt'
solutions = process_solutions(solution_file)
jobid = 's6'

solution_prob_dict = fair_maximin_solution(solutions, jobid)
solution_prob_dict 
solution = max(solution_prob_dict.iteritems(), key=operator.itemgetter(1))[0]
solution
solution_prob_dict = fair_l1_solution(solutions, jobid)
solution_prob_dict
solution = max(solution_prob_dict.iteritems(), key=operator.itemgetter(1))[0]
solution
'''


# In[9]:


'''
solutions = [[1,2], [1,3], [2,4], [1,4]]
job_id = 'test'
solution_prob_dict = fair_maximin_solution(solutions, jobid)
solution_prob_dict
solution = max(solution_prob_dict.iteritems(), key=operator.itemgetter(1))[0]
solution
solution_prob_dict = fair_l1_solution(solutions, jobid)
solution_prob_dict
'''


# In[10]:


'''
solutions = [[1,2], [1,3], [2,4], [1,4]]
jobid = 'test'
solution_prob_dict = fair_maximin_solution(solutions, jobid)

'''


# In[11]:


'''
solution_prob_dict

solution = max(solution_prob_dict.iteritems(), key=operator.itemgetter(1))[0]
solution_prob_dict
solution
'''

