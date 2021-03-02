#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os, sys
import random
import operator
import os.path
from os import path


# In[2]:


def process_solutions(solution_file):
    solutions = []
    if path.isfile(solution_file):
        with open(solution_file, 'r') as sf:
            lines = sf.readlines()
            #print(len(lines))
            for line in lines:
                line = line.replace('\n','')
                s = line.split(' ')
                if len(s)>0:
                    fixed_s = fix(s)
                    if len(solutions)>0:
                        if fixed_s==solutions[-1]:
                            pass
                        else:
                            solutions.append(fixed_s)    
                    else:
                        solutions.append(fixed_s)
        return solutions


# In[3]:


def fix(solution):
    fixed_solution = []
    for item in solution:
        fixed_solution.append(int(item))
    return fixed_solution


# In[4]:


def find_random_solutions(solution_file):
    with open(solution_file, 'r') as sf:
        lines = sf.read_lines()
        size = len(lines)
        random_solution =  random.randint(0, size)
        return lines[random_solution]  


# In[5]:


def evaluate_random_solution(solutions):
    prob_dict = {}
    solution_prob = float(1)/float(len(solutions))
    for solution in solutions:
        for item in solution:
            if item in prob_dict.keys():
                prob_dict[item]+=solution_prob
            else:
                prob_dict[item]=solution_prob
    return prob_dict


# In[6]:


'''solution_file = '../../../experiments/all_solution_cp/solutions/s6.txt'
solutions = process_solutions(solution_file)

prob_dict = evaluate_random_solution(solutions)
'''


# In[7]:


#print(#)


# In[8]:


#random_solution = max(prob_dict.iteritems(), key=operator.itemgetter(1))[0]


# In[9]:


#print(random_solution)


# In[ ]:




