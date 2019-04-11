import pyomo.environ as pe
import pandas as pd
import numpy as np
import pdb

pd.options.display.max_colwidth = 100


class AvgCostMDP2:
    def __init__(self,InDir,filename,filetype,readfile_suffixes,actions,alpha=0.9):#,absorbing_states=[]):
        self.InDir=InDir
        self.filename=[filename+s+filetype for s in readfile_suffixes] 
        self.actions=actions #these will be the sheetnames in above file that we want to read
        self.states=[]
        self.states_actions={}
        self.transitions={}
        self.qalys={}
        self.populate_states() #tpm: transition probability matrix
        self.populate_state_dependent_actions()
        self.qaly_func()
        self.alpha=alpha
#         self.transient_states=[s for s in self.states if s not in absorbing_states]
        
    def populate_states(self):
        for t in range(len(readfile_suffixes)):
            self.transitions[readfile_suffixes[t]]={}
            for a in self.actions:
                try:
                    df=pd.read_excel(self.InDir+'\\'+self.filename[t],sheetname=a)
                except:
                    print('\n\n!!!...Pandas read error...!!!\n\n')
                    pdb.set_trace()
                try:
                    df=df.set_index(df.columns[0])
                    self.transitions[readfile_suffixes[t]][a]=df
                except:
                    print('\n\n!!!...Pandas set index key error...!!!\n\n')
                    pdb.set_trace()
                if t==0:
                    try:
                        self.states=np.append(self.states,[s for s in list(df.index) if s not in self.states])
                        self.states=np.append(self.states,[s for s in list(df.columns) if s not in self.states])
                    except:
                        print('\n\n!!!...State space error...!!!\n\n')
                        pdb.set_trace()
        
    def populate_state_dependent_actions(self):
        for s in self.states:
            self.states_actions[s]=[]
        for a in self.transitions[readfile_suffixes[0]]:
            transition_df=self.transitions[readfile_suffixes[0]][a]
            for s in list(transition_df.index):
                if a not in self.states_actions[s]:
                    self.states_actions[s]=np.append(self.states_actions[s],a)# if a not in self.states_actions[s] else continue
    
    def qaly_func(self):
        if qaly=='readFile':
            df=pd.read_excel(self.InDir+'\\'+self.filename[0],sheetname="QALY for state")
            df=df[['State','Reward']] #We want to drop any other columns in the data file such
            for s in self.states:
                self.qalys[s]={}
                for a in self.actions:
#                     try:
#                         if a in self.states_actions[s]:
#                             q=-float(df[df['State']==s]['Reward'])
#                     except:
#                         pdb.set_trace()
                    self.qalys[s][a]=float(df[df['State']==s]['Reward']) if a in self.states_actions[s] else 0.0                  
        elif qaly==1:
            for s in self.states:
                self.qalys[s]={}
                for a in self.actions:
                    self.qalys[s][a]=1.0 if a in self.states_actions[s] else 0.0        

'''Robust MIP definition and solution'''
# %%time  
actions=['1st AED',
         '2nd AED',
#          '3rd AED',
#          'Workup to Surgery',
         'Workup to Surgery with 3rd AED',
         'Surgery or resection',
         'Medical Management',
         'AED after SF',
         'Discontinue AED']   
# absorbing_states=['Refractory Epilepsy','Adverse Outcomes','']

qaly='readFile'
InDir=r"C:\Users\Shreya\Dropbox\Shreya\Research Shreya\Epilepsy\Model"
filename='Data_Required_13' 
filetype='.xlsx'
num_TPMs=3 #number of transition probability matrices
readfile_suffixes=list(map(chr, range(97, 97+num_TPMs)))
mdp=AvgCostMDP2(InDir,filename,filetype,readfile_suffixes,actions,qaly)
num_states=len(mdp.states)
num_actions=len(mdp.actions)

ai_s_RHS=[np.round(1.0/num_states,3)]*num_states
ai_s_RHS[0]=0.04#1-sum(ai_s_RHS[1:])

#------ Model Definition -------

# del model
model = pe.ConcreteModel()
model.dual = pe.Suffix(direction = pe.Suffix.IMPORT)
model.TPMs  = pe.Set(initialize = range(num_TPMs))
model.states  = pe.Set(initialize = range(num_states))
model.actions  = pe.Set(initialize = range(num_actions))
# def cost_func(model,state,action): 
#     return mdp.qalys[mdp.states[state]][mdp.actions[action]]
# model.cost = pe.Param(model.states, model.actions, initialize = cost_func)
def cost_func(model): 
    return 1.0
model.cost = pe.Param(initialize = cost_func)
model.ai_s = pe.Param(model.states,   initialize = lambda model,s: ai_s_RHS[s]) #'s' is the index num of corresponding state
model.q = pe.Var(model.TPMs, model.states, model.actions, domain = pe.NonNegativeReals)
model.r = pe.Var(model.TPMs, model.states, model.actions, domain = pe.NonNegativeReals)
model.y = pe.Var(model.states, model.actions, domain = pe.Binary)
model.tau = pe.Var()
#------------- CONSTRAINTS -----------
                         
def avg_cost_mdp_dual_constraint1(model,t,j): #j is the state for which the constraint is being written
    sum_over_actions_q = sum(model.q[t,j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]])
    sum_over_probab_transitions_with_q=0
#     tpm=readfile_suffixes[t] #transition probability matrix
    for i in model.states: #we will loop over all states j
        for a in model.actions:
            action=mdp.actions[a]
            if action in mdp.states_actions[mdp.states[i]]:
                transitions=mdp.transitions[readfile_suffixes[t]][action]
                if mdp.states[j] in transitions.columns:
                    try:
                        pij=transitions.at[mdp.states[i],mdp.states[j]]
                        if not(np.isnan(pij)):
                            sum_over_probab_transitions_with_q=sum_over_probab_transitions_with_q + pij*model.q[t,i,a]  
                    except:
                        pij=0.0
    return(sum_over_actions_q - sum_over_probab_transitions_with_q == 0)
model.Constraint1 = pe.Constraint(model.TPMs,model.states, rule = avg_cost_mdp_dual_constraint1)
model.Constraint1.pprint()

def avg_cost_mdp_dual_constraint2(model,t,j): #j is the state for which the constraint is being written
    sum_over_actions_q = sum(model.q[t,j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]])
    sum_over_actions_r = sum(model.r[t,j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]]) 
    sum_over_probab_transitions_with_r=0
    for i in model.states: #we will loop over all states i
        for a in model.actions:
            action=mdp.actions[a]
            if action in mdp.states_actions[mdp.states[i]]:
                transitions=mdp.transitions[readfile_suffixes[t]][action]
                if mdp.states[j] in transitions.columns:
                    try:
                        pij=transitions.at[mdp.states[i],mdp.states[j]]
                        if not(np.isnan(pij)):
                            sum_over_probab_transitions_with_r=sum_over_probab_transitions_with_r + pij*model.r[t,i,a]  
                    except:
                        pij=0.0
#     if i==15:
#         print(mdp.states[i])
#         pdb.set_trace()
    return(sum_over_actions_q + sum_over_actions_r - sum_over_probab_transitions_with_r == model.ai_s[j])
model.Constraint2 = pe.Constraint(model.TPMs,model.states, rule = avg_cost_mdp_dual_constraint2)
model.Constraint2.pprint()

def policy_constraint3a(model,t,j,a): 
    return(model.q[t,j,a] <= model.y[j,a]*1e4)
model.Constraint3a = pe.Constraint(model.TPMs, model.states, model.actions, rule = policy_constraint3a)
model.Constraint3a.pprint()

def policy_constraint3b(model,t,j,a): 
    return(model.r[t,j,a] <= model.y[j,a]*1e4)
model.Constraint3b = pe.Constraint(model.TPMs, model.states, model.actions, rule = policy_constraint3b)
model.Constraint3b.pprint()

def policy_constraint4(model,j,a): #setting yja's corresponding to non-existent state-action pairs to be zero
    sum_over_y = sum(model.y[j,a] for a in model.actions if mdp.actions[a] not in mdp.states_actions[mdp.states[j]])
    return(sum_over_y == 0)
model.Constraint4 = pe.Constraint(model.states, model.actions, rule = policy_constraint4)
model.Constraint4.pprint()

def sum_y_constraint5(model,j): #j is the state for which the constraint is being written
    sum_over_y = sum(model.y[j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]])
    return(sum_over_y == 1)
model.Constraint5 = pe.Constraint(model.states, rule = sum_y_constraint5)
model.Constraint5.pprint()

def epigraph_constraint6(model,t): #i is the state for which the constraint is being written
#     pdb.set_trace() #take sum
    sum_cqs=sum(mdp.qalys[mdp.states[s]][mdp.actions[a]] * model.q[t,s,a] for s in model.states for a in model.actions)
    return(model.tau <= sum_cqs)#mdp.qalys[mdp.states[j]][mdp.actions[a]] * model.q[t,j,a])# model.cost[j,a] * model.q[t,j,a])
model.Constraint6 = pe.Constraint(model.TPMs, rule = epigraph_constraint6)
model.Constraint6.pprint()

#------ Objective Function ------

def obj_rule(model):
    return model.cost*model.tau
#     return sum(model.cost[s,a] * model.q[s,a] for s in model.states for a in model.actions)
model.OBJ = pe.Objective(rule = obj_rule, sense = pe.maximize)
# model.OBJ = pe.Objective(rule = model.tau, sense = pe.minimize)
model.OBJ.pprint()

#------ Solve LP ------

solver = pe.SolverFactory('gurobi') # Specify Solver
results = solver.solve(model, tee = False, keepfiles = False)
print("\n\nStatus:", results.solver.status)
print("Termination Condition:", results.solver.termination_condition)

# --------- POST-PROCESSING -------------------

states=[]
actions=[]
dual_vals=[]
opt_vals={}
#Find the TPM with the maximum cost
for t in range(len(readfile_suffixes)):
    opt_vals[t]=sum(mdp.qalys[mdp.states[s]][mdp.actions[a]] * model.q[t,s,a].value for s in model.states for a in model.actions)
t=max(opt_vals, key=opt_vals.get)    
for s in model.states:
    for a in model.actions:
        try:
            if (model.q[t,s,a].value > 0 and model.q[t,s,a].value != None) or (
                 model.r[t,s,a].value > 0 and model.r[t,s,a].value != None):
                states=np.append(states,mdp.states[s])
                actions=np.append(actions,mdp.actions[a])
                dual_vals=np.append(dual_vals,np.round(model.q[t,s,a].value,10))
#                 opt_val+=mdp.qalys[]*model.q[t,s,a].value
        except:
            print('Suspicious value at state %d action %d'%(s,a),model.q[t,s,a].value)
            continue
df=pd.DataFrame.from_dict(dict(state=states,action=actions,dual_val=dual_vals))
print("\n\nObjective function value: ", model.OBJ())
print(df[['state','action','dual_val']])
t=max(opt_vals, key=opt_vals.get)
print(readfile_suffixes[t],opt_vals[t],opt_vals)


   