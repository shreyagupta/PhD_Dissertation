import pyomo.environ as pe
import pandas as pd
import numpy as np
import pdb

pd.options.display.max_colwidth = 100


class AvgCostMDP:
    def __init__(self,InDir,filename,actions,alpha=0.9):#,absorbing_states=[]):
        self.InDir=InDir
        self.filename=filename
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
        for a in self.actions:
            try:
                df=pd.read_excel(self.InDir+'\\'+self.filename,sheetname=a)
            except:
                print('\n\n!!!...Pandas read error...!!!\n\n')
                pdb.set_trace()
            try:
                df=df.set_index(df.columns[0])
                self.transitions[a]=df
            except:
                print('\n\n!!!...Pandas set index key error...!!!\n\n')
                pdb.set_trace()
            try:
                self.states=np.append(self.states,[s for s in list(df.index) if s not in self.states])
                self.states=np.append(self.states,[s for s in list(df.columns) if s not in self.states])
            except:
                print('\n\n!!!...State space error...!!!\n\n')
                pdb.set_trace()
        
    def populate_state_dependent_actions(self):
        for s in self.states:
            self.states_actions[s]=[]
        for a in self.transitions:
            transition_df=self.transitions[a]
            for s in list(transition_df.index):
#                 if s=='Seizure free with drugs after Workup to Surgery with 3rd AED':
#                     pdb.set_trace()
                if a not in self.states_actions[s]:
                    self.states_actions[s]=np.append(self.states_actions[s],a)# if a not in self.states_actions[s] else continue
    
    def qaly_func(self):
        if qaly=='readFile':
            df=pd.read_excel(self.InDir+'\\'+self.filename,sheetname="QALY for state")
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


'''Dual LP definition and solution''' 
# %%time
InDir=r"C:\Users\Shreya\Dropbox\Shreya\Research Shreya\Epilepsy\Model"
filename='Data_Required_12_.xlsx'    
actions=['1st AED',
         '2nd AED',
         '3rd AED',
#          'Workup to Surgery',
         'Workup to Surgery with 3rd AED',
         'Surgery or resection',
         'Medical Management',
         'AED after SF',
         'Discontinue AED']   
# absorbing_states=['Refractory Epilepsy','Adverse Outcomes','']
qaly='readFile'
mdp=AvgCostMDP(InDir,filename,actions,qaly)

num_states=len(mdp.states)
num_actions=len(mdp.actions)

ai_s_RHS=[np.round(1.0/num_states,3)]*num_states
ai_s_RHS[0]=0.04#1-sum(ai_s_RHS[1:])

#------ Model Definition -------

# del model
model = pe.ConcreteModel()
model.dual = pe.Suffix(direction = pe.Suffix.IMPORT)
model.states  = pe.Set(initialize = range(num_states))
# model.states  = pe.Set(initialize = range(num_states))
model.actions  = pe.Set(initialize = range(num_actions))
def cost_func(model,state,action): 
    return mdp.qalys[mdp.states[state]][mdp.actions[action]]
model.cost = pe.Param(model.states, model.actions, initialize = cost_func)
model.ai_s = pe.Param(model.states,   initialize = lambda model,s: ai_s_RHS[s]) #'s' is the index num of corresponding state
model.X = pe.Var(model.states, model.actions, domain = pe.NonNegativeReals)
model.Y = pe.Var(model.states, model.actions, domain = pe.NonNegativeReals)

#------------- CONSTRAINTS -----------
                         
def avg_cost_mdp_dual_constraint1(model, j): #i is the state for which the constraint is being written
    sum_over_actions_X = sum(model.X[j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]])
    sum_over_probab_transitions_with_X=0
    for i in model.states: #we will loop over all states j
        for a in model.actions:
            action=mdp.actions[a]
            if action in mdp.states_actions[mdp.states[i]]:
                transitions=mdp.transitions[action]
                if mdp.states[j] in transitions.columns:
                    try:
                        pij=transitions.at[mdp.states[i],mdp.states[j]]
                        if not(np.isnan(pij)):
                            sum_over_probab_transitions_with_X=sum_over_probab_transitions_with_X + pij*model.X[i,a]  
                    except:
                        pij=0.0
    return(sum_over_actions_X - sum_over_probab_transitions_with_X == 0)
model.Constraint1 = pe.Constraint(model.states, rule = avg_cost_mdp_dual_constraint1)
# model.Constraint1.pprint()

def avg_cost_mdp_dual_constraint2(model, j): #i is the state for which the constraint is being written
    sum_over_actions_X = sum(model.X[j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]])
    sum_over_actions_Y = sum(model.Y[j,a] for a in model.actions if mdp.actions[a] in mdp.states_actions[mdp.states[j]]) 
    sum_over_probab_transitions_with_Y=0
    for i in model.states: #we will loop over all states j
        for a in model.actions:
            action=mdp.actions[a]
            if action in mdp.states_actions[mdp.states[i]]:
                transitions=mdp.transitions[action]
                if mdp.states[j] in transitions.columns:
                    try:
                        pij=transitions.at[mdp.states[i],mdp.states[j]]
                        if not(np.isnan(pij)):
                            sum_over_probab_transitions_with_Y=sum_over_probab_transitions_with_Y + pij*model.Y[i,a]  
                    except:
                        pij=0.0
    return(sum_over_actions_X + sum_over_actions_Y - sum_over_probab_transitions_with_Y == model.ai_s[j])
model.Constraint2 = pe.Constraint(model.states, rule = avg_cost_mdp_dual_constraint2)
# model.Constraint2.pprint()

#------ Objective Function ------

def obj_rule(model):
    return sum(model.cost[s,a] * model.X[s,a] for s in model.states for a in model.actions)
model.OBJ = pe.Objective(rule = obj_rule, sense = pe.maximize)
# model.OBJ.pprint()

#------ Solve LP ------

solver = pe.SolverFactory('gurobi') # Specify Solver
results = solver.solve(model, tee = False, keepfiles = False)
print("\n\nStatus:", results.solver.status)
print("Termination Condition:", results.solver.termination_condition)

# --------- POST-PROCESSING -------------------

states=[]
actions=[]
dual_vals=[]
for s in model.states:
    for a in model.actions:
        try:
            if (model.X[s,a].value != 0 and model.X[s,a].value != None) or (model.Y[s,a].value != 0 and model.Y[s,a].value != None):
                states=np.append(states,mdp.states[s])
                actions=np.append(actions,mdp.actions[a])
                dual_vals=np.append(dual_vals,np.round(model.X[s,a].value,10))
        except:
            print('Suspicious value at state %d action %d'%(s,a),model.X[s,a].value)
            continue
df=pd.DataFrame.from_dict(dict(state=states,action=actions,dual_val=dual_vals))
print("\n\nObjective function value: ", model.OBJ())
print(df[['state','action','dual_val']])