import scipy
import pandas
import pdb
#import csv


def create_dictionary(relationshipFile):
     relations = pandas.read_csv(relationshipFile)
     cols = scipy.array(relations.columns)
     rows = scipy.array(relations[cols[0]])
     #pdb.set_trace()
     bc = {}
     
     for c in range(len(cols)-1):
         lowerValues=[]
         for r in range(len(rows)):
             #pdb.set_trace()
             if not scipy.isnan(relations.iloc[r][c+1]):
                #pdb.set_trace()
                lowerValues.append(rows[r])
             
         bc[cols[c+1]]=lowerValues
     #pdb.set_trace()
     return bc    
             

def rank(bc):
    bckeys = bc.keys()
    ranked={}
    
    for k in bckeys:
        #pdb.set_trace()
        '''First we check if anything has been ranked yet or is this a new process:
             I. If the process is new, k gets rank 1 and it's values get rank 2'''
        if len(ranked.values()) == 0:
            ranked[k]=1
            for v in bc[k]:
                ranked[v] = ranked[k]+1 
        else:
            '''II. If the process not new then we proceed as follows:
            
            2. If k has already been ranked then we adjust it's ranking depending on any new found relationships'''
            if k in ranked.keys():
                #pdb.set_trace() 
                '''
                - k will stay at it's place and the rank of all it's values
                  having the same rank as k, will increase by 1. 
                - Additionally, any value not ranked yet or ranked as 'i' will get
                  a rank 1 lower than that of k'''
                for v in bc[k]:
                    if v in ranked.keys():
                        if ranked[v]=='i' or ranked[v]<=ranked[k]:
                            ranked[v]+=1
                    else:
                        ranked[v]=ranked[k]+1
                #pdb.set_trace()
                
            elif len(set.intersection(set(bc[k]), set(ranked.keys()))) > 0:
                '''3. If k has not already been ranked then:
                - We give it a rank 1 higher than the it's highest ranked value                
                - In the case where all of k's values are ranked as 'i', we rank
                  k as 1 and all it's values with rank i as 2'''
                #pdb.set_trace()
                ranked_i = [vi for vi,r in ranked.items() if r=='i']
                if  set(bc[k])<set(ranked_i):
                    ranked[k]=1
                else:
                    if min([ranked[routes] for routes in bc[k] if routes in ranked]) > 1:
                        ranked[k] = min([ranked[routes] for routes in bc[k] if routes in ranked]) -1
                    else:                        
                        for lowerRoutes in ranked.keys():
                            ranked[lowerRoutes]+=1 
                        ranked[k] = 1                      
                for v in bc[k]:
                    if v not in ranked.keys() or ranked[v]=='i':
                        ranked[v] = ranked[k]+1 
                #pdb.set_trace()    
            else:
                #pdb.set_trace()
                '''3. If k and it's any of it's values have not yet been ranked 
                      then we rank k as 1 and all it's values as 2'''
                #pdb.set_trace() 
                ranked[k]=1
                for v in bc[k]:
                    ranked[v] = ranked[k]+1
                    
        #pdb.set_trace()
        print k
        print bc[k]
        print ranked
        print '\n'            
    return ranked         

            
    
if __name__ == '__main__' :
    #bc1 = {'C':['B','D'],'B':['D'],'A':['B','C']}
    #bc2 = {'C':['B','D','A'],'B':['D'],'A':['B','D']}
    #bc3 = {}
    #levels = ['A','B','C','D']
    
    climits=[95,99]
    for cl in climits:
        relationshipFiles_firstName = ['tukey_D1_Step1','tukey_D1_Step2','tukey_D1_EQPcombos','tukey_D2_Step1','tukey_D2_Step2','tukey_D2_EQPcombos']
        #relationshipFile='tukeyD1_step1_matrix.csv'
        for fileName in relationshipFiles_firstName:
            relationshipFile = fileName+'_matrix'+str(cl)+'.csv'       
            bc = create_dictionary(relationshipFile)
            print rank(bc)
            rankdf = pandas.DataFrame(rank(bc).items())
            rankdf.columns = ['EQP','Rank']
            rankdf.to_csv('Rank'+'_'+relationshipFile)
        
        