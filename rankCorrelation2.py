import scipy
import pandas
import pdb
import distance
from itertools import permutations


def hamming_identity(rank_i,i):
    if rank_i == i:
        return 1
    else:
        return 0


def sgn(k):
    if k>0:
        return 1
    elif k<0:
        return -1
    else:
        return 0


def hamming(rankfile,verbose=True):
    
    '''Calculating the hamming distance between ranks'''
    rankFile = pandas.read_csv(rankfile) if isinstance(rankfile, basestring) else pandas.DataFrame.from_dict(rankfile)
    columnNames = scipy.array(rankFile.columns)
    rankTypes = columnNames[1:]
    d1=[] #array for storing hamming distances
    rankpair=[]
    for r1 in range(int(len(rankTypes)-1)):
        for r2 in scipy.arange(r1+1,len(rankTypes)): 
            d1.append(distance.hamming(rankFile[rankTypes[r1]],rankFile[rankTypes[r2]]))
            rankpair.append(rankTypes[r1] + ', ' + rankTypes[r2])
    

    '''Calculate Ms = max(hamming distances for all permutation of ranks 
    mu(i) and nu(i)), i.e., the maximum hamming distance between all
    permutations of ranks'''
    t = len(rankFile)
    mu = scipy.arange(1,t+1)
    Ms = t #This hold only for hamming equation 
    #print "Ms is: ", Ms, "selected from hamming distances: ",permuted_hamming_distances
    
    cs = t-1 #average hamming distance
    alpha = [] #array for strpring rank correlations
    for d in d1:
        alpha.append(1 - (2*float(d)/Ms))
    M = scipy.repeat(Ms,len(rankpair))    
    C = scipy.repeat(cs,len(rankpair))
    hamming_df = pandas.DataFrame(scipy.transpose([rankpair,d1,C,M,alpha]))
    hamming_df.columns = ['Ranks Compared','Hamming Distance',"Average Distance","Maximum Distance","Hamming Correlation"]
    hamming_df.to_csv('Hamming_Distance.csv')
    if isinstance(rankfile, basestring):     
        hamming_df.to_csv('Hamming_Distance.csv')
    else:
        print('\n\n Hamming Rank Correlation:')
#        print(hamming_df)
        if verbose:
            print(hamming_df)
        else:
            print(hamming_df["Hamming Correlation"])
            

def spearman(rankfile,verbose=True):
    '''Calculating the spearman distance between ranks'''
    rankFile = pandas.read_csv(rankfile) if isinstance(rankfile, basestring) else pandas.DataFrame.from_dict(rankfile)
    columnNames = scipy.array(rankFile.columns)
    rankTypes = columnNames[1:]
    d1=[] #array for storing spearman distances
    rankpair=[]
    alpha=[] #array for strpring rank correlations
    t = len(rankFile)
    cs = t*(t**2 - 1)/12 #average spearman distance
    Ms = 2*cs #maximum spearman distance between all permutations of ranks
    for r1 in range(int(len(rankTypes)-1)):
        for r2 in scipy.arange(r1+1,len(rankTypes)): 
            rankpair.append(rankTypes[r1] + ', ' + rankTypes[r2])
            d = float(scipy.sum((rankFile[rankTypes[r1]]-rankFile[rankTypes[r2]])**2)/2)
            d1.append(d)
            alpha.append(1 - (2*d/Ms))            
    C = scipy.repeat(cs,len(rankpair))
    M = scipy.repeat(Ms,len(rankpair))
    spearman_df = pandas.DataFrame(scipy.transpose([rankpair,d1,C,M,alpha]))
    spearman_df.columns = ['Ranks Compared','Spearman Distance',"Average Distance","Maximum Distance","Spearman Correlation"]
    if isinstance(rankfile, basestring):    
        spearman_df.to_csv('spearman_Distance.csv') 
    else:
        print('\n\n Spearman Rank Correlation:')
        if verbose:
            print(spearman_df)
        else:
            print(spearman_df["Spearman Correlation"])
           

def kendall(rankfile,verbose=True): 
    '''Calculating the spearman distance between ranks'''
    rankFile = pandas.read_csv(rankfile) if isinstance(rankfile, basestring) else pandas.DataFrame.from_dict(rankfile)
    columnNames = scipy.array(rankFile.columns)
    rankTypes = columnNames[1:]
    d1=[] #array for storing spearman distances
    rankpair=[]
    alpha=[] #array for strpring rank correlations
    t = len(rankFile)
    cs = t*(t - 1)/2 #average spearman distance
    Ms = 2*cs #maximum spearman distance between all permutations of ranks
    for r1 in range(int(len(rankTypes)-1)):
        for r2 in scipy.arange(r1+1,len(rankTypes)): 
            rankpair.append(rankTypes[r1] + ', ' + rankTypes[r2])
            d = 0.0
            for j in range(len(rankFile[rankTypes[r1]])):
                for i in range(j):
                    mu_j = rankFile[rankTypes[r1]][j]
                    mu_i = rankFile[rankTypes[r1]][i]
                    nu_j = rankFile[rankTypes[r2]][j]
                    nu_i = rankFile[rankTypes[r2]][i]                
                    d += 1 - sgn(mu_j - mu_i)*sgn(nu_j - nu_i)
            d1.append(d)
            alpha.append(1 - (2*d/Ms))            
    C = scipy.repeat(cs,len(rankpair))
    M = scipy.repeat(Ms,len(rankpair))
    kendall_df = pandas.DataFrame(scipy.transpose([rankpair,d1,C,M,alpha]))
    kendall_df.columns = ['Ranks Compared','Kendall Distance',"Average Distance","Maximum Distance","Kendall Correlation"]
    kendall_df.to_csv('kendall_Distance.csv')
    if isinstance(rankfile, basestring):    
        kendall_df.to_csv('kendall_Distance.csv') 
    else:
        print('\n\n Kendall Rank Correlation:')
        if verbose:
            print(kendall_df)
        else:
            print(kendall_df["Kendall Correlation"])
    
if __name__ == '__main__' :
#    rankFile = 'rankFileData2.csv' #Make all rank files using the format of this .csv file in directory: C:\Users\Shreya\Dropbox\Shreya\Research\Samsung2\Samsung2_data\Data 1 and 2\Ranking
#    rankFile = 'rankFileData1.csv'
    rankFile = 'rankFileData2.csv' #'Binary_Count_Route_Ranks.csv'
#    hamming(rankFile)
    rankFile = dict(items=['a','b','c','d','e','f','g','h','i','j'],
                    r1=[1,2,3,5,4,7,6,8,9,10],
                    r2=[2,1,3,6,7,4,5,9,8,10])
    rankFile = dict(items=['a','b','c','d','e','f','g'],
                    r1=[1,2,3,5,4,7,6],
                    r2=[2,1,3,6,7,4,5])
    spearman(rankFile,verbose=False)
    kendall(rankFile,verbose=False)
    hamming(rankFile,verbose=False)
    
    