import scipy
import pandas
import pdb
#import csv

def createInputMatrix(fileName):
    fromToData = pandas.read_csv(fileName)
    node = fromToData.Node.unique()
    fromNode = []
    toNode = []
    probabRequest = []
    cumProbabRequest = []
    
    for i, rowdata in fromToData.iterrows():
        '''Append the From and To nodes'''
        tempFromNode = scipy.chararray(len(node),itemsize=len(node[i]))
        tempFromNode[:] = node[i]
        fromNode.extend(tempFromNode)
        toNode.extend(node)
            
        '''Append the request probability for each From-To pair'''
        row = scipy.array(rowdata)
        probabRequest.extend(row[1:])
     
    for i in range(len(probabRequest)):
        if i==0:
            cumProbabRequest.append(probabRequest[i])
            prevSum = cumProbabRequest[i]
        elif probabRequest[i]!=0:
            cumProbabRequest.append(prevSum+probabRequest[i])
            prevSum = cumProbabRequest[i]
        else:
           cumProbabRequest.append(0.0) 
                
              
    '''Create and Save the Input Data''' 
    cumProbabRequest = scipy.array(cumProbabRequest,float)     
    inputFromToMatrix = scipy.transpose([fromNode,toNode,probabRequest,
                                                         cumProbabRequest])
    inputFromToData = pandas.DataFrame(inputFromToMatrix , columns = ['From',
                                                    'To','Request Probability',
                                                                'Cumulative'])
    pandas.DataFrame.to_csv(inputFromToData,'inputFromToData.csv')
    #pdb.set_trace()
    return inputFromToData

if __name__ == '__main__' :
    inputFromToData = createInputMatrix('Sample_From_To.csv')
    #print inputFromToData