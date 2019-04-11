import scipy
import pandas
import pdb
#import csv
import CreateInputMatrix

#arrData = pandas.read_csv('Sample_Arrival_Rate.csv')
#pdb.set_trace()

def simulateArrivals(arr_Data,totalArrs): 
    #pdb.set_trace()
    arrData = arr_Data[arr_Data['Cumulative'] != 0] 
    #pdb.set_trace() 
    num=0
    #arrNum = []
    #arrRate = []
    #arrProbab = []
    start=[]
    end=[]
    intArrTime = []
    intArrTime = []
    From = []
    To = []
    
    for i in range(len(arrData)):
        if i==0:
            start.append(0.0)
        else:
            start.append(end[i-1])
        end.append(float(arrData.Cumulative.values[i]))
    #pdb.set_trace()
    
    #while sum(intArrTime)<=1: #Generating requests for 1 hour
    while sum(intArrTime)<=20: #Generating requests for 10 hours
    #while sum(intArrTime)<=100: #Generating requests for 100 hours
    #while sum(intArrTime)<=1000: #Generating requests for 1000 hours
    #while sum(intArrTime)<=20: #Generating requests for 20 hours
    #while sum(intArrTime)<=24: #Generating requests for 24 hours
        
        '''Generate a random inter-arrival timme'''
        unifProb = scipy.random.uniform()
        time = scipy.log(unifProb)/(-totalArrs)
        if round(time,4)>0.0:
            intArrTime.append(round(time,4))
            
            '''Select another uniformly random number between 0 and 1 for 
            generating random request'''
            unifProb = scipy.random.uniform()
            
            '''Classify this request'''
            
            for i in range(len(arrData)):                    
                if (unifProb>start[i] and unifProb<=end[i]): 
                    From.append(str(arrData.From.values[i]))
                    To.append(str(arrData.To.values[i]))           
                    #arrProbab.append(float(arrData.ArrivalProbab.values[i]))
                    #arrRate.append(float(arrData.ArrivalRate.values[i]))
                    #arrProbab.append(float(arrData.Cumulative.values[i]))
                    num+=1
                    break
    #pdb.set_trace()        
    arrData = scipy.transpose([From,To,intArrTime]) #,arrRate,arrProbab
    arrivalData = pandas.DataFrame(arrData , columns = ['From','To',
                                                        'Time']) #Time is the inter-arrival time
                                            #,'ArrRate(perHour)','ArrProbab'
    #print arrivalData
    filename = 'FabSimulation_Arrivals_20hrs_'+str(totalArrs)+'arrs_cc0.5_case2.csv'
    pandas.DataFrame.to_csv(arrivalData,filename)
    #pdb.set_trace()
    return filename


if __name__ == '__main__' :
    '''Sample in Matrix Form?'''
    sampleLattice = False
    fabNetwork = False
    fab_hetero2 = True
    
    if sampleLattice:
        arrData = CreateInputMatrix.createInputMatrix('Sample_From_To_LatticeNodes.csv')
    elif fabNetwork:
        arrData = pandas.read_csv('Sample_From_To.csv')
    elif fab_hetero2:
        arrData = pandas.read_csv('FabSimulation_inputFromToData_cc0.5_case2.csv')
    else:
        arrData = pandas.read_csv('FabSimulation_inputFromToData_cc0.5.csv') 
    #pdb.set_trace() 
    
    '''We will choose "totalArrs" (i.e. total number of arrivals per hour),
       such that lambda/mu*C = ~0.98, i.e. we have a stable system'''
                                                                                                                                                                                                                                                      
    totalArrs = 550 #Total number of arrivals per hour
    print simulateArrivals(arrData,totalArrs)
    
    