"""
AUTHOR: Shreya Gupta
Created: 12/27/2015
Last Revised: 12/30/2015

"""

import scipy
import pandas
import pdb

def cycleTime_stats(database):
    i=0 #initial index
    w=1 #wait time index
    p=2 #pickup time index
    l=3 #load time index
    d=4 #delivery time index
    u=5 #unload time index
    load_unload_time = 0.0028 #10 seconds
    
    
    lots = database['Lot'].unique()   
    cycle_time=[]
    #pos_cycle_time=[]
    service_time=[]
    wait_time=[]
    pickup_time=[]
    delivery_time=[]
    for lot in lots:
        #temp = database[database['Lot']==lot]
        #event_Times = scipy.array(temp['Time'])
        event_Times = scipy.array(database[database['Lot']==lot]['Time'])*3600
        #if len(temp['Time'])>=2:
        if len(event_Times)>=2:
            '''Wait Time'''
            #pdb.set_trace()            
            if event_Times[w]-event_Times[i]>=0:
                wait_time.append(event_Times[w]-event_Times[i])
            else:
                '''\nNegative Time Interval\n'''
            
            #if len(temp['Time'])>=3:
            if len(event_Times)>=3:
                if event_Times[p]-event_Times[w]>=0:
                    pickup_time.append(event_Times[p]-event_Times[w])
                else:
                    '''\nNegative Time Interval\n'''
                    
                if len(event_Times)>=5:
                    if event_Times[d]-event_Times[l]>=0:
                        delivery_time.append(event_Times[d]-event_Times[l])
                    else:
                        '''\nNegative Time Interval\n'''
                        
                    if len(event_Times)>=6:
                        #pdb.set_trace()
                        #if (event_Times[p]-event_Times[w])+load_unload_time*2<=18.18:
                        #    pdb.set_trace()
                        if event_Times[u]-event_Times[w]>=0:
                            service_time.append((event_Times[u]-event_Times[w]))
                        if event_Times[u]-event_Times[i]>=0:
                            cycle_time.append((event_Times[u]-event_Times[i]))
                           
        
    avgWaitTime = round(scipy.average(wait_time),2)
    stdWaitTime = round(scipy.std(wait_time),2)
    avgPickupTime = round(scipy.average(pickup_time),2)
    stdPickupTime = round(scipy.std(pickup_time),2)
    avgDeliveryTime = round(scipy.average(delivery_time),2)
    stdDeliveryTime = round(scipy.std(delivery_time),2)
    avgCycleTime = round(scipy.average(cycle_time),2)#round(scipy.average(pos_cycle_time),2)
    stdCycleTime = round(scipy.std(cycle_time),2)#round(scipy.std(pos_cycle_time),2)
    avgServiceTime = round(scipy.average(service_time),2)
    stdServiceTime = round(scipy.std(service_time),2)

    #return scipy.array(cycle_time),scipy.array(pos_cycle_time),scipy.average(pos_cycle_time),pos_std
    return avgWaitTime,stdWaitTime,avgPickupTime,stdPickupTime,avgDeliveryTime,stdDeliveryTime,avgCycleTime,stdCycleTime,avgServiceTime,stdServiceTime

        
def utilization_stats(database,simTime):
    #pdb.set_trace()        
    vehs = database['Vehicle_Number'].unique()[1:]
    util=[]
    for veh in vehs:
        vehBusyTime=0.0
        veh_db = database[database['Vehicle_Number']==veh]
        #pdb.set_trace()
        lots = veh_db['Lot'].unique()   
        for lot in lots:
            eventTimes = scipy.array(veh_db[veh_db['Lot']==lot]['Time'])
            #pdb.set_trace()
            if len(eventTimes)==5: 
                vehBusyTime+=max(eventTimes)-eventTimes[0]
            else:
                vehBusyTime+=simTime-eventTimes[0] 
            #lot_db = veh_db[veh_db['Lot']==lot]
            ##pdb.set_trace()
            #maxTime = max(scipy.array(lot_db['Time']))
            #minTime = min(scipy.array(lot_db['Time']))
            #vehBusyTime+=maxTime-minTime
            ##pdb.set_trace()
        #pdb.set_trace()    
        util.append(round(vehBusyTime/simTime,2))
        #pdb.set_trace()
    #pdb.set_trace()
    return round(scipy.average(util)*100,2)
    
    
def return_stats(databaseName,simTime):
    #pdb.set_trace()
    database = pandas.read_csv(databaseName)
    #pdb.set_trace()
    avgWaitTime,stdWaitTime,avgPickupTime,stdPickupTime,avgDeliveryTime,stdDeliveryTime,avgCycleTime,stdCycleTime,avgServiceTime,stdServiceTime = cycleTime_stats(database)
    util = utilization_stats(database,simTime)
    #pdb.set_trace()
    return avgWaitTime,stdWaitTime,avgPickupTime,stdPickupTime,avgDeliveryTime,stdDeliveryTime,avgCycleTime,stdCycleTime,avgServiceTime,stdServiceTime,util
    
    
    
if __name__ == '__main__' :
    
    inputNum = 0
    inputDB = pandas.read_csv('inputFileName430.csv')
    #pdb.set_trace()
    
    simTime=20.0
    
    algorithm=[]
    averageWaitTime=[]
    averagePickupTime=[]
    averageDeliveryTime=[]
    averageServiceTime=[]
    averageCycleTime=[]
    stddevWaitTime=[]
    stddevPickupTime=[]
    stddevDeliveryTime=[]
    stddevServiceTime=[]
    stddevCycleTime=[]
    vehicle_utilization=[]
    
    
    while inputNum<36:
        #databaseName = 'L4_FabSimulation_Status_20.0hrs_550arrs_FabNetwork_Preservation_noETA_RBigM2_hetero0.5.csv'#'L3_FabSimulation_Status_5.0hrs_700arrs_FabNetwork_PFIFO_RLinear3_hetero0.5.csv'#'L3_FabSimulation_Status_1.0hrs_100arrs_FabNetwork_Preservation_RBigM3_hetero0.5.csv'
        databaseName = inputDB['algo'][inputNum]
        avgWaitTime,stdWaitTime,avgPickupTime,stdPickupTime,avgDeliveryTime,stdDeliveryTime,avgCycleTime,stdCycleTime,avgServiceTime,stdServiceTime,util = return_stats(databaseName,simTime)

        
        algorithm.append(databaseName)             
        averageWaitTime.append(avgWaitTime)
        averagePickupTime.append(avgPickupTime)
        averageDeliveryTime.append(avgDeliveryTime)
        averageServiceTime.append(avgServiceTime)
        averageCycleTime.append(avgCycleTime)
        stddevWaitTime.append(stdWaitTime)
        stddevPickupTime.append(stdPickupTime)
        stddevDeliveryTime.append(stdDeliveryTime)
        stddevServiceTime.append(stdServiceTime)
        stddevCycleTime.append(stdCycleTime)
        #trafficIntensity.append(round(rateOfArrival*(avgServiceTime/3600)/C,2))
        vehicle_utilization.append(util)
        
        inputNum+=1
    
    statistics = scipy.transpose([algorithm,
                                    averageWaitTime,averagePickupTime,averageDeliveryTime,
                                    averageServiceTime,averageCycleTime,stddevWaitTime,
                                    stddevPickupTime,stddevDeliveryTime,stddevServiceTime,
                                    stddevCycleTime,vehicle_utilization])    

    statisticsData = pandas.DataFrame(statistics, columns = [
                        'algorithm','averageWaitTime',
                        'averagePickupTime','averageDeliveryTime',
                        'averageServiceTime','averageCycleTime',
                        'stddevWaitTime','stddevPickupTime','stddevDeliveryTime',
                        'stddevServiceTime','stddevCycleTime',
                        'vehicle_utilization'] )

    with open('statistics_new42.csv', 'a') as f:(statisticsData).to_csv(f, header=False)

    