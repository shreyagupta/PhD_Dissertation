"""
AUTHOR: Shreya Gupta
Created: 04/04/2015
Last Revised: 01/07/2016

"""

"""
Basic OHTC simulation.

Keywords:
    - P/D: Pickup and Delivery

Covers:
    - Waiting for other lots to be delivered
    - Resources: Limited number of vehicles available for delivery
    - Extracts the network from a .csv file. This .csv file is created using 
      another code
    - Nodes: Transits between each pair of nodes in a network
    - Uses Dijkstra's algorithm to calculate the shortest path between any two
      nodes
    - Uses Poisson arrivals created by another external code and saved in a 
      .csv file
    - Runs the simulation for 1 hour
    - Display vehicle number delivering a particular lot
    - Calculates which edge every vehicle is on at any point of time
    - Calculates number of vehicles on every edge at any point of time
    - Dynamically updates the network distances
    - Updates the network distances dynamically depending on the number of
      vehicles on a given edge at any point of time, i.e. traffic information 
    - Uses updated network distances to update every vehicle's travel times 
      towards its current destination, It does this each time a vehcile moves 
      onto a new edge but usiign the updated network distance for that edge at 
      that time
    - DD1
    - DD2

Does not cover:      
    - 
      
Not Considered yet in our model:
    - Create Poisson pickup-delivery requests within the simulator

Scenario:
  Starting at time 0.00000 hours, the OHTC is receiving requests from the 
  nodes   of the system. These requests involve picking up a lot from the 
  source node   and delivering it at the destination node. Thus, 5 types of 
  times are involved   here (mentioned in the order they are incurred by a 
  vehicle):
      1. Waiting for a request to occur (not considered yet)
      2. Travelling to source node
      3. Loading at destination node
      4. Travelling to destination node
      5. Unloading at destination node
  Lot pick up requests arrive at the OHTC at random times. If a vehicle is 
  free, it starts moving towards the source node to pickup and deliver the 
  corresponding lot. Otherwise, the lot waits for the first available vehicle.
  
Unit of Time: Hours

Simulation Time: 1 hour
Number of Vehicels: 15


to do:
    - calculate the percentage of time each vehicle is idle 
"""

# SOURCE CODE:

import scipy
import pandas
import random
#import simpy
#import sys
import time
#import networkx
import pdb
import matplotlib.pyplot
#import matplotlib.image
#import FabConstructor
import FabConstructorDDD4
import test_stats2
import Arrival_Simulator


RANDOM_SEED = 42    #Not sure what this does. It was present in some examples so I included it as it is
NUM_VEHICLES = 15   # Number of vehicles (machines) in the OHTC
#SERVICETIME = 5    # Hours taken to deliver a lot
T_INTER = 0.001     # Create a pickup request every ~0.1 hours
start_time = 0.0000 #The time at which the simulation starts
event_time=0.0 #passing time in hours
prevEventTime=0.0
L_U_time = 0.0028
Qt = 0 
cycleTime=0.05 # 3 mins = 180 seconds
cycleLastTime = 0.0
#reservationETA_threshold = 1000.0#0.012#0.0083 # reserve when a veh ETA is 30 seconds
#Route_algo = 'Linear' #'DDD': Base | 'Linear': Add a linear but 1.5 times cost say | 'BigM': Add a very large cost to make jammed edge infeasible to travel on
#Pickup_algo = 'FIFO' #FIFO, nearest, reservation
#arrival_probab = 'hetero'#'homo' #'hetero': heterogeneous, 'homo': homogeneous
#max_Veh_Limit = 3 # max_Veh_Limit = 2,3,4,5
HeteroArrivalSkewRatio = 0.5
simulation_time = 1.0#20 #Set at values 0.1, 1.0, 5.0, 10.0, 20.0, 24.0, 100.0, 1000.0



lotname=[]
source=[]
destination=[]
activity=[]
#activity_time=[]
#route=[]
Qt_array = []
#travelTime=[]
time=[]
Qt_times=[]
vehicle=[]
vehicleLocation=[]
#vehNumber=scipy.arange(NUM_VEHICLES)+1
vehNumber=0
vehName='none'
#vehLoc='A'
initVehLoc=['A1']*NUM_VEHICLES
#vehLocation=[initVehLoc]*NUM_VEHICLES
#vehStatus=['F']*NUM_VEHICLES #F: Free, O: Occupied
#lotWithVeh=useVeh=pickUpLot=UnloadLot=newVehLoc=scipy.zeros_like(vehLocation,dtype=int)
number_of_Vehicles=[] #Stores the number of vehicles  on different edges 
                      #everytime a routing decision is made
#cycleTime_lastTime=[] #Stores the last time cycle time was recorded for 
                    #performing cyclical calculations like vehicle reservations

edgeDatatoDebug=[]
numvehsToDebug=[]

'''lot dictionaries'''
lotSourceLoc={} #Stores the lot pickup location
lotDestLoc={} #Stores the lot delivery location
lotVeh={} #Stores which vehicle is servicing a particular lot
lotRequestOccurTime={} #Stores the time a particular lot occurs for the first time in the system
lotServiceBeginsTime={} #Stores the time at whcih an available vehicle starts moving towards a request
lotsInQueue_distancefromVeh={}
lotsInQueue_additionTime={}
lotsInQueue_source={} #Stores the location of all the lots waiting in to be serviced
#lotsInQueue_unreserved={} #Lots in queue that do not have a vehicle reserved for them yet
#lotsInQueue_reserved_GC={} #Lots in queue that have a vehicle reservation and this reserved vehicle will reach
                           #its destination in more than 5mins
#lotsInQueue_reserved_LEC={} #Lots in queue that have a vehicle reservation and this reserved vehicle will reach
                            #its destination in <= 5mins
#lotWaitTime={} #Stores the time a particular lot has to wait to to be serviced
#(Note: Service begins when an available vehicle starts moving from its current location to pickup a lot)
lotPickupTravelTime={} #Stores, for every lot, the time taken by an assigned vehicle to reach it for pickup
lotPickupRoute={}
lotDeliveryTravelTime={} #Stores, for every lot, the time taken to transport it from source to destination
lotDeliveryRoute={}

'''vehicle dictionaries'''
veh_nextEventTime={}
veh_eventTimeUpdate={}
vehIdleStartTime={}
vehIdleEndTime={}
vehIdleTime={}
vehLot={} #Stores which lot is being serviced by a particular vehicle
vehReservedByLot={} #Stores which the lot which has reserved a vehicle
vehDeliverTime={} #Stores the last time a particular vehicle delivered a lot
vehLoadTime={} #Stores the last time a particular vehicle loaded a lot
vehRouteTime={} #Stores what a vehicle is doing.
#vehDeliveryTime={}
vehETA={} #The est=/loimated time of a vehicle to the assigned lot's destination-   
#W: Waiting for request, P: Picking up a request, D:Delivering a Request
#L: Loading a Request, U: Unloading a request

# Earlier definition of vehStatus
#Stores the last route a particular vehicle took and the time
                #the vehicle will take to travel on it
vehLoc={}
vehStatus={} #S: Still, M: Moving
vehPrevEdge={}
vehPreviousTime={} #The last time a vehicle started its journey towards a destination
prevVehEdge={} #Stores the edge a vehicle is on at a point of time before the current point of time
currVehEdge={}
prevVehEdgeTime={}
currVehEdgeTime={}

#edgeDatatoDebug={}

Qt_array.append(Qt)
Qt_times.append(0.0)

def initialize():
    for i in range(NUM_VEHICLES):
        vehLoc['veh'+str(i+1)]=initVehLoc[i]
        vehPreviousTime['veh'+str(i+1)]=0.0
        vehRouteTime['veh'+str(i+1)]=['None',0.0]
        vehStatus['veh'+str(i+1)]='W'
        prevVehEdge['veh'+str(i+1)]='None'
        currVehEdge['veh'+str(i+1)]='None'
        prevVehEdgeTime['veh'+str(i+1)]=0.0
        currVehEdgeTime['veh'+str(i+1)]=0.0
        vehIdleStartTime['veh'+str(i+1)]=0.0
        vehIdleTime['veh'+str(i+1)]=0.0
        vehETA['veh'+str(i+1)]=0.0
        veh_nextEventTime['veh'+str(i+1)]='None'
        veh_eventTimeUpdate['veh'+str(i+1)]=True
        
        '''empty all dictionaries'''
    vehPrevEdge.clear()
    vehIdleEndTime.clear()
    vehLot.clear()
    vehReservedByLot.clear()
    vehDeliverTime.clear()
    vehLoadTime.clear()
    lotSourceLoc.clear()
    lotDestLoc.clear()
    lotVeh.clear()
    lotRequestOccurTime.clear()
    lotServiceBeginsTime.clear()
    lotsInQueue_distancefromVeh.clear()
    lotsInQueue_additionTime.clear()
    lotsInQueue_source.clear()
    lotPickupTravelTime.clear()
    lotPickupRoute.clear()
    lotDeliveryTravelTime.clear()
    lotDeliveryRoute.clear()
    
    
    del lotname[:]
    del source[:]
    del destination[:]
    del activity[:]
    del Qt_array[:]
    del time[:]
    del Qt_times[:]
    del vehicle[:]
    del vehicleLocation[:]
    del number_of_Vehicles[:]
    
    vehNumber=0
    vehName='none'
    Qt_array.append(Qt)
    Qt_times.append(0.0)


def load_unload_time(fromNode, toNode):
    """The load/unload time is calculated for a source and destination node pair.
    
    We assume this to be a constant.
    
    """
    return L_U_time #10 seconds = ~0.0028 hours


class Fab(object):
    """The OHTC has a limited number of vehicles (``NUM_VEHICLES``) to serve its
    lots when pickup & delivery requests arrive.

    Lots have to request a pickup by one of the vehicles. When they get one, the
    vehicle loads (``load_unload_time'' hours) them and starts moving towards 
    the delivery destination (``deliverytime'' hours). The vehicle unloads 
    (``load_unload_time'' hours) the lot at the destination.

    """
    
    def __init__(self, num_vehicles):
        self.vehicles = num_vehicles
    
    def pickup(self,passingTime, lotName, vehNum,vehlocation, fromNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus,numvehs,Pickup_algo,Route_algo,max_Veh_Limit):
        """The delivery processes. The vehicle delivers a lot it has loaded."""        
        '''first we calculate the progress of the vehicles so far'''        
        '''now we yield the process'''
        print('%s is being approached by a vehicle at %.5f hrs from %s' % (lotName, passingTime,fromNode))
        #pdb.set_trace()
        PickupTimeToTravel, PickupRouteTaken = Route_Time_algorithm(vehlocation, fromNode, fabnetwork, numvehs, Route_algo,max_Veh_Limit)
        #pdb.set_trace()
        vehRouteTime['veh'+str(vehNum)]=[PickupRouteTaken,PickupTimeToTravel]
        vehPreviousTime['veh'+str(vehNum)]=passingTime
        #activity.append('Reached pickup location')
        #time.append(passingTime)         
        
        #vehStatus['veh'+str(vehNum)]='P'
        lotPickupRoute[lotName]=PickupRouteTaken
        lotPickupTravelTime[lotName]=0.0
        
        #lotServiceBeginsTime[lotName]=passingTime
        #lotWaitTime[lotName] = lotServiceBeginsTime[lotName] - lotRequestOccurTime[lotName]
        try:
            lotsInQueue_source[lotName]
        except KeyError:
            pdb.set_trace()
        #if lotName=='Lot 17':
        #    pdb.set_trace()
            
        del lotsInQueue_source[lotName]
        del lotsInQueue_distancefromVeh[lotName]        
        return PickupTimeToTravel,PickupRouteTaken,vehPreviousTime,vehRouteTime
        
    def delivery(self,passingTime,lotName, vehNum,vehlocation, destNode, fabnetwork,vehRouteTime, vehPreviousTime,vehStatus,numvehs,Pickup_algo,Route_algo,max_Veh_Limit):
        """The delivery processes. The vehicle delivers a lot it has loaded."""
        '''first we calculate the progress of the vehicles so far'''        
        
        '''now we yield the process'''
        #self.deliveryTime, self.deliveryRoute = RouteAndTime_DDD(fromNode, toNode, fabnetwork)
        #return self.deliveryTime, self.deliveryRoute
        print('%s is being delivered by a vehicle at %.5f hrs from %s' % (lotName, passingTime,destNode))
        
        DeliveryTimeToTravel, DeliveryRouteTaken = Route_Time_algorithm(vehlocation, destNode, fabnetwork, numvehs, Route_algo,max_Veh_Limit)

        vehRouteTime['veh'+str(vehNum)]=[DeliveryRouteTaken,DeliveryTimeToTravel]
        vehPreviousTime['veh'+str(vehNum)]=passingTime
        #activity.append('Reached pickup location')
        #time.append(passingTime)         
        
        #vehStatus['veh'+str(vehNum)]='P'
        lotDeliveryRoute[lotName]=DeliveryRouteTaken
        lotDeliveryTravelTime[lotName]=0.0
        
        #lotServiceBeginsTime[lotName]=passingTime
        #lotWaitTime[lotName] = lotServiceBeginsTime[lotName] - lotRequestOccurTime[lotName]
        return DeliveryTimeToTravel,DeliveryRouteTaken,vehPreviousTime,vehRouteTime#0.0001
        
    def load_unload(self, lot, fromNode, toNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus):
        """The loading/unloading processes. The vehicle loads (unloads) a lot at
        the source (destination)."""

        '''first we calculate the progress of the vehicles so far'''        
        '''now we yield the process'''        
        return load_unload_time



def RouteAndTime_DDD(fromNode, toNode, fabnetwork):
    """The delivery time is calculated for a source and destination node pair.
    
    As of now we assume a deterministic shortest path to be delivery time.
    We will improve it to become more dynamic.
    
    """   
    deliveryTime = fabnetwork.shortestPathTimeDDD(fromNode, toNode)
    route = fabnetwork.shortestPathDDD(fromNode, toNode)
    
    return deliveryTime, route 
    

def RouteAndTimeDD1(fromNode, toNode, fabnetwork, numvehs,max_Veh_Limit):
    """The delivery time is calculated for a source and destination node pair.
    
    As of now we assume a deterministic shortest path to be delivery time.
    We will improve it to become more dynamic.
    
    """
    fabnetwork.makeFabDD1(numvehs,maxVehLimit=max_Veh_Limit) #maxVehLimit=2,3,4
    deliveryTime = fabnetwork.shortestPathTimeDD1(fromNode, toNode)
    route = fabnetwork.shortestPathDD1(fromNode, toNode)   
    return deliveryTime, route
    
     
def RouteAndTimeDD2(fromNode, toNode, fabnetwork, numvehs,max_Veh_Limit):
    """The delivery time is calculated for a source and destination node pair.
    
    As of now we assume a deterministic shortest path to be delivery time.
    We will improve it to become more dynamic.
    
    """
    #numvehs[('C','F')]=5
    #numvehs[('E','F')]=2
    #pdb.set_trace()
    fabnetwork.makeFabDD2(numvehs,maxVehLimit=max_Veh_Limit,t2=0.0056) #maxVehLimit=2,3,4 ; t2 = 2*t1 for now
    #pdb.set_trace()
    #if any(tuple(x>2 for x in numvehs.values())):
    #    pdb.set_trace()
    deliveryTime = fabnetwork.shortestPathTimeDD2(fromNode, toNode)
    route = fabnetwork.shortestPathDD2(fromNode, toNode)
    #deliveryTime = fabnetwork.shortestPathTimeDD2('A','F')
    #route = fabnetwork.shortestPathDD2('A','F')
    #if any(tuple(x>2 for x in numvehs.values())):
    #    pdb.set_trace()
    
    return deliveryTime, route 
    

def Route_Time_algorithm(fromNode, toNode, fabnetwork, numvehs, Route_algo,max_Veh_Limit):
    #pdb.set_trace()
    if Route_algo=='Base':
        #pdb.set_trace()
        return RouteAndTime_DDD(fromNode, toNode, fabnetwork)
    elif Route_algo=='Linear':
        #pdb.set_trace()
        return RouteAndTimeDD2(fromNode, toNode, fabnetwork, numvehs,max_Veh_Limit)        
    elif Route_algo=='BigM':
        #pdb.set_trace()
        return RouteAndTimeDD1(fromNode, toNode, fabnetwork, numvehs,max_Veh_Limit)
        


#def num_Vehs_on_Edges(fabnetwork,vehRouteTime,time_now):
#def num_Vehs_on_Edges(fabnetwork,time_now, vehRouteTime,vehPreviousTime):
def num_Vehs_on_Edges(fabnetwork,currVehEdges,time_now):

    numVehsPerEdge={}
    #if float(time_now)>=0.0237:
    #    pdb.set_trace()

    for edge in fabnetwork.fabgraph.edges():
        #if float(time_now)>=0.0237:
        #        pdb.set_trace()
        numVehsPerEdge[edge]=0
    #pdb.set_trace()    

    for veh,edge in currVehEdges.items():
        if edge!='None':
            numVehsPerEdge[edge]+=1                     

    return numVehsPerEdge                    
    

def veh_edges(fabnetwork,time_now,vehRouteTime,vehPrevTime,vehPrevEdge,vehPrevEdgeTime):
    #pdb.set_trace()

    #if float(time_now)>=0.0016  and vehRouteTime['veh2'][0]!='None':
    #    pdb.set_trace()

    vehEdge={}
    vehEdgeTime={}
    
    #pdb.set_trace()
    #if float(time_now)>=1.5178:
    #    pdb.set_trace() 
    
    for veh,Route_and_Time in vehRouteTime.items():
        #pdb.set_trace()
        
        route=Route_and_Time[0] 
        
        #pdb.set_trace()

        #if float(time_now)>=6.8929 and (veh=='veh2'):
        #    print 'We reached here for veh9'
        #    pdb.set_trace() 
        
        #try:
        #    len(route)
        #except TypeError:
        #    pdb.set_trace()
                        
        if route=='None' or len(route)==1:
            #if float(time_now)>=0.0593 and (veh=='veh4'):
            #    pdb.set_trace()
            vehEdge[veh]='None'
            vehEdgeTime[veh]=0.0
            #if float(time_now)>=0.0593 and (veh=='veh4'):
            #    print 'This was executed for veh4'
            #    print vehEdge
            #    pdb.set_trace()
            
        #if float(time_now)>=0.0593 and (veh=='veh4'):
        #    pdb.set_trace()        
                      
        elif round(vehPrevTime[veh],4)==round(time_now,4) and len(route)>1:
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    pdb.set_trace()                
            #pdb.set_trace()
            vehEdge[veh]=(str(route[0]),str(route[1]))
            vehEdgeTime[veh]=fabnetwork.shortestPathTimeDDD(route[0],route[1])
            
        elif vehPrevEdge[veh]=='None':
            #pdb.set_trace()
            vehEdge[veh]='None'
            vehEdgeTime[veh]=0.0
            
        #elif vehPrevEdge[veh]==(str(route[-2]),str(route[-1])) and (round((vehPrevTime[veh]+lotPickupTravelTime[vehLot[veh]]+vehPrevEdgeTime[veh]),4)<=round(time_now,4)):
        elif (vehStatus[veh]=='P') and vehPrevEdge[veh]==(str(route[-2]),str(route[-1])) and (round((vehPrevTime[veh]+lotPickupTravelTime[vehLot[veh]]),4)<=round(time_now,4)):
            #pdb.set_trace()
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    pdb.set_trace()
            vehPrevEdge[veh]='None'
            vehEdge[veh]='None'
            vehEdgeTime[veh]=0.0
            
        #elif (round((vehPrevTime[veh]+lotPickupTravelTime[vehLot[veh]]+vehPrevEdgeTime[veh]),4)<=round(time_now,4)):
        elif (vehStatus[veh]=='P') and (round((vehPrevTime[veh]+lotPickupTravelTime[vehLot[veh]]),4)<=round(time_now,4)):
            #pdb.set_trace()
            #if float(time_now)>=0.0593:
            #    pdb.set_trace()
            for i in range(len(route)-2):
                #pdb.set_trace()
                if vehPrevEdge[veh]==(str(route[i]),str(route[i+1])):
                    vehEdge[veh]=(str(route[i+1]),str(route[i+2]))
                    vehEdgeTime[veh]=fabnetwork.shortestPathTimeDDD(route[i+1],route[i+2])
                    
        elif (vehStatus[veh]=='D') and vehPrevEdge[veh]==(str(route[-2]),str(route[-1])) and (round((vehPrevTime[veh]+lotDeliveryTravelTime[vehLot[veh]]),4)<=round(time_now,4)):
            #pdb.set_trace()
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    pdb.set_trace()
            vehPrevEdge[veh]='None'
            vehEdge[veh]='None'
            vehEdgeTime[veh]=0.0
            
        #elif (round((vehPrevTime[veh]+lotPickupTravelTime[vehLot[veh]]+vehPrevEdgeTime[veh]),4)<=round(time_now,4)):
        elif (vehStatus[veh]=='D') and (round((vehPrevTime[veh]+lotDeliveryTravelTime[vehLot[veh]]),4)<=round(time_now,4)):
            #pdb.set_trace()
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    pdb.set_trace()
            for i in range(len(route)-2):
                #pdb.set_trace()
                if vehPrevEdge[veh]==(str(route[i]),str(route[i+1])):
                    vehEdge[veh]=(str(route[i+1]),str(route[i+2]))
                    vehEdgeTime[veh]=fabnetwork.shortestPathTimeDDD(route[i+1],route[i+2])
    
        
        elif (vehStatus[veh]=='W' or vehStatus[veh]=='L' or vehStatus[veh]=='U'):
            vehPrevEdge[veh]='None'
            vehEdge[veh]='None'
            vehEdgeTime[veh]=0.0
                                    
        else:
            #pdb.set_trace()
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    pdb.set_trace()
            vehEdge[veh]=vehPrevEdge[veh]
            vehEdgeTime[veh]=vehPrevEdgeTime[veh]
            #if float(time_now)>=0.0593 and (veh=='veh9'):
            #    print 'This was executed 2'
            #    print vehEdge
            #pdb.set_trace()
            
        #if float(time_now)>=0.0593 and (veh=='veh4'):
        #    print 'This was executed again for veh4'
        #    print vehEdge
        #    pdb.set_trace()
        #    
                          
    return vehEdge, vehEdgeTime       


#def update_vehRouteTime(fabnetwork, time_now, vehRouteTime, vehPreviousTime, prevVehEdges, currVehEdges):
def update_vehRouteTime(time_now,time_next,fabnetwork, vehLot, prevVehEdge, currVehEdge,currVehEdgeTime,vehStatus,lotPickupTravelTime,lotDeliveryTravelTime):    
    #pdb.set_trace()
    for veh,veh_status in vehStatus.items():                  
        #pdb.set_trace()
        #if round(float(time_next),4)>=1.5178:
        #        pdb.set_trace()
        
        if (veh_status=='P') and (prevVehEdge[veh] != currVehEdge[veh]):
            #pdb.set_trace()
            #if round(float(time_next),4)>=1.5178:
            #    pdb.set_trace()
            lotPickupTravelTime[vehLot[veh]] += currVehEdgeTime[veh]
            #pdb.set_trace() 
            
    
        elif (veh_status=='D') and (prevVehEdge[veh] != currVehEdge[veh]):
            #pdb.set_trace()
            #if round(float(time_next),4)>=1.5178:
            #    pdb.set_trace()
            lotDeliveryTravelTime[vehLot[veh]] += currVehEdgeTime[veh]
            #pdb.set_trace()
        
        #pdb.set_trace()

def veh_ETA_calculator(currVehEdge, fabnetwork, numvehs, vehLot, vehRouteTime):
    #global vehRouteTime
        
    for veh,Route_and_Time in vehRouteTime.items():
        vehDest = Route_and_Time[0][len(Route_and_Time[0])-1]
        try:
            vehLot[veh]
        except KeyError:
            continue 
        lot = vehLot[veh]
        lotDest = lotDestLoc[lot]
        if currVehEdge[veh] != 'None':
            sourceNode = currVehEdge[veh][1]
        else:
            sourceNode=vehLoc[veh]       
        #print veh
        #pdb.set_trace()
        #if veh=='veh5':
        #    pdb.set_trace()

        if vehDest==lotDest: #vehicle is already on delivery route
            #pdb.set_trace()
            vehDeliveryTime, vehDeliveryRoute = Route_Time_algorithm(sourceNode, vehDest, fabnetwork, numvehs, Route_algo,max_Veh_Limit)
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTime_DDD(sourceNode, vehDest, fabnetwork)
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTimeDD1(sourceNode, vehDest, fabnetwork, numvehs)       
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTimeDD2(sourceNode, vehDest,, fabnetwork, numvehs)
            vehETA[veh] = vehDeliveryTime + L_U_time
            #pdb.set_trace()
                      
        else:
            #pdb.set_trace()
            vehLotSourceRouteTime, vehLotSourceRoute = Route_Time_algorithm(sourceNode, vehDest, fabnetwork, numvehs, Route_algo,max_Veh_Limit)
            #vehLotSourceRouteTime, vehLotSourceRoute = RouteAndTime_DDD(sourceNode, vehDest, fabnetwork)
            #vehLotSourceRouteTime, vehLotSourceRoute = RouteAndTimeDD1(sourceNode, vehDest, fabnetwork, numvehs)       
            #vehLotSourceRouteTime, vehLotSourceRoute = RouteAndTimeDD2(sourceNode, vehDest, fabnetwork, numvehs)
            #pdb.set_trace()
            vehDeliveryTime, vehDeliveryRoute = Route_Time_algorithm(lotSourceLoc[lot], lotDest, fabnetwork, numvehs, Route_algo,max_Veh_Limit)
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTime_DDD(lotSourceLoc[lot], lotDest, fabnetwork)
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTimeDD1(lotSourceLoc[lot], lotDest, fabnetwork, numvehs)       
            #vehDeliveryTime, vehDeliveryRoute = RouteAndTimeDD2(lotSourceLoc[lot], lotDest, fabnetwork, numvehs) 
            #pdb.set_trace()
            vehETA[veh] = vehLotSourceRouteTime + vehDeliveryTime + (2*L_U_time)
            #pdb.set_trace()


#def reserve():
#    continue

def nearestLot_reservation(fabnetwork, numvehs, vehReservedByLot,reservationETA_threshold):
    #pdb.set_trace()
    #if 'Lot 532' in lotsInQueue_source.keys():
    #    pdb.set_trace()
    lots_withReservation = []
    for veh, veh_ETA in vehETA.items():
        if veh_ETA<=reservationETA_threshold:
            dist_from_veh={}
            if bool(lotsInQueue_source):
                for lot, lotSource in lotsInQueue_source.items():
                    #if 'Lot 531' in lotsInQueue_source.keys():
                    #    pdb.set_trace()
                    if lot not in lots_withReservation: #reserve only those lots that do not already have a reservation
                        dist, route = Route_Time_algorithm(lotSource, lotDestLoc[vehLot[veh]], fabnetwork, numvehs, Route_algo,max_Veh_Limit)
                        #dist, route = RouteAndTime_DDD(lotSource, lotDestLoc[vehLot[veh]], fabnetwork)
                        #dist, route = RouteAndTime(lotSource, lotDestLoc[vehLot[veh]], fabnetwork, numvehs)       
                        #dist, route = RouteAndTime(lotSource, lotDestLoc[vehLot[veh]], fabnetwork, numvehs)
                        dist_from_veh[lot]=veh_ETA+dist
                if bool(dist_from_veh):
                    #if veh=='veh15':
                    #    pdb.set_trace()
                    lot_selected = min(dist_from_veh, key=dist_from_veh.get)
                    vehReservedByLot[veh] = lot_selected
                    if len([l for l in vehReservedByLot.values() if l==lot_selected])>1:
                       common_vehs = [vehKey for vehKey,lotVals in vehReservedByLot.items() if lotVals==lot_selected]
                       #pdb.set_trace()
                       distances={}
                       for comVehs in common_vehs:
                           dist, route = Route_Time_algorithm(lotSourceLoc[lot_selected], lotDestLoc[vehLot[veh]], fabnetwork, numvehs, Route_algo,max_Veh_Limit)
                           distances[comVehs]=vehETA[comVehs]+dist
                       veh_at_minDist = min(distances, key=distances.get)
                       delete_vehs = [vehKey for vehKey,distKeys in distances.items() if vehKey!=veh_at_minDist]
                       for vehs in delete_vehs:  
                           del vehReservedByLot[vehs]
                       #pdb.set_trace()
                        
                    #if vehReservedByLot[veh] == 'Lot 17': #vehReservedByLot[[veh for veh, lot in vehReservedByLot.items() if lot == selected_lot][0]]
                    #    pdb.set_trace()
                    lots_withReservation.append(min(dist_from_veh, key=dist_from_veh.get))
    #if bool(lotsInQueue):
    #    pdb.set_trace()
    return vehReservedByLot                
                                                

def cycle(time_now, fabnetwork, vehLot, vehRouteTime,currVehEdge, numvehs, vehReservedByLot,reservationETA_threshold):        
    veh_ETA_calculator(currVehEdge, fabnetwork, numvehs, vehLot, vehRouteTime)
    #for veh, ETA in vehETA.items():
    #    if ETA<=reservationETA_threshold:
    #        #reserve()
    #        nearestLot_reservation(fabnetwork, numvehs, vehReservedByLot)
    vehReservedByLot = nearestLot_reservation(fabnetwork, numvehs, vehReservedByLot,reservationETA_threshold)           
    #if bool(lotsInQueue):
    #    pdb.set_trace()
    return vehReservedByLot
    #if time_now>=0.09:
    #    pdb.set_trace()
                         


def manipulations(cycleLastTime, Pickup_algo, time_now, time_next, fabnetwork, vehLot, vehRouteTime, vehPreviousTime,currVehEdge, currVehEdgeTime,prevVehEdge,prevVehEdgeTime,vehStatus,lotPickupTravelTime,lotDeliveryTravelTime,Qt,Qt_array,Qt_times, vehReservedByLot,reservationETA_threshold):
    
    #global cycleLastTime, Pickup_algo
    
    #pdb.set_trace()
    changingTime_prev = round(float(time_now),4)
    #changingTime_curr = round(float(time_now)+ 0.0001,4) 
    #pdb.set_trace()
    #while changingTime_curr<=time_next:
    while round(changingTime_prev,4)<=time_next:
       
        currVehEdge, currVehEdgeTime = veh_edges(fabnetwork,changingTime_prev, vehRouteTime,vehPreviousTime,prevVehEdge, prevVehEdgeTime)
        #if max_Veh_Limit==3 and float(changingTime_prev)>=19.9242 and vehLot['veh12']=='Lot 3596':
        #    pdb.set_trace()        

        numvehs = num_Vehs_on_Edges(fabnetwork,currVehEdge,changingTime_prev)

        #if max_Veh_Limit==3 and float(changingTime_prev)>=19.9242 and vehLot['veh12']=='Lot 3596':
        #    pdb.set_trace()      

        fabnetwork.makeFabDDD(numvehs)  

        update_vehRouteTime(changingTime_prev,round(float(time_now),4),fabnetwork, vehLot,prevVehEdge, currVehEdge,currVehEdgeTime,vehStatus,lotPickupTravelTime,lotDeliveryTravelTime)
        
        #if Qt_times[len(Qt_times)-1] != round(changingTime_prev,0):
        #    if (Qt_times[len(Qt_times)-1]+0.9990<= changingTime_prev) and (Qt_times[len(Qt_times)-1]+1.0010>= changingTime_prev):
        #    #if (Qt_times[len(Qt_times)-1]+1) == changingTime_prev or (Qt_times[len(Qt_times)-1]+1) == round(changingTime_prev,0):      
        #        Qt_array.append(Qt)
        #        Qt_times.append(Qt_times[len(Qt_times)-1]+1)
            #pdb.set_trace()
            
        if time_now==Qt_times[len(Qt_times)-1]+1.0:
            #pdb.set_trace()
            Qt_array.append(Qt)
            Qt_times.append(time_now)
            
 
        if type(currVehEdge['veh3'])==float:
            pdb.set_trace()
        prevVehEdge = currVehEdge
        prevVehEdgeTime = currVehEdgeTime

        '''reservation algorithm'''
        if Pickup_algo in ['reservation','reservation_noETA']: #FIFO, nearest, reservation
            #DeliveryTimeToTravel, DeliveryRouteTaken = Route_Time_algorithm(fromNode, toNode, fabnetwork, numvehs, Route_algo,max_Veh_Limit)
            #DeliveryTimeToTravel, DeliveryRouteTaken = RouteAndTime_DDD(fromNode, toNode, fabnetwork)
            #veh_ETA[lotVeh[lotName]]=DeliveryTimeToTravel + (2*L_U_time)
    
            if round(time_now,2) != cycleLastTime:
                #pdb.set_trace()
                if (time_now>= cycleLastTime+cycleTime-0.0005) and (time_now <= 
                                                            cycleLastTime+cycleTime+0.0005):
                    #pdb.set_trace()
                    cycleLastTime=round(time_now,2)
                    vehReservedByLot = cycle(time_now, fabnetwork, vehLot, vehRouteTime,currVehEdge, numvehs, vehReservedByLot,reservationETA_threshold)

        changingTime_prev += 0.0001 #Add 0.5 seconds to the lastTime
    
    return currVehEdge, currVehEdgeTime,prevVehEdge,prevVehEdgeTime, vehReservedByLot
    #return

def FIFO_pickup(vehNumber, vehName):        
    lotnums = [int(lot[4:len(lot)]) for lot,source in lotsInQueue_source.items()]
    #pdb.set_trace()
    lotName = 'Lot '+str(min(lotnums))
    
    #pdb.set_trace()    
    lotVeh[lotName]=int(vehNumber)
    vehLot[vehName]=lotName
    #lotname.append(lotName)
    return lotName

#def nearestLot_pickup(vehNumber, vehName, lotName, fabnetwork, numvehs):
def nearestLot_pickup(vehNumber, vehName, fabnetwork, numvehs):
    '''In this function we do the following:
       1. Iterate through all the waiting in the queue
       2. In each iteration, calculate its distance from the vehicle under
          consideration
       3. Assign the lot nearest to the vehicle for pickup
       4. Remove this lot from the queue dictionary: lotsInQueue_distancefromVeh
    '''
    
    '''Steps 1 & 2''' 
    #pdb.set_trace()  
    for lot, dist in lotsInQueue_distancefromVeh.items():
        #pdb.set_trace()
        #PickupTimeToTravel, PickupRouteTaken = Route_Time_algorithm(vehLoc[vehName], lotSourceLoc[lotName], fabnetwork, numvehs, Route_algo,max_Veh_Limit)
        PickupTimeToTravel, PickupRouteTaken = Route_Time_algorithm(vehLoc[vehName], lotSourceLoc[lot], fabnetwork, numvehs, Route_algo,max_Veh_Limit)
        lotsInQueue_distancefromVeh[lot]=round(PickupTimeToTravel,4)
        #if lot=='Lot 531':
        #    pdb.set_trace()
            
    '''Steps 3''' 
    if not bool(lotsInQueue_distancefromVeh.keys()):
        pdb.set_trace()
    #pdb.set_trace()
    #if 'Lot 1500' in  lotsInQueue_distancefromVeh.keys():
    #    pdb.set_trace()
    selected_lot = min(lotsInQueue_distancefromVeh, key=lotsInQueue_distancefromVeh.get)
    #pdb.set_trace()
    lotVeh[selected_lot]=int(vehNumber)
    vehLot[vehName]=selected_lot
    #lotname.append(selected_lot)
    
    return  selected_lot        
        

#def reservationPolicy(vehNumber, vehName, lotName, fabnetwork, numvehs, vehReservedByLot):
def reservationPolicy(vehNumber, vehName, fabnetwork, numvehs, vehReservedByLot):
    #if bool(vehReservedByLot) and vehName=='veh13':
    #    pdb.set_trace()

    #if 'Lot 531' in vehReservedByLot.values():    
    #    pdb.set_trace()
        
    if bool(vehReservedByLot)==True:
        if vehName in vehReservedByLot.keys():
            #if vehName=='veh15':
            #    pdb.set_trace()
            selected_lot = vehReservedByLot[vehName]
            del vehReservedByLot[vehName]
            #if selected_lot=='Lot 17':
            #    pdb.set_trace()
        else:
            #selected_lot = nearestLot_pickup(vehNumber, vehName, lotName, fabnetwork, numvehs)
            selected_lot = nearestLot_pickup(vehNumber, vehName, fabnetwork, numvehs)
            #pdb.set_trace()
            if selected_lot in vehReservedByLot.values():
                del vehReservedByLot[[veh for veh, lot in vehReservedByLot.items() if lot == selected_lot][0]] #removes this lot from reservation list
            
            #if selected_lot=='Lot 17':
            #    pdb.set_trace()
            #pdb.set_trace()
            #nearestLot_reservation(fabnetwork, numvehs, vehReservedByLot)

        #pdb.set_trace()
    else:
        #pdb.set_trace()
        #selected_lot = nearestLot_pickup(vehNumber, vehName, lotName, fabnetwork, numvehs)
        selected_lot = nearestLot_pickup(vehNumber, vehName, fabnetwork, numvehs)
        #if selected_lot=='Lot 17':
        #    pdb.set_trace()
            
        #if selected_lot in vehReservedByLot.values():
        #    del vehReservedByLot[[veh for veh, lot in vehReservedByLot.items() if lot == selected_lot][0]] #removes this lot from reservation list
    #print 'xxx'
    lotVeh[selected_lot]=int(vehNumber)
    vehLot[vehName]=selected_lot
    #lotname.append(selected_lot)
    #if selected_lot=='Lot 17':
    #    pdb.set_trace()
                    
    return  selected_lot     


#def veh_assignedToLot(passingTime,vehNumber, vehName, lotName, fabnetwork, numvehs,vehReservedByLot,Pickup_algo):
def veh_assignedToLot(passingTime,vehNumber, vehName, fabnetwork, numvehs,vehReservedByLot,Pickup_algo):
    #for veh,veh_status in vehStatus.items():
    #            #pdb.set_trace()
    #            #if round(float(passingTime),4)>=0.0593:
    #            #    pdb.set_trace()
    #            if veh_status=='W':
    #                #if round(float(passingTime),4)>=0.0593:
    #                #    pdb.set_trace()
    #                if len(veh)>4:
    #                    vehNumber=veh[3]+veh[4]
    #                else:
    #                    vehNumber=veh[3]
    #                vehName='veh'+vehNumber
    #                #pdb.set_trace()
    #                vehStatus[vehName]='P' 
    #                break 

    #pdb.set_trace()
    if len(vehName)>4:
        vehNumber=int(vehName[3]+vehName[4])
    else:
        vehNumber=int(vehName[3])
    '''Algorithm 1: FIFO'''
    if Pickup_algo == 'FIFO': #FIFO, nearest, reservation
        #pdb.set_trace()
        selected_lotName = FIFO_pickup(vehNumber, vehName)
    
        '''Algorithm 2: Idle vehicle picks up Nearest lot'''
    elif Pickup_algo == 'nearest': #FIFO, nearest, reservation
        #pdb.set_trace()
        #selected_lotName = nearestLot_pickup(vehNumber, vehName, lotName, fabnetwork, numvehs)
        selected_lotName = nearestLot_pickup(vehNumber, vehName, fabnetwork, numvehs)
    
        '''Algorithm 3: The vehicle picks up the last lot that reserved it'''
    elif Pickup_algo in ['reservation','reservation_noETA']: #FIFO, nearest, reservation   
        #pdb.set_trace()
        #selected_lotName = reservationPolicy(vehNumber, vehName, lotName, fabnetwork, numvehs, vehReservedByLot)
        selected_lotName = reservationPolicy(vehNumber, vehName, fabnetwork, numvehs, vehReservedByLot)
        
    #vehicle.append(vehName)
    #pdb.set_trace()
    #vehicleLocation.append(vehLoc[vehName])
    
    return selected_lotName

def processLots(vehRouteTime,vehPreviousTime,vehReservedByLot,reservationETA_threshold,currVehEdge,currVehEdgeTime,prevVehEdge, prevVehEdgeTime,passingTime,veh_nextEventTime,lotNumber,prevlotRequestTime,lotsInQueue_source,lotsInQueue_distancefromVeh,Qt,Pickup_algo,Route_algo,max_Veh_Limit,num_vehicles, fabData,requestData, ohtc, fabnetwork):
    #pdb.set_trace()
    """A lot pickup request (our simulation process) (each lot has a ``name'')
    arrives at the OHTC (``ohtc'') and requests delivery to a destination node by
    the first available vehicle.

    The first available vehicle starts moving towards this request. Upon reaching
    the lot's source node, it loads the lot and starts the delivery process. When
    it reaches the destination node, it unloads the lot there.
    
    """

    
    #manipulations
    #currVehEdge,currVehEdgeTime,prevVehEdge,prevVehEdgeTime,vehReservedByLot = manipulations(cycleLastTime, Pickup_algo,passingTime, passingTime+0.0001, fabnetwork, vehLot, vehRouteTime, vehPreviousTime,currVehEdge,currVehEdgeTime,prevVehEdge,prevVehEdgeTime,vehStatus,lotPickupTravelTime,lotDeliveryTravelTime,Qt,Qt_array,Qt_times,vehReservedByLot)
    numvehs = num_Vehs_on_Edges(fabnetwork,currVehEdge,passingTime)
    number_of_Vehicles.append(numvehs.values())
    #pdb.set_trace()
    
    lotNum = lotNumber
    lotRequestTime = round(requestData.Time[lotNum-1] + prevlotRequestTime,4)
    #pdb.set_trace()    
    
    #print passingTime
    
    #if passingTime==round(0.1770,4):
    #    pdb.set_trace()
    #pdb.set_trace()
    
    if passingTime==lotRequestTime: #append lot a 
        #if passingTime==round(0.1770,4):
        #    pdb.set_trace()
                      
        #lot Details
        lotName = 'Lot '+str(lotNum)
        fromNode = requestData.From[lotNum-1]
        toNode = requestData.To[lotNum-1]
        print('%s pickup request arrives in the OHTC at %.5f hrs from %s for delivery at %s' % (lotName, passingTime, fromNode, toNode))
        prevlotRequestTime = lotRequestTime
        lotRequestOccurTime[lotName] = lotRequestTime
        
        #pdb.set_trace() 
        #increase lot number (lotNum) and change queue length (Qt)
        lotNum+=1 #look for next lot in following iterations
        Qt+=1 #One lot has entered the system
        
        activity.append('Pickup Request Appeared')
        lotname.append(lotName)
        source.append(fromNode)
        destination.append(toNode)    
        time.append(passingTime)
        vehicle.append(0)
        vehicleLocation.append('Not Known')
        lotSourceLoc[lotName]=fromNode
        lotDestLoc[lotName]=toNode
        lotRequestOccurTime[lotName]=passingTime
        #if lotName=='Lot 531':
        #    pdb.set_trace()
        lotsInQueue_source[lotName]=fromNode
        lotsInQueue_distancefromVeh[lotName]=""
        #if lotName=='Lot 86':
        #    pdb.set_trace()

    #if passingTime==0.1478:
    #    pdb.set_trace()
    #pdb.set_trace()
    temp_vehStatus = vehStatus
    for veh,veh_status in temp_vehStatus.items():
        #if passingTime==0.0033:
        #    pdb.set_trace()
        if bool(lotsInQueue_source.keys()) and bool(lotsInQueue_distancefromVeh.keys()) and vehStatus[veh]=='W':
            #if passingTime==0.0033:
            #    pdb.set_trace()
            #pdb.set_trace()    
            #if vehStatus[veh]=='W':
            #if passingTime==0.0033:
            #    pdb.set_trace()
            Qt-=1               
                                
            #Implement pickup algorithm within function veh_assignedToLot(...)
            #if not bool(lotsInQueue_distancefromVeh.keys()):
            #    pdb.set_trace()
            #pdb.set_trace()
            
            #selected_lotName = veh_assignedToLot(passingTime,vehNumber, vehName, lotName, fabnetwork, numvehs, vehReservedByLot,Pickup_algo)
            #if passingTime==0.0695:
            #    pdb.set_trace()
            selected_lotName = veh_assignedToLot(passingTime,vehNumber, veh, fabnetwork, numvehs, vehReservedByLot,Pickup_algo)
            #try:
            #    lotSourceLoc[selected_lotName]
            #except:
            #    pdb.set_trace()
                
            selected_lotName_fromNode = lotSourceLoc[selected_lotName]
            selected_lotName_toNode = lotDestLoc[selected_lotName]
            
            '''changes begin:'''
            lotName = selected_lotName
            fromNode = selected_lotName_fromNode
            toNode= selected_lotName_toNode
            #if passingTime==0.0695:
            #    pdb.set_trace()
            '''changes end'''
            
            #pdb.set_trace()
            #if lotName == 'Lot 86':
            #    pdb.set_trace()
            activity.append('Lot is being picked up')
            lotname.append(lotName)
            source.append(fromNode)
            destination.append(toNode)    
            time.append(passingTime)
            vehicle.append(veh)
            vehicleLocation.append(vehLoc[veh])
            #vehRouteTime['veh3']=['None',0.0]
            #if passingTime==0.0695:
            #    pdb.set_trace()
            eventTime,route,vehPreviousTime,vehRouteTime = ohtc.pickup(passingTime,lotName, lotVeh[vehLot[veh]],vehLoc[veh], fromNode, fabnetwork,vehRouteTime, vehPreviousTime,vehStatus,numvehs,Pickup_algo,Route_algo,max_Veh_Limit)
            veh_nextEventTime[veh]=round(passingTime+eventTime,4) 
            #if veh == 'veh3':
            #    pdb.set_trace()        
            vehStatus[veh]='P'
            
        elif vehStatus[veh]=='P':
            #if passingTime==0.0033:
            #    pdb.set_trace()
            route=vehRouteTime[veh][0]
            #if prevVehEdge[veh]==(str(route[-2]),str(route[-1])):
            #    pdb.set_trace()
            try:
                #(str(route[-2]),str(route[-1])) 
                if prevVehEdge[veh]==(str(route[-2]),str(route[-1])) and veh_eventTimeUpdate[veh]:
                    edgeTime, lastedge = Route_Time_algorithm(str(route[-2]), str(route[-1]), fabnetwork, numvehs, Route_algo,max_Veh_Limit)
                    #eventime = ohtc.load_unload()
                    #pdb.set_trace()
                    veh_nextEventTime[veh]=round(passingTime+edgeTime,4)
                    veh_eventTimeUpdate[veh] = False
                if passingTime == veh_nextEventTime[veh]:
                    #pdb.set_trace()
                    print('%s will now be loaded by a vehicle at %.5f hrs from %s' % (vehLot[veh], passingTime,lotSourceLoc[vehLot[veh]]))
                    veh_eventTimeUpdate[veh]=True
                    activity.append('Lot is being loaded')
                    lotname.append(vehLot[veh])
                    source.append(lotSourceLoc[vehLot[veh]])
                    destination.append(lotDestLoc[vehLot[veh]])    
                    time.append(passingTime)
                    vehicle.append(veh)
                    vehLoc[veh]=lotSourceLoc[vehLot[veh]]
                    vehicleLocation.append(vehLoc[veh])
                    vehStatus[veh]='L'
                    veh_nextEventTime[veh]=round(passingTime+0.0028,4)
                    veh_eventTimeUpdate[veh] = False
                    #if vehLot[veh]=='Lot7':
                    #    pdb.set_trace()
                    #pdb.set_trace()
            except IndexError:
                print('%s will now be loaded by a vehicle at %.5f hrs from %s' % (vehLot[veh], passingTime,lotSourceLoc[vehLot[veh]]))
                veh_eventTimeUpdate[veh]=True
                activity.append('Lot is being loaded')
                lotname.append(vehLot[veh])
                source.append(lotSourceLoc[vehLot[veh]])
                destination.append(lotDestLoc[vehLot[veh]])    
                time.append(passingTime)
                vehicle.append(veh)
                vehLoc[veh]=lotSourceLoc[vehLot[veh]]
                vehicleLocation.append(vehLoc[veh])
                vehStatus[veh]='L'
                veh_nextEventTime[veh]=round(passingTime+0.0028,4)
                veh_eventTimeUpdate[veh] = False

        elif vehStatus[veh]=='L':
            #print '\t\t\tin loading section'
            #pdb.set_trace()
            #ohtc.delivery()
            #veh_nextEventTime[veh]=passingTime+0.0028#+edgeTime
            veh_eventTimeUpdate[veh] = False
            #if passingTime==0.1478:
            #    pdb.set_trace()
            if passingTime == veh_nextEventTime[veh]:
                print('%s is loaded by a vehicle at %.5f hrs at %s' % (vehLot[veh], passingTime,lotSourceLoc[vehLot[veh]]))
                #print('%s is being moved to delivery location by a vehicle at %.5f hrs at %s' % (vehLot[veh], passingTime,lotSourceLoc[vehLot[veh]]))
                veh_eventTimeUpdate[veh]=True
                activity.append('Lot is loaded and moving for delivery')
                lotname.append(vehLot[veh])
                source.append(lotSourceLoc[vehLot[veh]])
                destination.append(lotDestLoc[vehLot[veh]])    
                time.append(passingTime)
                vehicle.append(veh)
                vehLoc[veh]=lotSourceLoc[vehLot[veh]]
                vehicleLocation.append(vehLoc[veh])
                vehStatus[veh]='D'
                #pdb.set_trace()
                eventTime,route,vehPreviousTime,vehRouteTime = ohtc.delivery(passingTime,vehLot[veh], lotVeh[vehLot[veh]],vehLoc[veh], lotDestLoc[vehLot[veh]], fabnetwork,vehRouteTime, vehPreviousTime,vehStatus,numvehs,Pickup_algo,Route_algo,max_Veh_Limit)
                #if veh=='veh12':
                #    pdb.set_trace()
                veh_nextEventTime[veh]=round(passingTime+eventTime,4)
                #pdb.set_trace()

        elif vehStatus[veh]=='D':
            route=vehRouteTime[veh][0]
            #pdb.set_trace()
            if prevVehEdge[veh]==(str(route[-2]),str(route[-1])) and veh_eventTimeUpdate[veh]:
                edgeTime, lastedge = Route_Time_algorithm(str(route[-2]), str(route[-1]), fabnetwork, numvehs, Route_algo,max_Veh_Limit)
                #eventime = ohtc.load_unload()
                #pdb.set_trace()
                veh_nextEventTime[veh]=round(passingTime+edgeTime,4)
                veh_eventTimeUpdate[veh] = False
            if passingTime == round(veh_nextEventTime[veh],4):
                print('%s has has reached delivery location at %.5f hrs at %s' % (vehLot[veh], passingTime,lotDestLoc[vehLot[veh]]))
                veh_eventTimeUpdate[veh]=True
                activity.append('Lot has reached delivery location')
                lotname.append(vehLot[veh])
                source.append(lotSourceLoc[vehLot[veh]])
                destination.append(lotDestLoc[vehLot[veh]])    
                time.append(passingTime)
                vehicle.append(veh)
                vehLoc[veh]=lotDestLoc[vehLot[veh]]
                vehicleLocation.append(vehLoc[veh])
                vehStatus[veh]='U'
                #pdb.set_trace()
                veh_nextEventTime[veh]=round(passingTime+0.0028,4)
                #pdb.set_trace()
                #eventime,route = ohtc.delivery(passingTime,lotName, lotVeh,vehLoc, fromNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus,numvehs,Route_algo,max_Veh_Limit)
                #eventTime = ohtc.delivery(passingTime,lotName, lotVeh,vehLoc, fromNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus,numvehs,Route_algo,max_Veh_Limit)
                #veh_nextEventTime[veh]=eventTime

        elif vehStatus[veh]=='U':
            #veh_nextEventTime[veh]=passingTime+0.0028#+edgeTime
            veh_eventTimeUpdate[veh] = False
            if passingTime == veh_nextEventTime[veh]:
                print('%s is unloaded by a vehicle at %.5f hrs at %s' % (vehLot[veh], passingTime,lotSourceLoc[vehLot[veh]]))
                veh_eventTimeUpdate[veh]=True
                activity.append('Lot is unloaded')
                lotname.append(vehLot[veh])
                source.append(lotSourceLoc[vehLot[veh]])
                destination.append(lotDestLoc[vehLot[veh]])    
                time.append(passingTime)
                vehicle.append(veh)
                #vehLoc[veh]=lotDestLoc[vehLot[veh]]
                vehicleLocation.append(vehLoc[veh])
                vehStatus[veh]='W'
                #eventime,route = ohtc.delivery(passingTime,lotName, lotVeh,vehLoc, fromNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus,numvehs,Route_algo,max_Veh_Limit)
                #eventTime = ohtc.delivery(passingTime,vehLot[veh], lotVeh[vehLot[veh]],vehLoc[veh], fromNode, fabnetwork, vehRouteTime, vehPreviousTime,vehStatus,numvehs,Route_algo,max_Veh_Limit)
                veh_nextEventTime[veh]='None'
                #pdb.set_trace()

        #if passingTime==round(0.2120,4):
        #    pdb.set_trace()
    #manipulations after each event
    currVehEdge,currVehEdgeTime,prevVehEdge,prevVehEdgeTime,vehReservedByLot = manipulations(cycleLastTime, Pickup_algo,passingTime, passingTime+0.0001, fabnetwork, vehLot, vehRouteTime, vehPreviousTime,currVehEdge,currVehEdgeTime,prevVehEdge,prevVehEdgeTime,vehStatus,lotPickupTravelTime,lotDeliveryTravelTime,Qt,Qt_array,Qt_times,vehReservedByLot,reservationETA_threshold)
        #else:
        #    break
    #pdb.set_trace()
    #if passingTime==round(0.2119,4):
    #    pdb.set_trace()
                
    return lotNum,prevlotRequestTime,Qt,lotsInQueue_source,lotsInQueue_distancefromVeh,currVehEdge,currVehEdgeTime,prevVehEdge, prevVehEdgeTime,vehReservedByLot,vehRouteTime,vehPreviousTime  



def simulate(cycleLastTime, Pickup_algo,reservationETA_threshold,Route_algo,max_Veh_Limit,num_vehicles, fabData, requestData,simTime):
    """Create an OHTC system, a number of initial lots and keep creating lots
    approx. every ``t_inter`` hours.
    
    We will later on change how these requests are created.
    
    """
    global currVehEdge, currVehEdgeTime, prevVehEdge, prevVehEdgeTime
    global lotRequestOccurTime, lotServiceBeginsTime,lotsInQueue_source,lotsInQueue_distancefromVeh, lotWaitTime 
    global lotPickupTravelTime, lotDeliveryTravelTime, lotPickupRoute
    global lotDeliveryRoute, Qt, Qt_array, Q_times, number_of_Vehicles
    global vehReservedByLot,vehPrevEdge,vehRouteTime,vehPreviousTime
    
    initialize()
    
    ohtc = Fab(num_vehicles) # Create the OHTC system
    fabnetwork = FabConstructorDDD4.FabNetwork(fabData) # Create the basic fabNetwork 
    numvehs=num_Vehs_on_Edges(fabnetwork,currVehEdge,0.0) # numvehs=num_Vehs_on_Edges(fabnetwork,start_time,vehRouteTime,vehPreviousTime) 
    fabnetwork.makeFabDDD(numvehs)
    '''Create the base for fabNetwork to be periodically updated based on number 
    of vehicles on every edge '''
    requestData = pandas.read_csv(requestData) # Create lot P/D requests
    #pdb.set_trace()
    '''i=0
    requestTime=round(float(requestData.Time[i]),4)'''
    lotNum=1
    prevlotRequestTime=0.0000
    Qt=0
    passingTime=0.0
    #for i in range(1):
    while passingTime<=simTime:    
        '''processLots(cycleLastTime, Pickup_algo,env, LotNum, 'Lot %d' % LotNum, requestData.From[i] ,requestData.To[i] , ohtc, fabnetwork,))
        i += 1
        #LotNum += 1
        #requestTime+=round(float(requestData.Time[i]),4)'''
        #pdb.set_trace()
        #lotNum += 1
        #pdb.set_trace()
        lotNum,prevlotRequestTime,Qt,lotsInQueue_source,lotsInQueue_distancefromVeh,currVehEdge,currVehEdgeTime,prevVehEdge, prevVehEdgeTime,vehReservedByLot,vehRouteTime,vehPreviousTime = processLots(vehRouteTime,vehPreviousTime,vehReservedByLot,reservationETA_threshold,currVehEdge,currVehEdgeTime,prevVehEdge, prevVehEdgeTime,round(passingTime,4),veh_nextEventTime,lotNum,prevlotRequestTime,lotsInQueue_source,lotsInQueue_distancefromVeh,Qt,Pickup_algo,Route_algo,max_Veh_Limit,num_vehicles, fabData,requestData,ohtc, fabnetwork)
        #pdb.set_trace()
        passingTime+=0.0001
    #pdb.set_trace()
    return lotNum

    
        

def simulationPlot(graph_filename):
    
    """Updating final system state"""
    #pdb.set_trace()
    global Qt, Qt_array, Qt_times
    #Qt_array.append(Qt)
    #Qt_times.append(Qt_times[len(Qt_times)-1]+1)
    #pdb.set_trace()
    
    """Utilization Plot"""  
    #fig, ax_f = matplotlib.pyplot.subplots() 
    #ax_c = ax_f.twinx() 
    matplotlib.pyplot.plot(Qt_times,Qt_array)
    matplotlib.pyplot.grid(True)
    matplotlib.pyplot.title('System State and Warm-up Period')
    matplotlib.pyplot.xlabel('Time (hours)')
    matplotlib.pyplot.ylabel('Q(t)')
    matplotlib.pylab.savefig(graph_filename)
    matplotlib.pyplot.show()          


def arrivals(rateOfArrival):
    arrData = pandas.read_csv('FabSimulation_inputFromToData_cc0.5_case2.csv')
    filename = Arrival_Simulator.simulateArrivals(arrData,rateOfArrival)
    #pdb.set_trace()
    return filename

    

if __name__ == '__main__' :
    rateOfArrival=550
    arr_rate = 0
    filename = ''
    ConstructNewFile = False
    if ConstructNewFile:
        while arr_rate<rateOfArrival-2 or arr_rate>rateOfArrival+2:
            filename = arrivals(rateOfArrival)
            arr_rate = len(pandas.read_csv(filename))/20
    else:
        filename='FabSimulation_Arrivals_20hrs_'+str(rateOfArrival)+'arrs_cc0.5_case2.csv'
        arr_rate = len(pandas.read_csv(filename))/20
    #arr_rate = len(pandas.read_csv(filename))/20    
    #pdb.set_trace()
    
          
    # Setup and start the simulation
    print('\nOHTC\n')
    random.seed(RANDOM_SEED)  # This helps reproducing the results

    
    C = 15 

    reservationETA_threshold = 0.012#1000.0#0.012#0.0083 # reserve when a veh ETA is 30 seconds
    #Route_algo = 'BigM' #'DDD': Base | 'Linear': Add a linear but 1.5 times cost say | 'BigM': Add a very large cost to make jammed edge infeasible to travel on
    #Pickup_algo = 'reservation' #FIFO, nearest, reservation
    arrival_probab = 'hetero'#'homo' #'hetero': heterogeneous, 'homo': homogeneous
    #max_Veh_Limit = 3 # max_Veh_Limit = 2,3,4,5
    
    #route_algos= ['Linear','BigM'] #'Base'
    route_algos= ['BigM']
    pickup_algos = ['reservation','reservation_noETA']#,'reservation','FIFO','nearest']
    #veh_limits = [2,3,4,5]
    veh_limits = [2]
    
    simulationTime=[]
    algorithm=[]
    algoVehLimit=[]
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
    arrivalRate=[]
    vehNumber_C=[]
    trafficIntensity=[]
    vehicle_utilization=[]
    maxVehOnEdge=[]
    avgVehsOnEdge=[] 
    count_0_vehs=[]
    count_1_vehs=[]
    count_2_vehs=[]
    count_3_vehs=[]
    count_4_vehs=[]
    count_5_vehs=[]
    count_6_vehs=[]
    count_7_vehs=[]
    count_8_vehs=[]
    count_9_vehs=[]
    count_10_vehs=[]
    
    for Route_algo in route_algos:
        for Pickup_algo in pickup_algos:
            for max_Veh_Limit in veh_limits:
    
                if Route_algo!='Base':
                    Route_algorithm=Route_algo+str(max_Veh_Limit)
                else:
                    Route_algorithm=Route_algo  
                    
                if Pickup_algo=='reservation_noETA':
                    reservationETA_threshold = 1000.0     
                #pdb.set_trace()
                if arrival_probab=='hetero':
                    #pdb.set_trace()
                    simulatedArrivalsCsvFile = filename#'FabSimulation_Arrivals_20hrs_700arrs_cc0.5_case2.csv'#'FabSimulation_Arrivals_20hrs_700arrs_cc0.5_case2.csv'#'FabSimulation_Arrivals_20hrs_390arrs_hetero_case2.csv'#'FabSimulation_Arrivals_20hrs_50arrs_cc0.5_case2.csv'#'FabSimulation_Arrivals_20hrs_390arrs_cc0.5.csv'
                    algo = 'FabNetwork_P'+Pickup_algo+'_R'+Route_algorithm+'_'+arrival_probab+str(HeteroArrivalSkewRatio) #algo = Base(DDD), DD1, DD2_2t1: t2 = 2*t1, DD_algo1: different DD algos with externalized FIFO pickup algorithms
                                #DD_algo2: different DD algos with externalized nearestLot and FIFO pickup algorithms
                else:
                    simulatedArrivalsCsvFile = 'FabSimulation_Arrivals_20hrs_390arrs.csv'
                    algo = 'FabNetwork_P'+Pickup_algo+'_R'+Route_algorithm+'_'+arrival_probab
                
                #rateOfArrival=simulatedArrivalsCsvFile[29:32]
                simTime=20.0
                lotNum = simulate(cycleLastTime, Pickup_algo,reservationETA_threshold,Route_algo,max_Veh_Limit,NUM_VEHICLES, 'FabSimulation_From_To_Data4.csv',simulatedArrivalsCsvFile,simTime)
                
                graph_filename=str(simTime)+'hrs_'+str(rateOfArrival)+'arrs_'+algo
                #simulationPlot(graph_filename+'.png')
                
                
                numVehData = pandas.DataFrame(number_of_Vehicles)
                numVehData_csv_filename='numVehData_DD2_20hrs_'+str(max_Veh_Limit)+'maxVehs.csv'
                '''
                pandas.DataFrame.to_csv(numVehData,numVehData_csv_filename)
                '''
            
                veh0=veh1=veh2=veh3=veh4=veh5=veh6=veh7=0
                
                for i in range(len(numVehData.columns)):
                    veh0+=len(numVehData[numVehData[i]==0])
                    veh1+=len(numVehData[numVehData[i]==1])
                    veh2+=len(numVehData[numVehData[i]==2])
                    veh3+=len(numVehData[numVehData[i]==3])
                    veh4+=len(numVehData[numVehData[i]==4])
                    veh5+=len(numVehData[numVehData[i]==5])
                    veh6+=len(numVehData[numVehData[i]==6])
                    veh7+=len(numVehData[numVehData[i]==7])
                    
            
                #Note: Route and TravelTime not displayed yet
                #pdb.set_trace()
                statusMatrix = scipy.transpose([lotname,source,destination,vehicle,vehicleLocation,activity,time]) 
                
                
                #pdb.set_trace()
                statusData = pandas.DataFrame(statusMatrix , columns = ['Lot','Source','Destination','Vehicle_Number','Vehicle_Location', 'Activity','Time'])
                statusDataFilename = 'L4_FabSimulation_Status_'+graph_filename+'.csv'
                pandas.DataFrame.to_csv(statusData,statusDataFilename)
                
                #pdb.set_trace()
                avgWaitTime,stdWaitTime,avgPickupTime,stdPickupTime,avgDeliveryTime,stdDeliveryTime,avgCycleTime,stdCycleTime,avgServiceTime,stdServiceTime,util = test_stats2.return_stats(statusDataFilename,simTime)
                #pdb.set_trace()
                                                                
                simulationTime.append(simTime) 
                algorithm.append(algo)             
                algoVehLimit.append(max_Veh_Limit)
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
                arrivalRate.append(rateOfArrival)
                vehNumber_C.append(C)
                #trafficIntensity.append(round(rateOfArrival*(avgServiceTime/3600)/C,2))
                trafficIntensity.append(round(arr_rate*100/(avgServiceTime*C),2))
                vehicle_utilization.append(util)
                maxVehOnEdge.append(max(map(max,number_of_Vehicles)))
                avgVehsOnEdge.append(sum(map(max,number_of_Vehicles))/(len(numVehData)*len(numVehData.columns)))
                count_0_vehs.append(veh0)
                count_1_vehs.append(veh1)
                count_2_vehs.append(veh2)
                count_3_vehs.append(veh3)
                count_4_vehs.append(veh4)
                count_5_vehs.append(veh5)
                count_6_vehs.append(veh6)
                count_7_vehs.append(veh7)
                    
                statistics = scipy.transpose([simulationTime,algorithm,algoVehLimit,
                                    averageWaitTime,averagePickupTime,averageDeliveryTime,
                                    averageServiceTime,averageCycleTime,stddevWaitTime,
                                    stddevPickupTime,stddevDeliveryTime,stddevServiceTime,
                                    stddevCycleTime,arrivalRate,vehNumber_C,
                                    trafficIntensity,vehicle_utilization,maxVehOnEdge,
                                    avgVehsOnEdge,count_0_vehs,count_1_vehs,count_2_vehs,
                                    count_3_vehs,count_4_vehs,count_5_vehs,count_6_vehs,
                                    count_7_vehs])    
            
                statisticsData = pandas.DataFrame(statistics, columns = ['simulation_time',
                                    'algorithm','algoVehLimit','averageWaitTime',
                                    'averagePickupTime','averageDeliveryTime',
                                    'averageServiceTime','averageCycleTime',
                                    'stddevWaitTime','stddevPickupTime','stddevDeliveryTime',
                                    'stddevServiceTime','stddevCycleTime',
                                    'arrivalRate','vehNumber_C','trafficIntensity',
                                    'vehicle_utilization','maxVehOnEdge','avgVehsOnEdge',
                                    'count_0_vehs','count_1_vehs','count_2_vehs',
                                    'count_3_vehs','count_4_vehs','count_5_vehs',
                                    'count_6_vehs','count_7_vehs'] )
                
                #print statisticsData
                print 'rateOfArrival: ',rateOfArrival
                print 'simulation Time: ',simTime
                print 'true arrival Rate: ', arr_rate
                print 'util: ',util
                
                with open('statistics_new31.csv', 'a') as f:(statisticsData).to_csv(f, header=False)  