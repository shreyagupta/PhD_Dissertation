import scipy
import networkx
import pandas
import pdb

class FabNetwork(object):
    def __init__(self, distData):
        self.distData = pandas.read_csv(distData)
        self.fromNode = self.distData.From
        self.toNode = self.distData.To
        self.time = self.distData.Time
        
        #We define the basic graph of the fab
        self.fabgraph = networkx.DiGraph()
        for i in range(len(self.fromNode)):
            self.fabgraph.add_edge(self.fromNode[i],self.toNode[i],length=self.time[i])
        
        
        '''
        ALGORITHM 1 (Base Algorithm): Daifuku Dynamic Dijkstra's Algorithm (DDD)
        We define a graph where time taken by a vehicle to travel on edge is:
        f = k + t*n
        where,
        k = time to taken to travel on an empty edge
        t = some constant additional time per additional vehicle on an edge
        n = number of vehicles on the edge
        
        We update this graph in a separate function below
        '''
        self.fabgraphDDD = networkx.DiGraph()  
        
        
        '''
        ALGORITHM 2: Dynamic Dijkstra's Algorithm 1 (DD1)
        We define a graph where time taken by a vehicle to travel on edge is:
        f = k + t*n, n <= maxVehLimit
        f = k + M*n, n > maxVehLimit
        where,
        k = time to taken to travel on an empty edge
        t = some constant additional time per additional vehicle on an edge
        n = number of vehicles on the edge
        M = really large positive number chosen to set a high edge cost to repel
            vehicles
        maxVehLimit = Maximum number of vehicles wanted on a route
        
        We update this graph in a separate function below
        '''
        self.fabgraphDD1 = networkx.DiGraph()  
        
                   
        '''
        ALGORITHM 3: Dynamic Dijkstra's Algorithm 2 (DD1)
        We define a graph where time taken by a vehicle to travel on edge is:
        f = k + t1*n, n <= maxVehLimit
        f = k + t2*n, n > maxVehLimit
        t1 < t2
        where,
        k = time to taken to travel on an empty edge
        t1 = some constant additional time per additional vehicle on an edge
        t2 = some constant additional time per additional vehicle on an edge,
             t2 is chosen to set a high edge cost to repel vehicles from chosing
             jammed or heavy-traffic routes
        n = number of vehicles on the edge
        maxVehLimit = Maximum number of vehicles wanted on a route
        
        We update this graph in a separate function below
        '''
        self.fabgraphDD2 = networkx.DiGraph()           
                                    
        
    def shortestPath(self, startNode, destNode):
        return networkx.shortest_path(self.fabgraph,startNode,destNode,weight='length')
        
        
    def shortestPathTime(self, startNode, destNode):
            return networkx.shortest_path_length(self.fabgraph,startNode,destNode,weight='length')

        
    def edges(self, startNode, destNode):
        return self.fabgraph.edges
        
    
    def makeFabDDD(self, numVehs, t=0.0028):
        #pdb.set_trace()
        for i in range(len(self.fromNode)):
            #pdb.set_trace()
            self.fabgraphDDD.add_edge(self.fromNode[i],self.toNode[i],length=float(self.time[i])+(t*float(numVehs[(str(self.fromNode[i]),str(self.toNode[i]))])))              
                       
    def shortestPathDDD(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path(self.fabgraphDDD,startNode,destNode,weight='length')
                
    def shortestPathTimeDDD(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path_length(self.fabgraphDDD,startNode,destNode,weight='length')
        

    def makeFabDD1(self, numVehs, maxVehLimit, t=0.0028, M=10000.000):
        for i in range(len(self.fromNode)):
            numVehOnEdge = numVehs[(str(self.fromNode[i]),str(self.toNode[i]))]
            if numVehOnEdge<=maxVehLimit:
                self.fabgraphDD1.add_edge(self.fromNode[i],self.toNode[i],length=float(self.time[i])+(t*numVehOnEdge))
            else:
                self.fabgraphDD1.add_edge(self.fromNode[i],self.toNode[i],length=float(self.time[i])+(M*numVehOnEdge))               
    
    def shortestPathDD1(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path(self.fabgraphDD1,startNode,destNode,weight='length')
                
    def shortestPathTimeDD1(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path_length(self.fabgraphDD1,startNode,destNode,weight='length')
        
    
    def makeFabDD2(self, numVehs, maxVehLimit, t1=0.0028, t2=0.05): #Add 15secs more than maxVehLimit on an edge
        for i in range(len(self.fromNode)):
            numVehOnEdge = numVehs[(str(self.fromNode[i]),str(self.toNode[i]))]
            if numVehOnEdge<=maxVehLimit:
                self.fabgraphDD2.add_edge(self.fromNode[i],self.toNode[i],length=self.time[i]+(t1*numVehOnEdge))
            else:
                #pdb.set_trace()
                self.fabgraphDD2.add_edge(self.fromNode[i],self.toNode[i],length=self.time[i]-(t1*maxVehLimit)+(t2*numVehOnEdge)) 
                #pdb.set_trace()              
    
    def shortestPathDD2(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path(self.fabgraphDD2,startNode,destNode,weight='length')
                
    def shortestPathTimeDD2(self, startNode, destNode):
        #pdb.set_trace()
        return networkx.shortest_path_length(self.fabgraphDD2,startNode,destNode,weight='length')

        
if __name__ == '__main__' :
    
    fabnetwork = FabNetwork('FabNodeData.csv')
    print fabnetwork.shortestPath('STK1','STB2')
    print fabnetwork.shortestPathTime('STK1','STB2')
    print fabnetwork.edges
    