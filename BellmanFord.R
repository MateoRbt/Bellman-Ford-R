#bellman ford calculation
Bf <- function (graph,vertt)
{
  dist=matrix()
  parent=matrix()
  flag=0
  for(i in 1:vertt)
  {
    dist[i]=1000  #sptree 
    parent[i]=0 #list of parent nodes 
  }
  s= as.integer(readline(prompt="Enter a number: "))
  dist[s]=0
  # find shortest path
  for(i in 1:vertt)
  {
    for(j in 1:vertt)
    {
      if (graph[j,i]!=0)
      {
        temp1=graph[j,i]+dist[i]
        if(dist[j]>temp1)
        {
          dist[j]=temp1
          parent[j]=i
          
        }
       
      }
       
    }
    
  }
  # find negative circle
  for(i in 1:vertt)
  {
    for(j in 1:vertt)
    {
      if (graph[j,i]!=0)
      {
        temp1=graph[j,i]+dist[i]
        if(dist[j]>temp1)
        {
          flag=1;
          
        }
        
      }
      
    }
    
  }
 if(flag == 0)
 {
  cat("the shortest paths are !!! \n")
  print(parent)
  print(dist)
 }
  else
  {
    cat("Negative circle detected !!")
  }
  
}

#initialize graph 
v=3 #input the numbers or vertices 
temp=v*v
gve= matrix (
  
  
  #input weights and edges of the graph.In case of no edge between 2 vertices put 0
  #   n1 n2   n3  
  c(  0,  0,  0,    #n1
      5,  0,  0,    #n2
      2, -10, 0  ), #n3
  
  #the number of rows and columns is equal to the number of vertices
  nrow = v,
  ncol = v,
  byrow = TRUE          
)


Bf(gve,v)