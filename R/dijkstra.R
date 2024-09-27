#' @export
#'
dijkstra <-
function(graph,init_node){
  allNodes<-unique(graph$v1)
  distanceMatrix<-matrix(data=Inf,nrow=length(allNodes),ncol=1,dimnames=list(allNodes,1))
  distanceMatrix[as.character(init_node),]<-0
  unVisitedNodes<-allNodes[allNodes!=init_node]
  cumDistance<-0
  
  distances<-graph[(graph$v1==init_node),c("v2","w")]
  distanceMatrix[as.character(distances$v2),]=distances$w
  
  minDist<-min(distances[distances$v2 %in% unVisitedNodes,"w"])
  minDistNode<-distances[(distances$v2 %in% unVisitedNodes)&(distances$w == minDist),"v2"]
  unVisitedNodes<-unVisitedNodes[unVisitedNodes!=minDistNode]
  distances<-graph[(graph$v1==minDistNode)&(graph$v2 %in% unVisitedNodes),c("v2","w")]
  cumDistance<-cumDistance+minDist
  distances$w<-distances$w+cumDistance
  a<-distanceMatrix[as.character(distances$v2),]>distances$w
  a<-names(a[a==TRUE])
  distanceMatrix[a,]<-distances[distances$v2==a,"w"]
  
  minDist<-min(distanceMatrix[unVisitedNodes,1])
  minDistNode<-as.numeric(names(which(distanceMatrix[unVisitedNodes,1]==minDist)))
  
  while (length(unVisitedNodes)>1) {
    minDist<-min(distanceMatrix[unVisitedNodes,1])
    minDistNode<-as.numeric(names(which(distanceMatrix[unVisitedNodes,1]==minDist)[1]))
    unVisitedNodes<-unVisitedNodes[unVisitedNodes!=minDistNode]
    distances<-graph[(graph$v1==minDistNode)&(graph$v2 %in% unVisitedNodes),c("v2","w")]
    
    if(!is.infinite(distanceMatrix[minDistNode,1])){
      cumDistance<-distanceMatrix[minDistNode,1]
    }else{cumDistance<-0}
    
    distances$w<-distances$w+cumDistance
    if (length(distances$w)>1){
      a<-distanceMatrix[as.character(distances$v2),]>distances$w
      a<-names(a[a==TRUE])
      distanceMatrix[a,]<-distances[distances$v2==a,"w"]
    }else if((length(distances$w)==1)&(distanceMatrix[as.character(distances$v2),]>distances$w)){
      a<-as.character(distances$v2)
      distanceMatrix[a,]<-distances[distances$v2==a,"w"]
    }else {next}
  }
  return(as.vector(distanceMatrix))
}