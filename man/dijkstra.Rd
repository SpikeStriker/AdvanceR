\name{dijkstra}
\alias{dijkstra}
\title{Dijkstra's algorithm}
\description{Function calculating distance based on Dijkstra's algorithm}
\usage{dijkstra(graph, init_node)}
\arguments{
  \item{graph}{
Graph takes a dataframe object with 3 columns: v1,v2,w;
v1: numeric column that contains starting or from node
v2: numeric column that contains ending or to node
w:  numeric column that contains distance between nodes from and to 
}
  \item{init_node}{
init_node is a numeric scalar value that represents the starting node to be used 
when calculating all distances}
}
\details{
Dijkstra's algorithm finds the shortest path from a given source node to every 
other node and has a time complexity of O(V)^2 where V is the number of nodes.}
\value{
The function returns a vector with shortest distances to all nodes starting 
from the init_node.}
\author{Uday Jain}
\note{This exercise is done as a part of coursework for 732A94}
\references{
\href{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}{Dijkstra's Algorithm}
}
\seealso{
\href{https://rpubs.com/yogesh665/DJA}{DJA}
}
\examples{
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                        v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                        w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9));
library(elucideanDistanceToNode)
dijkstra(wiki_graph, 3);
dijkstra(wiki_graph, 1);

## The function is currently defined as
function (graph, init_node) 
{
    allNodes <- unique(graph$v1)
    distanceMatrix <- matrix(data = Inf, nrow = length(allNodes), 
        ncol = 1, dimnames = list(allNodes, 1))
    distanceMatrix[as.character(init_node), ] <- 0
    unVisitedNodes <- allNodes[allNodes != init_node]
    cumDistance <- 0
    distances <- graph[(graph$v1 == init_node), c("v2", "w")]
    distanceMatrix[as.character(distances$v2), ] = distances$w
    minDist <- min(distances[distances$v2 \%in\% unVisitedNodes, 
        "w"])
    minDistNode <- distances[(distances$v2 \%in\% unVisitedNodes) & 
        (distances$w == minDist), "v2"]
    unVisitedNodes <- unVisitedNodes[unVisitedNodes != minDistNode]
    distances <- graph[(graph$v1 == minDistNode) & (graph$v2 \%in\% 
        unVisitedNodes), c("v2", "w")]
    cumDistance <- cumDistance + minDist
    distances$w <- distances$w + cumDistance
    a <- distanceMatrix[as.character(distances$v2), ] > distances$w
    a <- names(a[a == TRUE])
    distanceMatrix[a, ] <- distances[distances$v2 == a, "w"]
    minDist <- min(distanceMatrix[unVisitedNodes, 1])
    minDistNode <- as.numeric(names(which(distanceMatrix[unVisitedNodes, 
        1] == minDist)))
    while (length(unVisitedNodes) > 1) {
        minDist <- min(distanceMatrix[unVisitedNodes, 1])
        minDistNode <- as.numeric(names(which(distanceMatrix[unVisitedNodes, 
            1] == minDist)[1]))
        unVisitedNodes <- unVisitedNodes[unVisitedNodes != minDistNode]
        distances <- graph[(graph$v1 == minDistNode) & (graph$v2 \%in\% 
            unVisitedNodes), c("v2", "w")]
        if (!is.infinite(distanceMatrix[minDistNode, 1])) {
            cumDistance <- distanceMatrix[minDistNode, 1]
        }
        else {
            cumDistance <- 0
        }
        distances$w <- distances$w + cumDistance
        if (length(distances$w) > 1) {
            a <- distanceMatrix[as.character(distances$v2), ] > 
                distances$w
            a <- names(a[a == TRUE])
            distanceMatrix[a, ] <- distances[distances$v2 == 
                a, "w"]
        }
        else if ((length(distances$w) == 1) & (distanceMatrix[as.character(distances$v2), 
            ] > distances$w)) {
            a <- as.character(distances$v2)
            distanceMatrix[a, ] <- distances[distances$v2 == 
                a, "w"]
        }
        else {
            next
        }
    }
    return(as.vector(distanceMatrix))
  }
}
\keyword{ ~ dijkstra }
\concept{ ~ dijkstra's algorithm }
