\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
