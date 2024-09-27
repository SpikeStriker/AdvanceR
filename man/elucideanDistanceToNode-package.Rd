\name{elucideanDistanceToNode-package}
\alias{elucideanDistanceToNode-package}
\alias{elucideanDistanceToNode}
\docType{package}
\title{
\packageTitle{elucideanDistanceToNode}
}
\description{
\packageDescription{elucideanDistanceToNode}
}
\details{
The package contains 2 functions:
dijkstra and euclidean
}
\author{
\packageAuthor{elucideanDistanceToNode}

Maintainer: \packageMaintainer{elucideanDistanceToNode}
}
\references{
\href{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}{Dijkstra's Algorithm}
}
\keyword{ ~dijkstra }
\keyword{ ~euclidean }

%% Uncomment below to imitate parts of library(help = elucideanDistanceToNode)
%\section{The \file{DESCRIPTION} File}{\packageDESCRIPTION{elucideanDistanceToNode}}
%\section{Documentation Index}{\packageIndices{elucideanDistanceToNode}}

\seealso{
\href{https://rpubs.com/yogesh665/DJA}{DJA}
}
\examples{
wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                        v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                        w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9));

dijkstra(wiki_graph, 3);
dijkstra(wiki_graph, 1);
}
