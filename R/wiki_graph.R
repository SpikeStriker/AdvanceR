#' Graph
#'
#' A data frame representing a graph with edges and weights.
#'
#' @format A data frame with three columns:
#' \describe{
#'   \item{v1}{The start node of the edge.}
#'   \item{v2}{The end node of the edge.}
#'   \item{w}{The weight of the edge.}
#' }
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'     v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'     w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' 
#' @name wiki_graph
#' @references \href{https://en.wikipedia.org/wiki/Graph}{Graph - Wikipedia}
#' @export
wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))