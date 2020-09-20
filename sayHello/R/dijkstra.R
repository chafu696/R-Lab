#' @title dijkstra
#' 
#' @description dijkstra returns a vector showing the length of the shortest path from an initial node to each other node in a given graph. 
#' @details The function uses Dijkstra's algorithm to calculate the shortest path from the initial node. 
#' For specific details on how the algorithm works, visit the link in See Also.
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Dijkstra\%27s_algorithm}
#' 
#' @param graph A data frame with three variables (v1, v2, w) describing the edges of a graph (from v1 to v2) and the length of the edge (w)
#' @param init_node A number, the initial vertex
#' 
#' @return A vector giving the length of shortest paths.
#'
#' @examples #Describing a graph as a data frame in the required format.
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'                         v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                         w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' @examples dijkstra(wiki_graph, 1)
#' @examples dijkstra(wiki_graph, 3)
#' 
#' @export



dijkstra <- function(graph, init_node) {
  stopifnot(init_node %in% graph[,1], length(init_node) == 1, is.data.frame(graph), names(graph)==c("v1","v2","w"))
  Q <- c()
  distance <- c()
  
  for (v in 1:max(graph[,1])) {
    distance[v] <- Inf
    Q <- c(Q,v)
  }
  distance[init_node] <- 0

  while (length(Q) != 0) {
    u <- subset(Q, Q %in% which(distance == min(distance[Q])))[1] 
    Q <- Q[-match(u, Q)]
    
    for (v in graph[graph[,1] == u & graph[,2] %in% Q, 2]) {
      alt <- distance[u] + graph[graph[,1] == u & graph[,2] == v, 3]
      if (alt < distance[v]) {
        distance[v] <- alt
      }
    }
  }
  return(distance)
}