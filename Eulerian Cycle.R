find_eulerian_cycle <- function(adj_list) {
  # Convert adjacency list into list of edges
  virgin_edges <- list()
  for (node in names(adj_list)) {
    for (neighbor in adj_list[[node]]) {
      virgin_edges <- c(virgin_edges, list(c(node, neighbor)))
    }
  }

  # Get random node and build initial cycle
  cur_node <- sample(names(adj_list), 1)
  eulerian_cycle <- get_cycle(cur_node, virgin_edges)[1:(length(get_cycle(cur_node, virgin_edges))-1)]

  # Extend current cycle with other cycles until all edges go out
  while (length(virgin_edges) > 0) {
    i_end <- 1
    while (i_end <= length(eulerian_cycle)) {
      if (get_edge(eulerian_cycle[i_end], virgin_edges)) {
        new_cycle <- get_cycle(eulerian_cycle[i_end], virgin_edges)
        eulerian_cycle_updated <- c(eulerian_cycle[1:(i_end-1)], new_cycle, eulerian_cycle[(i_end+1):length(eulerian_cycle)])
        eulerian_cycle <- eulerian_cycle_updated
        break
      }
      i_end <- i_end + 1
    }
  }
  
  return(c(eulerian_cycle, eulerian_cycle[1]))
}

get_edge <- function(node, edges) {
  for (edge in edges) {
    if (edge[1] == node || edge[2] == node) {
      return(edge)
    }
  }
  return(NULL)
}

get_cycle <- function(cur_node, edges) {
  cycle <- list(cur_node)
  next_node <- cur_node
  while (TRUE) {
    edge <- get_edge(next_node, edges)
    if (is.null(edge)) {
      break
    }
    edges <- edges[-which(edges == edge, arr.ind = TRUE)[1],]
    next_node <- ifelse(edge[1] == next_node, edge[2], edge[1])
    cycle <- c(cycle, next_node)
  }
  return(cycle)
}

# Sample Input
adj_list <- list(
  "0" = c(3),
  "1" = c(0),
  "2" = c(1, 6),
  "3" = c(2),
  "4" = c(2),
  "5" = c(4),
  "6" = c(5, 8),
  "7" = c(9),
  "8" = c(7),
  "9" = c(6)
)

# Call function with sample input
result <- find_eulerian_cycle(adj_list)
cat(result, sep = " ")
