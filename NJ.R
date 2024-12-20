NJ <- function(D_matrix, total_dim, edge_vector) {
  n = dim(D_matrix)[1]
  id_m = matrix(0, ncol=n, nrow=n) # identity matrix
  diag(id_m) <- 1
  Q = matrix(0, ncol=n, nrow=n)
  trans_m = matrix(1, ncol=n, nrow=n)
  d1 = D_matrix %*% trans_m
  d2 = t(d1)
  Q = (n-2) * D_matrix - d1 - d2
  diag(Q) <- 0
  minQ <- min(Q)
  cord = which(Q==minQ, arr.ind=TRUE)[1,]
  cord1 = cord[2]
  cord2 = cord[1]
  
  edge1 = edge_vector[cord1]
  edge2 = edge_vector[cord2] # retrieve the edge values for joining taxa
  delta1 = 0.5 * D_matrix[cord1,cord2] + 0.5/(n-2)*(d1[cord1,cord2]-d2[cord1,cord2])
  delta2 = D_matrix[cord1,cord2] - delta1
  
  if (n==3) {
    delta3 = D_matrix[1,3] - delta1 # it's for the last iteration which will omit 3 delta values. The last Q matrix must be a matrix with all equal values not in diagonal
    edge3 = edge_vector[3]
    return(list(c(edge1, edge2, edge3), c(delta1, delta2, delta3)))
  } # the last iteration would not need to update the D matrix and just return edge values and delta
  
  trans_m = id_m
  trans_m[cord1,cord2] = 1
  new_row = trans_m %*% D_matrix
  
  trans_m = id_m
  trans_m[cord1,] = -1
  new_row = 0.5 * (new_row %*% trans_m)[cord1,c(-cord1, -cord2)]
  
  D_matrix <- D_matrix[c(-cord1,-cord2),]
  D_matrix <- D_matrix[,c(-cord1,-cord2)] # delete 2 rows and 2 cols that belong to the 2 joined taxa
  edge_vector <- edge_vector[c(-cord1,-cord2)] # do the same with the edge vector
  edge_vector <- append(total_dim+n-2, edge_vector) # create a new edge value for new node
  D_matrix <- rbind(new_row, D_matrix)
  D_matrix <- cbind(c(0,new_row), D_matrix) # always the new node is in the first col and row
  return(list(D_matrix, c(edge1, edge2), c(delta1, delta2), edge_vector))
}


NJ_iter <- function(D_init) {
  dim = dim(D_init)[1]
  edge_v = c(1:dim) # vector can be operated and record the edge values' changes
  edge <- matrix(0, ncol=2, nrow=2*dim-3)
  tip.label <- as.character(c(1:dim))
  Nnode = dim - 2
  edge.length <- c(rep(0, each=2*dim-3))
  if (dim > 3) {
    for(i in dim:4) {
      out <- NJ(D_init, dim, edge_v)
      D_init <- out[[1]]
      edge[c(2*dim-2*i+1,2*dim-2*i+2),] <- c(dim+i-2, dim+i-2, out[[2]][1], out[[2]][2]) # the first 2 elements are edge values for new node created in last iteration
      edge.length[c(2*dim-2*i+1,2*dim-2*i+2)] <- out[[3]]
      edge_v <- out[[4]]
    }
  }
  out <- NJ(D_init, dim, edge_v)
  edge[c(2*dim-5, 2*dim-4, 2*dim-3),] <- c(dim+1, dim+1, dim+1, out[[1]][1], out[[1]][2], out[[1]][3])
  edge.length[c(2*dim-5, 2*dim-4, 2*dim-3)] <- out[[2]]
  
  tree = list("edge"=edge, "tip.label"=tip.label, "Nnode"=Nnode, "edge.length"=edge.length)
  tree$edge <- cbind(matrix(rev(tree$edge), ncol=2)[,2], matrix(rev(tree$edge), ncol=2)[,1])
  tree$edge.length <- rev(tree$edge.length)
  class(tree) <- "phylo"
  return(tree)
}