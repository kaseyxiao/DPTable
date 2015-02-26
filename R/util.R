#'computing epsilon.alpha if using sampling under differential privacy
amplify_epsilon_under_sampling <- function(epsilon, sample.rate){
  epsilon.alpha <- log(exp(1)**(epsilon) - 1 + sample.rate) - log(sample.rate)
  return(epsilon.alpha)
}

compute_best_sampling_rate_with_Gtest = function(data.name,DB.size, epsilon.1, domain) {
  #e.g. e=0.05, N=300000, \beta=0.25
  init.array <- seq(0, 1, by = 0.001)
  beta.array <- init.array[2: length(init.array)]
  flag.all.binary <- (length(which(domain$dsize != 2)) == 0)
  get_noise_scale <- function(size, eps, x){
    N = size * x
    epsilon.alpha <- amplify_epsilon_under_sampling(eps, x)
    sensitivity.scale <- compute_Gtest_sensitivity_scale(N, flag.all.binary)
    b <- 2 * sensitivity.scale / epsilon.alpha
    return(b)
  }
  b.array <- lapply(beta.array, function(x) get_noise_scale(DB.size, epsilon.1, x))
  plot(beta.array, b.array, xlab = 'sampling rate', ylab = 'noise scale', type = 'l'
       , main = paste("data:", data.name,"; epsilon.1 =", epsilon.1))
  b.min <- min(unlist(b.array))
  beta <- beta.array[which(b.array == b.min)]
  return(beta)
}
compute_Gtest_sensitivity_scale = function(N, flag.all.binary) {
  mi.sen <- compute_mi_sensitivity_scale(N, flag.all.binary)
  Gtest.scale <- 2 * mi.sen
  return(Gtest.scale)
  
}

compute_mi_sensitivity_scale = function(N, flag.all.binary) {
  if(flag.all.binary){
    sensitivity.scale <- (1 / N) * log(N) + ((N - 1) / N) * log(N / (N - 1))
  }else{
    sensitivity.scale <- (2 / N) * log((N + 1) / 2) + ((N - 1) / N) * log((N + 1) / (N - 1))
  } 
  return(sensitivity.scale)
}

# single_junction_tree_construction <- function(edges, nodes, data, data.filename, type
#                                               , flag.noise, flag.consistent = FALSE
#                                               , epsilon.2 = NULL) {
#   jtree <- JunctionTree$new(edges, nodes, data.filename, type)   
#   jtree$do_inference(data, flag.noise, flag.consistent, epsilon.2)
#   return(jtree)
#   
# }
# 
# batch_junction_tree_construction <- function(depgraph, data, data.filename
#                                              , flag.noise , flag.consistent = FALSE
#                                              , epsilon.2 = NULL) {
#   jtrees <- list()
#   for (type in names(depgraph$edges)) {
#     jtrees[[type]] <- single_junction_tree_construction (depgraph$edges[[type]]
#                                                          , depgraph$nodes, data, data.filename, type
#                                                          , flag.noise, flag.consistent, epsilon.2)
#   }
#   return(jtrees)
#  
# }