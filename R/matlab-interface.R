# script for matlab connection evaluation:
# evaluate(matlab, "A=1+2;")
# data <- getVariable(matlab, c("A"))
# 
#   evaluate(matlab, "run('D:/Dropbox/Git/DPHigh-dim/JunctionTree/matlab/r_matlab.m')")
# #evaluate(matlab, "exit")
# close(matlab)
# showConnections()
# closeAllConnections()
# print("finish running matlab script !")

write_clique_matlab <- function(out.dir, filename, domain) {
    if (!exists("matlab") || !isOpen(matlab)) {
      load_matlab()
    }
    
  filepath <- paste(out.dir, filename, ".clique", sep = '')
  clique.lines <- readLines(filepath)
  num.of.clique <- length(clique.lines)
  clique.out <- lapply(seq_along(clique.lines), function(ii) {
    attrs <- unlist(strsplit(clique.lines[[ii]], " "))
    indices <- which(domain$name %in% attrs)
    line <- paste(paste(indices, collapse = " "), "\n", sep = "")
    return(line)
  })
  curr.path <- getwd()
  #FindOptimalMerging8('Data4-sample005-1-CV2.noisy.in', 5, 100, 'Data4-sample005-1-CV2.noisy.merge')
  
  if (num.of.clique > 2){
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    command.file <- paste(out.dir, "/r_matlab", "_", timestamp, ".m", sep = "")
    outfile <- paste(out.dir, filename, ".in", sep = '')
    domain.out <- lapply(seq_along(domain$name), function(ii) {paste(ii, " ", domain$dsize[ii], "\n", sep = "")})
    cat(unlist(domain.out), file = outfile, sep = "")
    cat(paste(paste(rep("-", 20), collapse=""), "\n", sep = "")
        , file = outfile, sep = "", append = TRUE)
    
    cat(unlist(clique.out), file=outfile, sep = "", append = TRUE)
    cat(paste("cd(\'", curr.path, "\')\n", sep = "")
        , file = command.file
        , sep = "")
    cat(paste("addpath(\'", curr.path, "/matlab\')\n", sep = "")
        , file = command.file
        , sep = "", append=TRUE)    
    fin <- paste("\'",out.dir, filename, ".in\'", sep= '')    
    matlab.lines <- lapply(seq.int(from = 2, to = num.of.clique - 1)
                                   , function(ii){
                                      fout <- paste("\'", out.dir, filename,  ".merge", ii, "\'", sep="")
                                      params <- paste(fin, ii, 100, fout, sep=", ")  
                                      line <- paste("FindOptimalMerging8(", params, ")\n", sep = "") 
                                      if (file.exists(fout)) file.remove(fout)
                                      return(line)
                                    })  
    cat(unlist(matlab.lines), file = command.file, append = TRUE, sep="")
    evaluate(matlab, paste("run('", command.file, "')", sep=""))
  }
  close_matlab()
  merge.1 <- paste(out.dir, filename,  ".merge", 1, sep="")
  cat(paste(paste(seq.int(length(domain$name)), collapse=" "), "\n", sep="")
      , file = merge.1, append = FALSE)
  merge.last <- paste(out.dir, filename,  ".merge", num.of.clique, sep="")
  cat(unlist(clique.out), file=merge.last, sep = "", append = FALSE) 
  return()
}

compute_best_clusters_matlab <- function(out.dir, filename, domain) {
  filepath <- paste(out.dir, filename,  ".clique", sep = '')
  outpath <- paste(out.dir, filename,  ".cluster", sep = '')
  clique.lines <- readLines(filepath)
  num.of.clique <- length(clique.lines)
  total.variances <- rep(0, num.of.clique)
  for (ii in seq.int(num.of.clique)) {
    merge.file <- paste(out.dir, filename,  ".merge", ii, sep = '')
    cliques <- readLines(merge.file, warn=FALSE)
    clique.sizes <- lapply(cliques, function(x) {
      indices <- do.call(as.integer,(strsplit(str_trim(x), " ")))
      sizes <- domain$dsize[indices]
      return(sizes)
      
    })
    products <- lapply(clique.sizes, function(x) prod(x))
    total.variances[[ii]] <- 2 * (ii ^ 2) * sum(unlist(products))
  }
  best.merge <- which.min(total.variances)
  best.merge.file <- paste(out.dir, filename,  ".merge", best.merge, sep= '')
  best.cliques <- readLines(best.merge.file, warn=FALSE)
  cluster.out <- lapply(best.cliques, function(x){
    indices <- do.call(as.integer, (strsplit(str_trim(x), " ")))
    attrs <- domain$name[indices]
    line <- paste(paste(attrs, collapse=" "),"\n", sep = "")
    return(line)
  })
  cluster.file <- paste(out.dir, filename,  ".cluster", sep = '')
  cat(unlist(cluster.out), file = cluster.file, append=FALSE, sep="")
}


load_margin <- function(filepath){
  lines <- readLines(filepath)
  num.of.lines <- length(lines)
  margins <- lapply(seq_along(lines), function(ii) {
    attrs <- unlist(strsplit(lines[[ii]], " "))
    return(attrs)
  })
  return(margins)
  
}

load_clusters <- function(out.dir, filename){
  filepath <- paste(out.dir, filename,  ".cluster", sep = '')
  clusters <- load_margin(filepath) 
  return(clusters)
}

load_cliques <- function(out.dir, filename){
  filepath <- paste(out.dir, filename,  ".clique", sep = '')
  cliques <- load_margin(filepath)
  return(cliques)
}
library("R.oo")

load_matlab <- function() {
  closeAllConnections()
  Matlab$startServer(minimize=FALSE, port=9998)
  matlab <<- Matlab(port=9998)
  setOption(matlab,"readResult/maxTries",10000000)
#   setVerbose(matlab, threshold = -2)
  open(matlab)
  while(!isOpen(matlab)){
    print("MATLAB server is not running: wait 30 seconds.")
    Sys.sleep(30)       
  }
}

close_matlab <- function() {
#   browser()
  evaluate(matlab, "pid = feature('getpid')")
  ans <- getVariable(matlab, "pid")
  pid <- as.integer(ans$pid)
  close(matlab)
  while(isOpen(matlab)){
    print("MATLAB server is still running: wait 30 seconds.")
    Sys.sleep(30)       
  }
  system(paste("taskkill /IM", pid, sep=" "))    
  rm(matlab)
}


