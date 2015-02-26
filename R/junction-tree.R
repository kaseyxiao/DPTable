#' Junction tree class
#' 
#' Build junction tree and do inference
#' 
JunctionTree <- setRefClass(
  "JunctionTree",
  
  fields = list(
    jtree = "ANY", #ripOrder
    POTgrain = "ANY", #POTgrain, graphical model
    POTgrain.consistent = 'ANY', #POTgrain after enforcing consistency
    name = "character",  
    depgraph.type = "character",
    POTlist = "ANY",
    POTlist.consistent = "ANY",
    debug = "logical",
    consistent = "logical",
    with.margin.noise = "logical",
    cluster.noisy.freq = "list",
    cluster.noisy.freq.consistent = "list",
    cluster.freq = "list",
    clique.noisy.freq = "list",
    clique.noisy.freq.consistent = "list",
    clique.freq = "list",
    clusters = "list"
    ),
  
  methods = list(
    
    initialize = function(out.dir, edges = NULL
                          , nodes = NULL
                          , data.filename = NULL
                          , type = NULL
                          , flag.out = TRUE
                          , flag.debug = FALSE
                          , flag.build = TRUE
                          , jtree.file = NULL) {
#       browser()
      .self$debug <- flag.debug
      if(flag.build){
        depgraph <- ugList(edges)
        depgraph <- addNode(setdiff(nodes, 
                                    nodes(depgraph)), depgraph)
        
        .build_junction_tree(depgraph)
        if (flag.out) {
          out.path <- paste(out.dir, data.filename, "-", type, sep="")
          .output_cliques(out.path)
          .output_junction_tree(out.path)
          saveRDS(.self$jtree, file = paste(out.path, '-jtree.Rdata', sep=""))
        }
        
      }else{
        .self$jtree <- readRDS(jtree.file) 
      }
      .self$consistent = TRUE
      .self$with.margin.noise = FALSE
    },
    
    .build_junction_tree = function(depgraph) {
      depgraph <- triangulate(depgraph)
      .self$jtree <- jTree(depgraph)
      plot(.self$jtree)
    },
    
    .output_cliques = function(filepath) {
      file.conn <- file(paste(filepath, '.clique', sep = ""))
      writeLines(unlist(lapply(.self$jtree$cliques, paste, collapse = " ")), file.conn)
      close(file.conn) 
    },
    
    .output_junction_tree = function(filepath) {
      capture.output(.self$jtree, file = paste(filepath, '.jtree', sep = ""))
      #png(paste(filepath, '-jtree.png', sep = ""))
      #jpeg(paste(filepath, '-jtree.jpg', sep = ""))
      plot(.self$jtree)
      dev.off()
    },
           
    
    
    .message_passing = function() {
      gin <- grain(compilePOT(.self$POTlist))
      .self$POTgrain <- propagate(compile(gin))
      if (.self$with.margin.noise && .self$consistent) {
        gin.consistent <- grain(compilePOT(.self$POTlist.consistent))
        .self$POTgrain.consistent <- propagate(compile(gin.consistent))
      }
    },
    simulate = function(num.of.syn, flag.consistent=TRUE, flag.debug=FALSE){
      if(.self$debug && flag.debug) browser() 
      if (flag.consistent) {
        curr.grain <- .self$POTgrain.consistent
      }else {
        curr.grain <- .self$POTgrain
      }
      if(is.null(curr.grain)) stop("POTgrain is not provided yet")
      data.sim <- simulate.grain(curr.grain, num.of.syn)
      return(data.sim)
      
    },

    svm_miss_rate_after_simulation = function(data.origin
                                              , predict.attrs
                                              , data.sim, data.test
                                              , flag.debug = TRUE){
      miss.rates <- lapply(seq_along(predict.attrs), function(ii) {
        if(.self$debug && flag.debug) browser() 
        predict.attr <- predict.attrs[[ii]][['attr.name']]
        f <- as.formula(paste(predict.attr, "~.", sep = ""))
        model.sim <- svm(f, data.sim, kernel="linear")
        res <- predict(model.sim, newdata = data.test)
        predict.values <- predict.attrs[[ii]][['predict.value']]
        sim.results <- unlist(lapply(res, function(x) {x %in% predict.values}))
        true.results <- unlist(lapply(data.test[[predict.attr]], function(x) {x %in% predict.values}))
        miss.rate <- length(which(sim.results != true.results))/length(sim.results)
        return(miss.rate)
      })
      names(miss.rates) <- names(predict.attrs)
      return(miss.rates)
      
    },
    
    svm_miss_rate = function(data.origin, test.attrs, flag.consistent, flag.debug = TRUE) {
      if(.self$debug && flag.debug) browser() 
      if (flag.consistent) {
        curr.grain <- .self$POTgrain.consistent
      }else {
        curr.grain <- .self$POTgrain
      }
      if(is.null(curr.grain)) stop("POTgrain is not provided yet")
      N <- nrow(data.origin)
      data.sim <- simulate.grain(curr.grain, N)
      idx <- floor(0.8 * N)
      test.data <- data.origin[(idx+1) : N, ]
      new.data <- t
      miss.rates <- lapply(seq_along(test.attrs), function(ii) {
        if(.self$debug && flag.debug) browser() 
        test.attr <- test.attrs[ii]
        f <- as.formula(paste(test.attr, "~.", sep = ""))
        model.sim <- svm(f, data.sim[1:idx, ], kernel="linear")
        model.origin <- svm(f, data=data.origin[1:idx,], kernel="linear")
        res <- predict(model.sim, newdata = test.data)
        res.origin <- predict(model.origin, newdata = test.data)
        miss.rate <- length(which(res != test.data[[test.attr]]))/length(res)
        miss.rate.origin <- length(which(res.origin != test.data[[test.attr]]))/length(res.origin)
        return(miss.rate)
      })
      return(miss.rates)
    },
    
    
    
    distance_kway_marginal = function(attrs, k, data.origin, do.consistent = FALSE
                                      , flag.debug = FALSE, flag.random = FALSE, num.of.query = 200) {
#       browser()
      if (flag.random) {
        k.way.attrs <-  get_kway_random_query(attrs, k, num.of.query)
      }else{
        k.way.attrs <- combn(attrs,k)        
      }
      num.k.way <- dim(k.way.attrs)[2]
      total.var.dist <- rep(0, num.k.way)
      L2.error <- rep(0, num.k.way)
      if (do.consistent) {
        total.var.dist.consistent <- rep(0, num.k.way)
        L2.error.consistent <- rep(0, num.k.way)
      }else {
        total.var.dist.consistent <- NA
        L2.error.consistent <- NA
      }
#       kl.dist<-rep(0, num.k.way)
      t <- data.table(data.origin)
      for (i in seq_len(num.k.way)) {
        attr.group<-k.way.attrs[,i]
        margin.noisy<-querygrain(.self$POTgrain, nodes = attr.group, 
                                 type = "joint", result = "data.frame")
        sum.origin=nrow(data.origin)
        setkeyv(t, attr.group)
        curr_levels = lapply(attr.group, function(x) levels(t[, get(x)]))
        # omit "by = .EACHI" in data.table <= 1.9.2
        margin.origin <- t[do.call(CJ, curr_levels)
                 , list(freq.origin = .N), allow.cartesian = T, by = .EACHI]
        N <- sum(margin.origin$freq.origin)
        margin.origin$freq.origin = margin.origin$freq.origin/N
        #merge two results
        margin <- plyr::join(margin.origin, margin.noisy, by=attr.group)
        total.var.dist[i]<-0.5*dist(rbind(margin$freq.origin, margin$Freq)
                                    , method="minkowski", p=1)
        cat(i, "total.var.dist:", total.var.dist[i], "\n")
        L2.error[i]<-dist(rbind(margin$freq.origin*N, margin$Freq*N)
                                     , method="minkowski", p=2)/N
        cat(i, "L2.error:", L2.error[i], "\n")
        

        if (do.consistent && .self$with.margin.noise) {
          if(.self$debug && flag.debug) browser()
          margin.noisy.consistent <- querygrain(.self$POTgrain.consistent, nodes = attr.group, 
                                                type = "joint", result = "data.frame")  
          margin.consistent <- plyr::join(margin.origin, margin.noisy.consistent, by=attr.group)
          total.var.dist.consistent[i]<-0.5*dist(rbind(margin.consistent$freq.origin
                                                       , margin.consistent$Freq)
                                                 , method="minkowski", p=1)
          cat(i, "total.var.dist with consistency:", total.var.dist.consistent[i], "\n")
          L2.error.consistent[i]<-dist(rbind(margin.consistent$freq.origin*N, margin.consistent$Freq*N)
                         , method="minkowski", p=2)/N
          cat(i, "L2.error with consistency:", L2.error.consistent[i], "\n")
        }

#         kl.dist[i]<-dist(rbind(margin$freq.origin, margin$Freq)
#                          , method="minkowski", p=2)
      }
      ans<-list()
      ans$var<-total.var.dist
      ans$var.consistent <- total.var.dist.consistent
      ans$L2.error <- L2.error
      ans$L2.error.consistent <- L2.error.consistent
      return(ans)
    },
    get_kway_random_query = function(attrs, k, num.of.query){
      if (k > length(attrs)) {
        cat("Not enough", k,  "attributes in total", length(domain$name), "attributes")
        stop()
      }
      queries <- list()
      qcount <- 0
      while (qcount < num.of.query) {
        curr.attrs <- sample(attrs, k)
        flag.contain <- any(sapply(queries, function(x) all(sort(x)==sort(curr.attrs))))
        if (!flag.contain){
          queries[[qcount + 1]] <- curr.attrs
          qcount <- qcount + 1
        }else{
          cat(curr.attrs, "\n")
        }
      }
      queries <- do.call(cbind, queries)
      return(queries)
      
    },
    
    kway_random_query = function(data.origin, domain, k, num.of.query, flag.consistency = TRUE) {
      if (k > length(domain$name)) {
        cat("Not enough", k,  "attributes in total", length(domain$name), "attributes")
        stop()
      }
      if (flag.consistency && !is.null(.self$POTgrain.consistent)) {
        curr.grain <- .self$POTgrain.consistent
      }else {
        curr.grain <- .self$POTgrain
      }
      t <- data.table(data.origin)
      N <- nrow(t)
      count.origin <- c()
      count.noisy <- c()
      for (ii in seq_len(num.of.query)) {
        attrs<-sample(domain$name, k)
        random.levels <- lapply(attrs, function(x) sample(domain$levels[[x]], size=1))
        states <- lapply(random.levels, function(x) as.factor(x))
        net <- setEvidence(curr.grain, nodes = attrs, states = as.vector(unlist(states)))
        count.noisy[ii] <- pEvidence(net)*N
        setkeyv(t, attrs)
        count.origin[ii] <- t[states, .N]
      }
      L2.error<-dist(rbind(count.noisy, count.origin)
                                            , method="minkowski", p=2)/N
      MSE <- (1/num.of.query)*(sum((count.origin-count.noisy)^2))
      cat("L2 error:", L2.error, "MSE: ", MSE, "\n")
      ans <- list()
      ans$count.origin <- count.origin
      ans$count.noisy <- count.noisy
      ans$MSE < MSE
      ans$L2.error <- L2.error
      return(ans)
      
    },
    do_inference_with_merge = function(out.dir, data, domain, data.filename
                                       , flag.noise = TRUE
                                       , do.consistent = TRUE
                                       , epsilon.2 = NULL
                                       , flag.debug = FALSE, flag.matlab = TRUE) {
      #use save/load for junction tree
      if(flag.matlab){
        write_clique_matlab(out.dir, data.filename, domain) 
        compute_best_clusters_matlab(out.dir, data.filename, domain)  
      }
      
        
      .self$with.margin.noise <- flag.noise
      if (.self$with.margin.noise) {
        .self$consistent <- do.consistent
      }else {
        .self$consistent <- TRUE
      }

      .self$clusters <- load_clusters(out.dir, data.filename)
      .self$cluster.noisy.freq <- .inject_marginal_noise_data_table(data, epsilon.2, .self$clusters)
      consistency <- ConsistentMargin$new(nrow(data), .self$clusters, domain, .self$cluster.noisy.freq)
      .self$cluster.noisy.freq <- consistency$fix_negative_entry_approx(flag.set = FALSE)
      .self$cluster.noisy.freq.consistent <- consistency$enforce_global_consistency() 
      
      .self$clique.noisy.freq <- set_clique_margin_from_cluster(data, domain, 
                                                                noisy.freq = .self$cluster.noisy.freq)
      .self$clique.noisy.freq.consistent <- set_clique_margin_from_cluster(
        data, domain, noisy.freq = .self$cluster.noisy.freq.consistent)
      .init_potential_data_table(data, .self$clique.noisy.freq)
      .init_potential_data_table(data, .self$clique.noisy.freq.consistent, do.consistent = TRUE)
      .message_passing()
      
    },
    
    
    do_inference = function(data, domain, flag.noise = FALSE
                            , do.consistent = TRUE
                            , epsilon.2 = NULL
                            , flag.debug = FALSE ) {
      if(.self$debug && flag.debug) browser()
      .self$with.margin.noise <- flag.noise
      if (.self$with.margin.noise) {
        .self$consistent <- do.consistent
      }else {
        .self$consistent <- TRUE
      }
      if (.self$with.margin.noise){
        #epsilon.alpha.2 <- amplify_epsilon_under_sampling(epsilon.2, beta)
        .inject_clique_marginal_noise_data_table(data, epsilon.2)
        if (.self$debug && flag.debug) browser()
        consistency <- ConsistentMargin$new(nrow(data), .self$jtree$cliques, domain, .self$clique.noisy.freq)
        .self$clique.noisy.freq <- consistency$fix_negative_entry_approx(flag.set = FALSE)
        .init_potential_data_table(data, .self$clique.noisy.freq)
        
        
        if (.self$consistent) {
          .self$clique.noisy.freq.consistent <- consistency$enforce_global_consistency() 
          .init_potential_data_table(data, .self$clique.noisy.freq.consistent, do.consistent = TRUE)
        }

        
#         print(clique.noisy.freq)
      }else{
        .init_potential_data_table(data)

      }
      .message_passing()
    },

    .inject_marginal_noise_data_table = function(data, epsilon, margins){
      t <- data.table(data)
      margin.noisy.freq<-list()
      sen=1
      num.margins<-length(margins)
      b=2*(num.margins)*sen/epsilon
      Lap<-DExp(rate=1/b)
      for (i in seq_len(length(margins))) {
        cq<-margins[[i]]
        setkeyv(t, cq)
        curr_levels = lapply(cq, function(x) levels(t[,get(x)]))
        # omit "by = .EACHI" in data.table <= 1.9.2
        #t[CJ(levels(A1), levels(A2), levels(A3), .N, allow.cartesian = T, by = .EACHI]
        xxx <- t[do.call(CJ, curr_levels)
                 , list(freq = .N), allow.cartesian = T, by = .EACHI][, freq]
        noises <- r(Lap)(length(xxx))
        freq.noisy <- xxx+noises
        #print(cbind(xxx, freq.noisy))
        margin.noisy.freq[[i]] <- freq.noisy
      } 
    return(margin.noisy.freq)
      
    },

    .set_clusters = function(clusters){
      .self$clusters <- clusters
    },

    match_clique_to_cluster = function(cid){
      clique <- .self$jtree$cliques[[cid]]
      for(ii in seq_len(length(.self$clusters))){
        if(all(clique %in% .self$clusters[[ii]])){
          return(ii)
        }
      }
      return(NULL)
    },
  

    set_clique_margin_from_cluster = function(data, domain, noisy.freq){
      t <- data.table(data)
      cliques <- .self$jtree$cliques
      ans <- list()
      match_ids <- unlist(lapply(seq_along(cliques), function(cid) match_clique_to_cluster(cid)))
      for (i in seq_len(length(cliques))) {
        cl <- .self$clusters[[match_ids[i]]]
        cq <- cliques[[i]]
        curr_cl_levels <- lapply(cl, function(x) domain$levels[[x]])
        index <- data.table(do.call(CJ, curr_cl_levels))
        setnames(index, cl)
        cluster.freq.noisy <- noisy.freq[[match_ids[i]]]
        margin.cluster <- cbind(index, cluster.freq.noisy)   

        curr_cq_levels = lapply(cq, function(x) domain$levels[[x]])
        # omit "by = .EACHI" in data.table <= 1.9.2
        #t[CJ(levels(A1), levels(A2), levels(A3), .N, allow.cartesian = T, by = .EACHI]
        margin.cluster <- data.table(margin.cluster)
        setkeyv(margin.cluster, cq)
        freq.noisy <- margin.cluster[do.call(CJ, curr_cq_levels)
                        , list(Freq=sum(cluster.freq.noisy)), allow.cartesian = T, by = .EACHI][, Freq]
        

        ans[[i]] <- freq.noisy
      }    
      return(ans)
    },

    
    .inject_clique_marginal_noise_data_table = function(data, epsilon.2) {
      t <- data.table(data)
      cliques <- .self$jtree$cliques
      .self$clique.noisy.freq<-list()
      sen=1
      num.cliques<-length(cliques)
      b=2*(num.cliques)*sen/epsilon.2
      Lap<-DExp(rate=1/b)
      for (i in seq_len(length(cliques))) {
        cq<-cliques[[i]]
        setkeyv(t, cq)
        curr_levels = lapply(cq, function(x) levels(t[,get(x)]))
        # omit "by = .EACHI" in data.table <= 1.9.2
        #t[CJ(levels(A1), levels(A2), levels(A3), .N, allow.cartesian = T, by = .EACHI]
        xxx <- t[do.call(CJ, curr_levels)
                 , list(freq = .N), allow.cartesian = T, by = .EACHI][, freq]
        noises <- r(Lap)(length(xxx))
        freq.noisy <- xxx+noises
        #print(cbind(xxx, freq.noisy))
        .self$clique.noisy.freq[[i]] <- freq.noisy
      } 
    },
    .init_potential_data_table = function(data, clique.noisy.freq = NULL, do.consistent = FALSE, flag.debug = FALSE) {
      cliques<-.self$jtree$cliques
      seps<-.self$jtree$separators
      ans <- vector("list", length(cliques))
      N=nrow(data)
      t <- data.table(data)
      for (ii in seq_along(cliques)){
        print(ii)
        cq  <- cliques[[ii]]
        sp  <- seps[[ii]]
        setkeyv(t, cq)
        curr_levels = lapply(cq, function(x) levels(t[,get(x)]))
        # omit "by = .EACHI" in data.table <= 1.9.2
        #t[CJ(levels(A1), levels(A2), levels(A3), .N, allow.cartesian = T, by = .EACHI]
        xxx <- t[do.call(CJ, curr_levels)
                 , list(freq = .N), allow.cartesian = T, by = .EACHI]
        if (.self$debug && flag.debug) browser()
        .self$clique.freq[[ii]] <- xxx[, freq]
        
        if(length(clique.noisy.freq) > 0){
          freq.noisy <- clique.noisy.freq[[ii]]
          xxx[, freq.noisy:=freq.noisy]
#           print(xxx)
          f <- as.formula(paste("freq.noisy~", paste(cq, collapse = "+")))
          xxx <- xtabs(formula = f, data = xxx) 
        }else{
          f <- as.formula(paste("freq~", paste(cq, collapse = "+")))
          xxx <- xtabs(formula = f, data = xxx)           
        }
        #ftable(t.cq)
        t.cq <- tableMargin(xxx, cq)
        names(dimnames(t.cq)) <- cq
        
        if (!is.null(seps) && length(sp)>0){
          t.sp      <- tableMargin(t.cq, sp)
          ans[[ii]] <- tableOp2(t.cq, t.sp, op=`/`)
        } else {
          ans[[ii]] <- t.cq / sum(t.cq)
        }   
      }
      attr(ans, "rip")     <-.self$jtree    
      if (!is.null(clique.noisy.freq) && do.consistent) {
        .self$POTlist.consistent <- ans 
      }else {
        .self$POTlist <- ans
      }
      
    }
  )
)


