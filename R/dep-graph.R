#' class for construct dependence graph
#' 
DependenceGraph <- setRefClass(
  "DependenceGraph",
  
  fields = list(
    beta = "numeric",
    epsilon.1 = "numeric",
    N = "numeric",
    edges = "list",
    nodes = "vector",
    flag.all.binary = "logical",
    pairwise.table = "data.frame",
    thresh = "list",
    debug = "logical"
    ),
  
  methods = list(
    initialize = function(data, flag.sample, flag.noise
                          , beta = 1, epsilon.1 = NULL
                          , thresh.CV = 0.2, thresh.pvalue = 1e-6
                          , flag.debug = FALSE) {
      .self$debug <- flag.debug
      if (.self$debug) browser("debug dep graph class")
      .self$N <- nrow(data$rows)
      .self$thresh$CV <- thresh.CV
      .self$thresh$pvalue <- thresh.pvalue
      .self$edges = list('CV' = list(), 'chi2' = list(), 'CV2.noisy' = list()
                         , 'Gtest.noisy' = list())
      .self$nodes = data$domain$name
      .check_all_binary(data$domain)
      .self$pairwise.table <-data.frame(dk.name = character()
                                        , dl.name = character()
                                        , dk = integer()
                                        , dl = integer()
                                        , chi2 = numeric()
                                        , mi = numeric()
                                        , Gtest.LH = numeric()
                                        , Gtest.RH = numeric()
                                        , chi2.critical = numeric()
                                        , CV = numeric()
                                        , CV2.LH = numeric()
                                        , CV2.RH = numeric()
                                        , pvalue = numeric()
                                        
      )
      
      
      if (flag.sample && flag.noise){
        #from util.R
        if (.self$debug) browser()
        epsilon.alpha.1 <- amplify_epsilon_under_sampling(epsilon.1, beta)
        sensitivity.scale.mi <- compute_mi_sensitivity_scale(.self$N, .self$flag.all.binary)
        b.scale.mi <- 2 * sensitivity.scale.mi / epsilon.alpha.1
        print(paste("beta", beta
                    , "epsilon alpha", epsilon.alpha.1
                    , "sample size", .self$N))
      }
      .construct_dep_graph(data, flag.noise, b.scale.mi)
    },

    .check_all_binary = function(domain) {
      #check whether attrs are all binary
      .self$flag.all.binary <- (length(which(domain$dsize != 2)) == 0)     
    }, 
    
    .get_xtable_expected_sum = function(xtable) {
      rsums <- rowSums(xtable)
      rsums <- matrix(rsums, nrow = length(rsums), ncol = 1)
      csums <- colSums(xtable)
      csums <- matrix(csums, nrow = 1, ncol = length(csums))
      table.sum <- sum(rsums)
      expected_sum <- rsums %*% csums / table.sum
      return(expected_sum)
    },
    
    .filter_association_edges = function(pair, measure, bar, type) {
      if(measure >= bar){
        curr_length <- length(.self$edges[[type]])
        .self$edges[[type]][[curr_length + 1]]<- pair
      }      
    },
    
    .construct_dep_graph = function(data, flag.noise, b.scale.mi = NULL) {
      if(.self$debug) browser()
      if (flag.noise && is.numeric(b.scale.mi)) {
        #convert all measures of association to mi 
        #in DExp function, lambda is 1/b, b is the scale of Laplace, 1/(2*b)*exp^(-|x|/b)
        Lap.Gtest <- DExp(rate = 1 / b.scale.mi)
        noise.thresh.Gtest <- r(Lap.Gtest)(1) #get one Laplacian noise
        Lap.CV2 <- DExp(rate = 1 / b.scale.mi)
        noise.thresh.CV2 <- r(Lap.CV2)(1)        
      }     
      pairs <- combn(as.vector(data$domain$name), 2)
      for (i in seq_len(ncol(pairs))) {
#         if(.self$debug) browser()
        pair <- pairs[,i]
        dk.name <- pair[1]
        dl.name <- pair[2]
        curr_xtab <- xtabs(formula = ~ get(dk.name) + get(dl.name), data = data$rows)
        dk <- data$domain$dsize[which(data$domain$name == dk.name)]
        dl <- data$domain$dsize[which(data$domain$name == dl.name)]
        #compute measures of association       
        #compute chi2
        expected_sum <- .get_xtable_expected_sum(curr_xtab)
        chi2 <- sum((curr_xtab - expected_sum) ** 2 / expected_sum, na.rm = TRUE)
        pvalue <- 1 - pchisq(chi2, df = (dk - 1) * (dl - 1))
        chi2.critical <- qchisq((1 - .self$thresh[['pvalue']]), df = (dk - 1) * (dl - 1))
        .filter_association_edges(pair, chi2, chi2.critical, type = "chi2")
        
        #compute mi and Gtest
        mi <- mi.empirical(curr_xtab, unit = 'log')
        
        CV <- sqrt(chi2 / (.self$N * (min(dk, dl) - 1)))
        .filter_association_edges(pair, CV, .self$thresh[['CV']], type = "CV")
        
        if (flag.noise) {
          CV2.RH <- (.self$thresh[['CV']] ^ 2) * (min(dk, dl) - 1)/2 + noise.thresh.CV2
          CV2.LH <- mi + r(Lap.CV2)(1)
          if(.self$debug && CV2.LH > CV2.RH) {
            print(paste("CV", CV, "mi", mi))
            browser()
          }           
          .filter_association_edges(pair, CV2.LH, CV2.RH, 'CV2.noisy')
          Gtest.LH <- mi + r(Lap.Gtest)(1) 
          Gtest.RH <- chi2.critical / (2 * .self$N) + noise.thresh.Gtest
          .filter_association_edges(pair, Gtest.LH, Gtest.RH, 'Gtest.noisy')          
        }else{
          CV2.LH <- mi
          CV2.RH <- (.self$thresh[['CV']] ^ 2) * (min(dk, dl) - 1) / 2
          Gtest.LH <- mi
          Gtest.RH <- chi2.critical / (2 * .self$N)
          
        }
        .append_pairwise_association_table(dk.name, dl.name
                                           , dk, dl
                                           , chi2, mi
                                           , Gtest.LH, Gtest.RH
                                           , chi2.critical
                                           , CV, CV2.LH, CV2.RH
                                           , pvalue)
        
      }      
      
    },
    
    .append_pairwise_association_table = function(dk.name, dl.name, dk, dl
                                                  , chi2, mi, Gtest.LH, Gtest.RH
                                                  , chi2.critical
                                                  , CV, CV2.LH, CV2.RH, pvalue) {
      newrow<-data.frame(dk.name = dk.name
                         , dl.name = dl.name
                         , dk = dk
                         , dl = dl
                         , chi2 = chi2
                         , mi = mi
                         , Gtest.LH = Gtest.LH
                         , Gtest.RH = Gtest.RH
                         , chi2.critical = chi2.critical
                         , CV = CV
                         , CV2.LH = CV2.LH
                         , CV2.RH = CV2.RH
                         , pvalue = pvalue
      )
      .self$pairwise.table<-rbind(.self$pairwise.table,newrow)
      
    }
    
    )
)


