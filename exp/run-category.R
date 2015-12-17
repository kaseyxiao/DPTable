# get current directory
full.fpath <- tryCatch(normalizePath(parent.frame(2)$ofile),  # works when using source
                       error=function(e) # works when using R CMD
                         normalizePath(unlist(strsplit(commandArgs()[grep('^--file=', commandArgs())], '='))[2]))
this.dir <- dirname(full.fpath)
code.dir <- paste(dirname(this.dir), "R", sep = "/")
data.dir <- paste(dirname(this.dir), "data", sep = "/")

# set the working directory to the main folder containing all the directories
wd.dir <- dirname(this.dir)
setwd(wd.dir)
#options(error = recover)
options(error = NULL)
source(paste(code.dir, "init.R", sep = "/"))

main <- function(args) {
  exp.specs <- parse_args(args)
  flag.sample <- exp.specs$flag.sample
  data.name <- exp.specs$data.name
  print(exp.specs)
  epsilon.1 <- exp.specs$epsilon.1
  epsilon.2 <- exp.specs$epsilon.2
  CV.thresh <- exp.specs$CV
  nrun<-exp.specs$nrun
  flag.sim <- exp.specs$flag.sim
  flag.process.query <- exp.specs$flag.process.query
#   random_kways = c(4, 6)
#   all_kways = c(2, 3)
  random_kways = c(2)
  all_kways = c(2)
  
  cat("load data: ", data.name, "\n")
  curr.data <- Data$new(data.name)
  tag.sample <- as.character(epsilon.1)
  out.dir <- paste('./output/', data.name
                   , "_CV_", as.character(CV.thresh), "_"
                   ,format(Sys.time(), "%Y%m%d_%H%M%S"), "/"
                   , sep = ""
  ) 

  dir.create(out.dir)
  errors <- ErrorStats(data.name, epsilon.1, epsilon.2, out.dir)
  
  for (i in 1:nrun) {
    tag.run <- paste("-run-",i, sep = "")
    tag.out <- paste(tag.sample, tag.run, sep = "")
    sample.filename <- paste(out.dir, data.name
                             , '-eps1-', tag.out, '.dat', sep = "")
    if (!file.exists(sample.filename)|| flag.sample) {
      beta <- compute_best_sampling_rate_with_Gtest(data.name, curr.data$DB.size
                                                    , epsilon.1
                                                    , curr.data$domain)
      data.file <- curr.data$sample_data(out.dir, rate = beta
                                         , out.tag = paste('-eps1-', tag.out, sep = "")
      )  
    } else {
      data.file <- paste(data.name, '-eps1-', tag.out, sep = "")
    }
    
    
    sample.data <- curr.data$load_sample_data(out.dir, filename = data.file)
    sample.info <- curr.data$load_sample_info(out.dir, filename = data.file)
    beta <- as.numeric(sample.info$sample.rate)
    sample.depgraph <- DependenceGraph$new(sample.data
                                           , flag.sample = TRUE
                                           , flag.noise = TRUE
                                           , beta = beta
                                           , epsilon = epsilon.1
                                           , thresh.CV = CV.thresh
                                           , flag.debug = FALSE
    )
    
    types <- c('CV', 'chi2', 'CV2.noisy', 'Gtest.noisy')
    jtrees <- lapply(types, function(x){
      jtree <- JunctionTree$new(out.dir, edges=sample.depgraph$edges[[x]]
                                , nodes = sample.depgraph$nodes
                                , data.filename = data.file
                                , type=x
                                , flag.debug = TRUE)   
      return(jtree)
    })
    names(jtrees) <- types
    plot(jtrees[['CV2.noisy']]$jtree)
    curr.jtree <- jtrees[['CV2.noisy']]
    type <- "CV2.noisy" 
    # if using exiting junction tree structure
    #     curr.jtree <- JunctionTree$new(flag.build = FALSE
    #                                    , jtree.file = paste("output/",data.file, "-", type, "-jtree.Rdata",sep=""))
    curr.jtree$do_inference_with_merge(
      out.dir
      , curr.data$origin
      , curr.data$domain
      , data.filename = paste(data.file, "-", type, sep="")
      , flag.noise = TRUE
      , do.consistent = TRUE
      , epsilon.2=epsilon.2
      , flag.debug = FALSE
      , flag.matlab = TRUE
    )
    if (flag.sim){
      data.sim <- curr.jtree$simulate(curr.data$DB.size)
      data.sim <- data.sim[, colnames(curr.data$origin)]
      data.sim.file <- paste(out.dir, data.file, "-eps2-", epsilon.2, "-sim.dat",sep="")
      write.table(data.sim
                  , data.sim.file
                  , row.names = FALSE
                  , col.names = FALSE
                  , quote=FALSE
                  , sep = ",")
      
    }
    
    
    #random query
    if (flag.process.query) {
      for(i in seq_along(random_kways)){
        prob.dist <- curr.jtree$distance_kway_marginal(attrs = curr.data$domain$name
                                                       , k = random_kways[i]
                                                       , data.origin = curr.data$origin
                                                       , do.consistent = TRUE
                                                       , flag.random = TRUE
                                                       , num.of.query = 200)
        errors$add_error_stats_record('random', data.file, prob.dist, random_kways[i])
        errors$write_out_errors('random', data.file, random_kways[i])
      }
      for(i in seq_along(all_kways)){
        prob.dist <- curr.jtree$distance_kway_marginal(attrs = curr.data$domain$name
                                                       , k = all_kways[i]
                                                       , data.origin = curr.data$origin
                                                       , do.consistent = TRUE
                                                       , flag.random = FALSE
        )
        errors$add_error_stats_record('all', data.file, prob.dist, all_kways[i])
        errors$write_out_errors('all', data.file, all_kways[i])        
      }
      
    }
    print("finish")
  }
  if (flag.process.query) {
    jtree.file <- paste(data.name, '-eps1-', tag.sample, sep="")
    for(i in seq_along(random_kways)){
      errors$write_out_avg_errors('random', jtree.file, random_kways[i])    
    }
    for(i in seq_along(all_kways)){
      errors$write_out_avg_errors('all', jtree.file, all_kways[i])    
    }    
  }
}
args <- commandArgs(trailingOnly = TRUE)
main(args)