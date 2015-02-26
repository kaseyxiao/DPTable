ErrorStats <- setRefClass(
  "error statistics class",
  fields = list(
    out.dir = "character",
    data.name = "character",
    epsilon.1 = "numeric",
    epsilon.2 = "numeric",
    errors = "ANY",
    svm.miss.rates = "ANY"
    ),
  methods = list(
    initialize = function(data.name, epsilon.1, epsilon.2, out.dir){
      .self$out.dir <- out.dir
      .self$data.name <- data.name
      .self$epsilon.1 <- epsilon.1
      .self$epsilon.2 <- epsilon.2
      .self$errors <- data.table(query.type = character()
                                 , jtree.file = character()
                                 , L2.error = numeric()
                                 , L2.error.consistent = numeric()
                                 , total.var = numeric()
                                 , total.var.consistent = numeric()
                                 , kway = numeric()
                                 , timestamp = character()
                                 , nquery = numeric()
                                 )
      .self$svm.miss.rates <- data.table(attr.content = character()
                                         , attr.name = character()
                                         , miss.rate = numeric()
                                         , timestamp = character())
    },
    add_svm_record = function(attr.content, attr.name, miss.rate, timestamp=NULL){
      if(is.null(timestamp)){
        timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")        
      }
      .self$svm.miss.rates <- rbind(.self$svm.miss.rates, list(attr.content, attr.name
                                                               , miss.rate, timestamp))
      
    },
    write_out_avg_svm_miss_rates = function(jtree.file){
      timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
      out.file <- paste(.self$out.dir, jtree.file
                        , "-eps2-", .self$epsilon.2
                        , "-avg-svm.error"
                        , sep="")
      out <- paste(paste("data.name", "attr.content", "attr.name"
                         ,"epsilon.1", "epsilon.2"
                         , "avg.miss.rate", "timestamp", sep="\t")
                   , "\n", sep="")
      cat(out, file=out.file)
      write_out_svm_avg_miss_rates <- function(attr.content, attr.name, avg.miss.rate){
        line <- paste(.self$data.name
                      , attr.content, attr.name
                      , .self$epsilon.1, .self$epsilon.2
                      , format_decimal(avg.miss.rate)
                      , timestamp
                      , sep = "\t"
        )
        return(line)
      }
      
      
      attr.content <- unique(.self$svm.miss.rates$attr.content)
      setkey(.self$svm.miss.rates, "attr.content")
      out<-lapply(attr.content, function(x){
        x <- as.character(x)
        records <- .self$svm.miss.rates[x,]$miss.rate
        attr.name <- unique(.self$svm.miss.rates[x,]$attr.name)
        avg.miss.rate <- sum(records)/length(records)
        line <-  write_out_svm_avg_miss_rates(x, attr.name, avg.miss.rate)
        return(line)
      })
      cat(unlist(out), file=out.file, sep="\n", append=TRUE)
    },
    
    write_out_svm_miss_rates = function(jtree.file){
      out.file <- paste(.self$out.dir, jtree.file, "-eps2-", .self$epsilon.2
                        , "-svm.error", sep="")
      out <- paste(paste("data.name", "attr.content", "attr.name"
                         ,"epsilon.1", "epsilon.2"
                         , "miss.rate", "timestamp", sep="\t")
                   , "\n", sep="")
      cat(out, file=out.file)
      write_out_svm_record <- function(record){
        line <- paste(.self$data.name
                     , record[['attr.content']], record[['attr.name']]
                     , .self$epsilon.1, .self$epsilon.2
                     , format_decimal(record[['miss.rate']]), record[['timestamp']]
                     , sep = "\t"
                     )
        return(line)
      }

      out <- lapply(seq_len(nrow(.self$svm.miss.rates)), function(ii){
        write_out_svm_record(.self$svm.miss.rates[ii,])
        })
      cat(unlist(out), file=out.file, sep = "\n", append = TRUE)
      
      
      
    },
    add_error_stats_record_from_file = function(err.stats){
      .self$errors <- rbind(.self$errors, list(as.character(err.stats$query.type)
                                               , as.character(err.stats$jtree.file)
                                               , as.numeric(err.stats$L2.error)
                                               , as.numeric(err.stats$L2.consistent)
                                               , as.numeric(err.stats$total.var)
                                               , as.numeric(err.stats$total.var.consistent)
                                               , as.numeric(err.stats$k.way)
                                               , as.character(err.stats$timestamp)
                                               , as.numeric(err.stats$num.of.query)
      )
      )
      
    },
    
    add_error_stats_record = function(qtype, jtree, prob.dist, k){
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      L2.error <- sum(prob.dist$L2.error)/length(prob.dist$L2.error)
      L2.error.consistent <- sum(prob.dist$L2.error.consistent)/length(prob.dist$L2.error.consistent)
      total.var <- sum(prob.dist$var)/length(prob.dist$var)
      total.var.consistent <- sum(prob.dist$var.consistent)/length(prob.dist$var.consistent)
      nquery <- length(prob.dist$L2.error)
      .self$errors <- rbind(.self$errors, list(qtype
                                               , jtree
                                               , L2.error
                                               , L2.error.consistent
                                               , total.var
                                               , total.var.consistent
                                               , k
                                               , timestamp
                                               , nquery
                                               )
                            )
    },
    write_out_errors = function(qtype, jtree, k){
      ndigit = 10
      setkeyv(.self$errors, c("query.type", "jtree.file", "kway"))
      record <- .self$errors[.(qtype, jtree, k), nomatch=0]
      eps2.tag = paste(unlist(strsplit(as.character(.self$epsilon.2), split="\\."))
                       , sep="", collapse="")
      
      out.file <- paste(.self$out.dir, jtree, "-eps2-", .self$epsilon.2
                        , "-", qtype, "-", k, "way",".error", sep="")
      
      cat(paste("data.name", "jtree.file"
                , "epsilon.1", "epsilon.2"
                , "query.type", "k.way", "num.of.query"
                , "L2.error", "L2.consistent"
                , "total.var", "total.var.consistent"
                , "timestamp", sep=",")
          , file=out.file
          , sep = "\n"
          )
      out <- c(paste(.self$data.name
               , jtree
               , .self$epsilon.1
               , .self$epsilon.2
               , qtype
               , k
               , record[, nquery]
               , format_decimal(record[, L2.error])
               , format_decimal(record[, L2.error.consistent])
               , format_decimal(record[, total.var])
               , format_decimal(record[, total.var.consistent])
               , record[, timestamp], sep = ","
               ))
      cat(out
          , file=out.file
          , sep="\n"
          , append = TRUE
          )
    },
    format_decimal = function(num){
      ndigit = 10
      return(format(round(num, ndigit), nsmall=ndigit)) 
    },
    write_out_avg_errors = function(qtype, jtree, k){
      ndigit = 10
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      setkeyv(.self$errors, c("query.type", "kway"))
      records = .self$errors[.(qtype, k), nomatch=0]
      eps2.tag = paste(unlist(strsplit(as.character(.self$epsilon.2), split="\\."))
                      , sep="", collapse="")
      out.file = paste(.self$out.dir, jtree, "-eps2-", .self$epsilon.2 
                       , "-avg-", qtype, "-", k, "way",  ".error", sep="")
      cat(paste("data.name", "jtree.file"
                , "epsilon.1", "epsilon.2"
                , "query.type", "k.way", "num.of.run"
                , "mean.L2.error", "var.L2.error"
                , "mean.L2.error.consistent", "var.L2.error.consistent"
                , "mean.total.var", "var.total.var"
                , "mean.total.var.consistent", "var.total.var.consistent"
                ,sep=",")
          , file=out.file
          , sep = "\n"
      )      
      out <- c(paste(.self$data.name
               , .self$epsilon.1
               , .self$epsilon.2
               , qtype
               , k
               , nrow(records)
               , format_decimal(records[, mean(L2.error)])
               , format_decimal(records[, var(L2.error)])
               , format_decimal(records[, mean(L2.error.consistent)])
               , format_decimal(records[, var(L2.error.consistent)])
               , format_decimal(records[, mean(total.var)])
               , format_decimal(records[, var(total.var)])
               , format_decimal(records[, mean(total.var.consistent)])
               , format_decimal(records[, var(total.var.consistent)])           
               , sep=","))
      cat(out, file=out.file, sep="\n", append = TRUE)
      
    }
    )
  )