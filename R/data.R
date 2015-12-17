Data <- setRefClass(
  "Data",
  
  fields = list(
    origin = "ANY",
    name = "character",
    domain = "ANY",
    DB.size = "numeric"
    ),
  
  methods = list(
    
    initialize = function(data.name) {
      .self$name <- data.name
      .load_coarse_data_summary()
      .load_coarse_data()
    }, 
    
    sample_data = function(out.dir, rate, out.tag = '') {
      data.sample <- .self$origin[sample(.self$DB.size, round(.self$DB.size * rate)), ]
      # print(nrow(data.sample))
      out.path <- file.path(out.dir, .self$name)
      out.name = paste(out.path, as.character(out.tag), sep = "")
      write.table(data.sample
                  , file = paste(out.name, '.dat', sep="")
                  , row.names = FALSE
                  , col.names = FALSE
                  , sep = ","
      )
      out.info = list(data.name = .self$name
                      , data.size = .self$DB.size
                      , sample.name = out.name
                      , sample.rate = rate
                      , sample.size = nrow(data.sample)
      )
      out.info.table = do.call(rbind.data.frame, out.info)
      write.table(out.info.table
                  , file = paste(out.name, '.info', sep = "")
                  , col.names = FALSE
      )
      return(paste(.self$name, as.character(out.tag), sep = ""))
      
    },
    
    .load_coarse_data_summary = function(){
      .self$domain <- list()
      dataname <- .self$name
      path <- file.path('.', 'Data', dataname)
      file.conn <- file(paste(path, '-coarse.domain', sep = ''))
      domain.info <- readLines(file.conn)
      close(file.conn)
      domain.info <- lapply(domain.info, function(x) unlist(strsplit(x, split = " ")))
      domain.info[1] <- NULL
      
      .self$domain$name <- sapply(domain.info, function(x) paste('A',x[1], sep = ""))
      .self$domain$category <- sapply(domain.info, function(x) x[2])
      .self$domain$dsize <- sapply(domain.info, function(x) as.integer(x[3]))
      
      process_levels <- function (x) {
        category <- x[2]
        dsize <- as.integer(x[3])
        levels <- c()
        if (x[2] == "C") {
          levels <- x[4:(dsize+3)]
        } else {
          levels <- as.integer(x[4: (dsize + 3)])
        }
        return(levels)
      }
      .self$domain$levels <- lapply(domain.info, function(x) process_levels(x))
      names(.self$domain$levels) <- .self$domain$name
    },
    
    
    load_sample_info = function(out.dir, dataname, filename){
      path <- file.path(out.dir,filename)
      sample.info <- list()
      info.table <- read.table(file = paste(path, '.info', sep = ''))
      for (i in seq_len(nrow(info.table))) {
        key <- as.character(info.table[i, 1])
        value <- as.character(info.table[i, 2])
        sample.info[[key]] <- value
      }
      return(sample.info)
    },
    
    load_sample_data = function(out.dir, filename){
      data <- list()
      path <- file.path(out.dir, filename)
      rows <- read.csv(file = paste(path,'.dat', sep = '')
                       , header = FALSE
                       , col.names = .self$domain$name
                       , check.names = FALSE
                       , colClasses = rep('factor', length(.self$domain$name))
      )
      rows <- as.data.frame(rows)
      for(col.name in colnames(rows)){
        rows[col.name] <- lapply(rows[col.name]
                               , factor
                               , levels = .self$domain$levels[[
                                 which(.self$domain$name == col.name)]])
      }
      data$rows <- rows
      data$domain <- domain
      return(data)
    },
    
    .load_coarse_data = function() {
      .self$origin <- list()
      path <- file.path('.', 'Data', .self$name)
      rows <- read.csv(file = paste(path, '-coarse.dat', sep = '')
                       , header = FALSE
                       , col.names = .self$domain$name
                       , colClasses = rep('factor', length(.self$domain$name))
                       , check.names = FALSE
      )
      rows <- as.data.frame(rows)
      
      for (col.name in colnames(rows)) {
        rows[col.name]<-lapply(rows[col.name], factor
                               , levels = .self$domain$levels[[
                                 which(.self$domain$name == col.name)]])
      }
      .self$origin <- rows
      .self$DB.size = as.integer(nrow(rows))
      print(paste("Dataset size:", .self$DB.size))
       
    }
    )
)