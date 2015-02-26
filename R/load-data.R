#' Load data.
#' 
#' Load files in the data subdirectory.
#'
#' 
sample_data <- function(dataname, filename, rate, out.tag = '') {
  data.path <- file.path('.', 'data', filename)
  data.raw <- read.csv(
    file = paste(data.path, '.dat', sep='')
    , header=FALSE,
    , check.names=FALSE
  )
  Dsize = nrow(data.raw)
  data.sample <- data.raw[sample(Dsize, round(nrow(data.raw)*rate)), ]
  print(nrow(data.sample))
  out.path <- file.path('.', 'output', dataname)
  out.name = paste(out.path, '-sample', as.character(out.tag), sep="")
  write.table(data.sample
            , file=paste(out.name, '.dat', sep="")
            , row.names=FALSE
            , col.names=FALSE
            , sep=","
            )
    out.info = list(data.name=dataname
         , data.size=Dsize
         , sample.name = out.name
         , sample.rate=rate
         , sample.size=nrow(data.sample)
         )
  out.info.table = do.call(rbind.data.frame, out.info)
  write.table(out.info.table
              , file=paste(out.name, '.info', sep="")
              , col.names=FALSE
                )
  return(paste(dataname, '-sample', as.character(out.tag), sep=""))

}

load_coarse_data_summary<-function(dataname){
  path <- file.path('.', 'data', dataname)
  data.summary <- list()
 
  file.conn<-file(paste(path, '-coarse.domain', sep=''))
  domain.info<-readLines(file.conn)
  close(file.conn)
  domain.info<-lapply(domain.info, function(x) unlist(strsplit(x, split=" ")))
  data.summary$Dsize = as.integer(domain.info[[1]][2])
  domain.info[1]<-NULL
  data.summary$domain<-list()

  data.summary$domain$name<-sapply(domain.info, function(x) paste('A',x[1], sep=""))
  data.summary$domain$category<-sapply(domain.info, function(x) x[2])
  data.summary$domain$dsize<-sapply(domain.info, function(x) as.integer(x[3]))
  
  process_levels<-function(x){
    category<-x[2]
    dsize<-as.integer(x[3])
    levels=c()
    if(x[2]=="C"){
      levels<-x[4:(dsize+3)]
    }else{
      levels<-as.integer(x[4:(dsize+3)])
    }
    return(levels)
  }
  data.summary$domain$levels<-lapply(domain.info, function(x) process_levels(x))
  browser()
  names(data.summary$domain$levels) <- data.summary$domain$name
  return(data.summary)  
}

load_data_summary <- function(dataname){
  path <- file.path('.', 'data',dataname)
  data.summary <- list()
  summary.head <- read.table(
    file=paste(path, '.domain', sep='')
    , header = FALSE
    , nrows = 1
    , check.names=FALSE
  )
  data.summary$Dsize = as.integer(summary.head[2])
  data.summary$domain <- read.table(
    file=paste(path, '.domain', sep='')
    , header = FALSE
    , skip = 1
    , col.names = c('name', 'category', 'dsize')
    , colClasses = c("character", "character", "numeric")
    , check.names=FALSE
  )
  return(data.summary)
}

load_sample_info <- function(dataname, filename){
  path <- file.path('.', 'output',filename)
  sample.info<-list()
  info.table<-read.table(file=paste(path, '.info', sep=''))
  for(i in 1:nrow(info.table)){
    key = as.character(info.table[i,1])
    value = as.character(info.table[i,2])
    sample.info[[key]]=value
  }
  return(sample.info)
}

load_sample_data <- function(dataname, filename){
  data <- list()
  path <- file.path('.', 'output',filename)
  domain <- load_coarse_data_summary(dataname)$domain
  rows <- read.csv(file = paste(path,'.dat', sep='')
    , header=FALSE
    , col.names=domain$name
    , check.names=FALSE
    , colClasses=rep('factor', length(domain$name))
  )
  rows<-as.data.frame(rows)
  for(col.name in colnames(rows)){
    rows[col.name]<-lapply(rows[col.name], factor, levels=domain$levels[[which(domain$name==col.name)]])
  }
  data$rows=rows
  data$domain=domain
  return(data)
}
load_coarse_data<-function(dataname){
  data <- list()
  path <- file.path('.', 'data', dataname)
  domain <- load_coarse_data_summary(dataname)$domain
  rows <- read.csv(file = paste(path, '-coarse.dat', sep='')
                   , header=TRUE
                   , col.names=domain$name
                   , colClasses=rep('factor', length(domain$name))
                   , check.names=FALSE
  )
  rows<-as.data.frame(rows)

  for(col.name in colnames(rows)){
    rows[col.name]<-lapply(rows[col.name], factor, levels=domain$levels[[which(domain$name==col.name)]])
  }
  data$rows=rows
  data$domain=domain
  return(data)  
}



