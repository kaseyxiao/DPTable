library("caret")

#read domain file

read_domain <- function(data.id) {
  path <- file.path('.', 'Data', paste("Data", data.id, sep=""))
  file.conn <- file(paste(path, '.dom', sep = ''))
  domain.info <- readLines(file.conn)
  close(file.conn)
  domain.info<-lapply(domain.info, function(x) unlist(strsplit(x, split=" ")))
  #       .self$DB.size = as.integer(domain.info[[1]][2])
  domain.size <- as.integer(domain.info[[1]])
  domain.info[1]<-NULL
  domain <- NULL
  domain$info <- domain.info
  domain$name<-sapply(seq(domain.size), function(x) paste('A',x[1], sep=""))
  domain$category<-sapply(domain.info, function(x) x[1])
  process_category <- function (x) {
    category <- x[1]
    levels <- c()
    if (x[1] == "C") {
      category <- "integer"
    } else {
      category <- "factor"
    }
    return(category)
  }
  domain$category <- sapply(domain.info, function(x) process_category(x))
  return(domain)  
}


#read data file
read_data <- function(data.name, data.id, domain) {
  path <- file.path('.', 'Data', paste(data.name, data.id, sep=""))
  rows <- read.table(file = paste(path, '.dat', sep = '')
                     , header = FALSE
                     , skip = 1
                     , check.names = FALSE
                     , sep= " "
  )
  rows[, ncol(rows)] <- NULL
  colnames(rows) <- domain$name
  rows <- as.data.frame(rows)
  for(ii in 1:ncol(rows)){
    col.name <- domain$name[ii]
    if(domain$category[ii] == "factor"){
      rows[col.name]<-lapply(rows[col.name], factor
                             , levels = domain$info[[ii]][2:length(domain$info[[ii]])])   
    }
    
  }
  return(rows)
}

normalize_data <- function(data){
  l2norm<-apply(data, 1, function(x) sqrt(sum(x^2)))
  data.norm <- data/l2norm
  return(data.norm)
}

transform_data <- function(predict.attr, data.train, data.test
                           , pos.values, normalize=TRUE, scale=TRUE) {
  f <- as.formula(paste(predict.attr, "~.", sep = ""))
  rows <- rbind(data.train, data.test)
  dummies <- dummyVars(f, data = rows)
  xxx <- predict(dummies, newdata = rows)
  preProcValues <- preProcess(xxx, method=c("range"))
  xxx <- predict(preProcValues, xxx)
  response <-   sapply(rows[, predict.attr], function(x) {
    if (x %in% pos.values){
      return(1)
    }else{
      return(-1)
    }
  })
  if (normalize){
    xxx <- normalize_data(xxx)
  }
  train <- NULL
  test <- NULL
  train.size <- nrow(data.train)
  train$rows <- xxx[1:train.size, ]
  test$rows <- xxx[(train.size+1):nrow(xxx), ]
  train$response <- response[1:train.size]
  test$response <- response[(train.size+1):nrow(xxx)]
  data.scale <- NULL
  data.scale$train <- train
  data.scale$test <- test
  return(data.scale)
}

compute_miss_rate <- function(predict, real){
  return(length(which(predict!=real))/length(real))
}
compute_svm <- function(w, x, bias){
  ans <- t(as.matrix(w) %*% t(as.matrix(x)) - bias)
  ans <- sapply(ans, FUN=get_sign)
}
read_objective_result <- function(in.file){
  ans <- NULL

  info <- read.table(in.file)
  d <- ncol(info) - 1
  ans$w <- info[1,c(1:d)]
  ans$w_output <- info[2, c(1:d)]
  ans$w_obj <- info[3,c(1:d)]
  return(ans)
}
write_transformed_data <- function(out.file, task.attr, predict.attr, x, y){
  lines <- c(paste("predict.name", task.attr),
             paste("predict.attr", predict.attr),
             paste("feature.dim", ncol(x)))
  cat(lines, file=out.file, sep="\n")
  data <- cbind(y, x)
#   colnames(data) <- c(predict.attr, colnames(x))
  write.table(data
              , out.file
              , row.names = FALSE
              , col.names = FALSE
              , quote=FALSE
              , sep = "\t"
              , append=TRUE)
  
}
write_objective_data <- function(out.file, x, y, epsilon){
  h <- 0.5
  lambda <- 1.0
  line <- paste(nrow(x), ncol(x), lambda, epsilon, h, sep="\n")
  cat(line, file=out.file, sep = "\n")
  write.table(x
              , out.file
              , row.names = FALSE
              , col.names = FALSE
              , quote=FALSE
              , sep = "\t"
              , append=TRUE)
  write.table( y
               , out.file
               , row.names = FALSE
               , col.names = FALSE
               , quote=FALSE
               , sep = "\n"
               , append=TRUE)
}
read_objective_data <- function(filename){
  xxx<-readLines(filename)
  n <- as.integer(xxx[1])
  data<-xxx[6:(5+n)]
  library(stringr)
  data<- lapply(data,  function(x) {
    return(as.numeric(unlist(strsplit(str_trim(x), split="\t+"))))
  }
  )
  data<-do.call(rbind.data.frame, data)
  colnames(data) <- sapply(seq(dim(data)[2]), function(x) paste('A',x[1], sep=""))
  response <- xxx[(6+n):length(xxx)]
  response<- sapply(response,  function(x) {
    return(as.numeric(unlist(strsplit(str_trim(x), split=" +"))))
  }
  )
  ans <- NULL
  ans$rows <- data
  ans$response <- response
  return(ans)
}
get_sign <- function(x){
  if(x>=0){
    return(1)
  }else{
    return(-1)
  }
}
read_libsvm_model <- function(filename, d){
  content <- readLines(filename)
  index <- which(content=="SV")
  model.info <- lapply(content[1:index], function(x){
    return(unlist(strsplit(str_trim(x), split=" +")))
  })
  names(model.info) <- unlist(lapply(model.info, function(x) {
    return(x[1])
    }))
  #sgn(w^Tx - rho)
  rho <- as.numeric(model.info$rho[2])
  SV <- content[(index+1):length(content)]
  xxx <- read_libsvm_default(SV, d)
  sv.coeffs <- as.matrix(xxx[,1])
  sv.vectors <- xxx[, c(2:ncol(xxx))]
  w <- t(sv.coeffs) %*% sv.vectors
  model <- NULL
  model$w <- w
  model$rho <-rho
  return(model)
}
read_libsvm_default <-function(content, dimensionality){
  num_lines = length( content )
  yx = matrix( 0, num_lines, dimensionality + 1 )
  
  # loop over lines
  for ( i in 1:num_lines ) {
    
    # split by spaces
    line = as.vector( strsplit( content[i], ' ' )[[1]])
    
    # save label
    yx[i,1] = as.numeric( line[[1]] )
    
    # loop over values
    for ( j in 2:length( line )) {
      
      # split by colon
      index_value = strsplit( line[j], ':' )[[1]]
      
      index = as.numeric( index_value[1] ) + 1    # +1 because label goes first
      value = as.numeric( index_value[2] )
      
      yx[i, index] = value
    }
  }
  
  return( yx )  
}
read.libsvm <- function( filename, dimensionality ) {
  
  content <- readLines( filename )
  yx <- read_libsvm_default(content, dimensionality)
  return(yx)

}

write.libsvm = function( filename, x, y ) {
  
  # open an output file
  f = file( filename, 'w' )
  
  # loop over examples
  for ( i in 1:nrow( x )) {
    
    # find indexes of nonzero values
    indexes = which( as.logical( x[i,] ))
    
    # those nonzero values
    values = x[i, indexes]
    
    # concatenate to the target format ( "1:6 3:77 6:8" )  	
    iv_pairs = paste( indexes, values, sep = ":", collapse = " " )
    
    # add label in the front and newline at the end
    output_line = paste( y[i], " ", iv_pairs, "\n", sep = "" )
    
    # write to file
    cat( output_line, file = f )
    
    # print progress
    if ( i %% 1000 == 0 ) {
      print( i )
    }
  }
  
  # close the connection
  close( f )
}