curr.platform = Sys.info()["sysname"]
if(curr.platform == "Windows") {
  if (Sys.info()["user"] == 'XiaoQian'){
    setwd("C:/Users/XiaoQian/Dropbox/Git/DPHigh-dim/JunctionTree")
  }
  else{
    setwd("D:/Dropbox/Git/DPHigh-dim/JunctionTree")     
  }
}else if(curr.platform == "Darwin") {
  setwd("~/Desktop/DPTable")  
}else if(curr.platform == "Linux"){
  setwd("~/DPHigh-dim/JunctionTree")
}
library("e1071")
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

transform_data <- function(predict.attr, rows, pos.values) {
  f <- as.formula(paste(predict.attr, "~.", sep = ""))
  
  dummies <- dummyVars(f, data = rows)
  xxx <- predict(dummies, newdata = rows)
  response <-   sapply(rows[, predict.attr], function(x) {
    if (x %in% pos.values){
      return(1)
    }else{
      return(-1)
    }
  })
  ans <- cbind(xxx, response)
  return(ans)
}

write_out_data <- function(data.out, predict.attr, epsilon=0.5){
  h <- 0.5
  lambda <- 1.0
  out.file <- paste("./output/adult_", predict.attr, "_", epsilon, ".txt", sep = "")
  line <- paste(nrow(data.out), ncol(data.out) - 1, lambda, epsilon, h, sep="\n")
  cat(line, file=out.file, sep = "\n")
  write.table(data.out[,colnames(data.out) !="response"]
              , out.file
              , row.names = FALSE
              , col.names = FALSE
              , quote=FALSE
              , sep = "\t"
              , append=TRUE)
  write.table( data[, "response"]
               , out.file
               , row.names = FALSE
               , col.names = FALSE
               , quote=FALSE
               , sep = "\n"
               , append=TRUE)
}

write.libsvm = function( filename, y, x ) {
  
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

data.id = 2
domain <- read_domain(data.id)
predict.attr <- "A15"

data.train <- read_data("Train", data.id, domain)
data.test <- read_data("Test", data.id, domain)

train.trans <- transform_data(predict.attr, data.train, pos.values=c(">50K"))
test.trans <- transform_data(predict.attr, data.test, pos.values=c(">50K"))
epsilon <- 0.5
write_out_data(train.trans, predict.attr, epsilon)
ans <- read.table(paste("./output/ans_", predict.attr, "_", epsilon, ".txt", sep=""))
w <- ans[1,c(1:ncol(train.trans)-1)]
b <- ans[1,c(ncol(ans))]
w_dp <- ans[3,c(1:ncol(train.trans)-1)]
b_dp <- ans[3,c(ncol(ans))]
index <- nrow(train.trans) + 1
test.scale <- total.scale[, colnames(total.scale) != 'response'][index:nrow(total.scale),]
train.scale <- total.scale[, colnames(total.scale) != 'response'][1:index-1,]
ans <- t(as.matrix(w) %*% t(as.matrix(train.scale)) - b)
ans <- sapply(ans, function(x){
  if(x>0){
    return(1)
  }else{
    return(-1)
  }
})
total <- rbind(train.trans, test.trans)
preProcValues <- preProcess(total[, colnames(total) != 'response'], method=c("range"))
total.scale <- predict(preProcValues, total[, colnames(total) != 'response'])
response <- total[, "response"]
train.size <- nrow(train.trans)
test.size <- nrow(test.trans)
# total.scale <- cbind(total.scale, response)
# write_out_data(total.scale[1:nrow(train.trans), ], predict.attr, epsilon)
filename <- "./output/adult_libsvm.txt"
x <- total.scale[1:train.size, ]
y <- response[1:train.size]
x.test <- total.scale[(train.size+1):nrow(total.scale), ]
y.test <- response[(train.size+1):nrow(total.scale)]
write.libsvm("./output/adult_libsvm_test.txt", y.test, x.test)
l2norm<-apply(total.scale, 1, function(x) sqrt(sum(x^2)))
total.scale.norm <- total.scale/l2norm
x.train.norm <- total.scale.norm[1:train.size, ]
x.test.norm <- total.scale.norm[(train.size+1):nrow(total.scale), ]
write.libsvm("./output/adult_libsvm_train_norm.txt", y, x.train.norm)
write.libsvm("./output/adult_libsvm_test_norm.txt", y.test, x.test.norm)

# ff <- as.formula(paste("response", "~.", sep = ""))
# model <- svm(ff, data=total.scale[1:nrow(train.trans), ], kernel="linear")

preProcValues <- preProcess(xxx, method=c("range"))
train.scale <- predict(preProcValues, xxx)
#get max value of each column
# apply(train.scale, 2, max)
xxx<-readLines("./output/bloodsvm.txt")
n <- as.integer(xxx[1])
data<-xxx[6:(5+n)]
data<- lapply(data,  function(x) {
  return(as.numeric(unlist(strsplit(str_trim(x), split=" +"))))
}
)
data<-do.call(rbind.data.frame, data)
colnames(data) <- sapply(seq(dim(data)[2]), function(x) paste('A',x[1], sep=""))
response <- xxx[(6+n):length(xxx)]
ans <- read.table(paste("./output/ans_bloodsvm.txt", sep=""))
w <- ans[1,c(1:(ncol(ans)-1))]
b <- ans[1,c(ncol(ans))]
ans <- t(as.matrix(w) %*% t(as.matrix(data)) - 0)
ans <- sapply(ans, function(x){
  if(x>0){
    return(1)
  }else{
    return(-1)
  }
})
