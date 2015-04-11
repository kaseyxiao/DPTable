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
source("R/data.R")
source("R/svm-model-init.R")
main <- function(){
  data.name <- "Train2"
  if(data.name == "Train2"){
    test.name = "Test2"
    selected.attrs <- c("gender", "education", "salary", "marital")
  }else{
    stop("Not valid dataset !")
  }

  tasks <- svm_model_init()
  all.predict.attrs <- tasks[[data.name]]
  predict.attrs <- lapply(selected.attrs, function(x)all.predict.attrs[[x]])
  names(predict.attrs) <- selected.attrs
  tasks <- svm_model_init()
  all.predict.attrs <- tasks[[data.name]]
  predict.attrs <- lapply(selected.attrs, function(x)all.predict.attrs[[x]])
  names(predict.attrs) <- selected.attrs
 
  cat("load data: ", data.name, "\n")
  data.train <- Data$new(data.name)$origin
  cat("load data: ", test.name, "\n")
  data.test <- Data$new(test.name)$origin  
  miss.rates <- lapply(seq_along(predict.attrs), function(ii) {
    predict.attr <- predict.attrs[[ii]][['attr.name']]
    f <- as.formula(paste(predict.attr, "~.", sep = ""))
    train <- as.data.frame(sapply(data.train, function(x){as.numeric(as.character(x))}))
    train$A10[train$A10==0] <- -1
    train[, "A10"] <- as.factor(train[, "A10"])
    test <- as.data.frame(sapply(data.test, function(x){as.numeric(as.character(x))}))
    test$A10[test$A10==0] <- -1
    test[, "A10"] <- as.factor(test[, "A10"])
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")s
    attr(test, "intercept") <- 0
    x <- model.matrix(Terms, m)
    xxx <- model.matrix(f, data.train)
    model <- svm(f, data.train, kernel = "linear")
    res <- predict(model, newdata = data.test)
    length(which(res==data.test$A15))/nrow(data.test)
    predict.values <- predict.attrs[[ii]][['predict.value']]
    sim.results <- unlist(lapply(res, function(x) {x %in% predict.values}))
     response <-   sapply(data.train[,"A10"], function(x) {
      if (x == 0){
        return(-1)
      }else{
        return(1)
      }
    })
    write.table( result
                , out.file
                , row.names = FALSE
                , col.names = FALSE
                , quote=FALSE
                , sep = "\n"
                , append=TRUE)
    #n, d, lambda, epsilon, h
    # Obtain feature weights
    ans1 <- read.table("./output/ans1.txt")
    w <- ans1[1,c(1:ncol(data.train)-1)]
    b <- ans1[1,c(ncol(ans1))]
    w_dp <- ans1[3,c(1:ncol(data.train)-1)]
    b_dp <- ans1[3,c(ncol(ans1))]
    # Calculate decision values manually
    w = t(model$coefs) %*% model$SV
    test <- sapply(data.test[colnames(data.test) !="A10"], as.numeric)
    as.numeric(levels(f))[f]
    test.scaled = scale(data.test[colnames(data.train) !="A10"], model$x.scale[[1]], model$x.scale[[2]]) 
    test.scaled = scale(data.test[colnames(data.train) !="A10"], center=TRUE, scale=TRUE)
    test = model.matrix(f, data.test[colnames(data.test)])
    xxx <- t(as.matrix(test))
    ans <- t(w %*% t(test) - model$rho)
    ans <- sapply(ans, function(x){
      if(x>0){
        return(1)
      }else{
        return(0)
      }
    })
    ans <- sapply(ans, as.factor)
    T
    # Should equal...
    fit$decision.values
    true.results <- unlist(lapply(data.test[[predict.attr]], function(x) 
    {x %in% predict.values}))
    miss.rate <- length(which(sim.results != true.results))/length(sim.results)
    return(miss.rate)
  })
  names(miss.rates) <- names(predict.attrs)
  sapply(a, function(x) {
    if (x == 0){
      return(-1)
    }else{
      return(1)
    }
  })
  
}

# epsilon <- 0.5
# write_out_data(train.trans, predict.attr, epsilon)
# ans <- read.table(paste("./output/ans_", predict.attr, "_", epsilon, ".txt", sep=""))
# w <- ans[1,c(1:ncol(train.trans)-1)]
# b <- ans[1,c(ncol(ans))]
# w_dp <- ans[3,c(1:ncol(train.trans)-1)]
# b_dp <- ans[3,c(ncol(ans))]
# index <- nrow(train.trans) + 1
# test.scale <- total.scale[, colnames(total.scale) != 'response'][index:nrow(total.scale),]
# train.scale <- total.scale[, colnames(total.scale) != 'response'][1:index-1,]
# ans <- t(as.matrix(w) %*% t(as.matrix(train.scale)) - b)
# ans <- sapply(ans, function(x){
#   if(x>0){
#     return(1)
#   }else{
#     return(-1)
#   }
# })
# total <- rbind(train.trans, test.trans)
# preProcValues <- preProcess(total[, colnames(total) != 'response'], method=c("range"))
# total.scale <- predict(preProcValues, total[, colnames(total) != 'response'])
# response <- total[, "response"]
# train.size <- nrow(train.trans)
# test.size <- nrow(test.trans)
# # total.scale <- cbind(total.scale, response)
# # write_out_data(total.scale[1:nrow(train.trans), ], predict.attr, epsilon)
# filename <- "./output/adult_libsvm.txt"
# x <- total.scale[1:train.size, ]
# y <- response[1:train.size]
# x.test <- total.scale[(train.size+1):nrow(total.scale), ]
# y.test <- response[(train.size+1):nrow(total.scale)]
# write.libsvm("./output/adult_libsvm_test.txt", y.test, x.test)
# l2norm<-apply(total.scale, 1, function(x) sqrt(sum(x^2)))
# total.scale.norm <- total.scale/l2norm
# x.train.norm <- total.scale.norm[1:train.size, ]
# x.test.norm <- total.scale.norm[(train.size+1):nrow(total.scale), ]
# write.libsvm("./output/adult_libsvm_train_norm.txt", y, x.train.norm)
# write.libsvm("./output/adult_libsvm_test_norm.txt", y.test, x.test.norm)
# 
# # ff <- as.formula(paste("response", "~.", sep = ""))
# # model <- svm(ff, data=total.scale[1:nrow(train.trans), ], kernel="linear")
# 
# preProcValues <- preProcess(xxx, method=c("range"))
# train.scale <- predict(preProcValues, xxx)
# #get max value of each column
# # apply(train.scale, 2, max)
# xxx<-readLines("./output/bloodsvm.txt")
# n <- as.integer(xxx[1])
# data<-xxx[6:(5+n)]
# library(stringr)
# data<- lapply(data,  function(x) {
#   return(as.numeric(unlist(strsplit(str_trim(x), split=" +"))))
# }
# )
# data<-do.call(rbind.data.frame, data)
# colnames(data) <- sapply(seq(dim(data)[2]), function(x) paste('A',x[1], sep=""))
# response <- xxx[(6+n):length(xxx)]
# write.libsvm("./output/bloodsvm_libsvm.txt", response, data)
# 
# f <-   as.formula(paste("A15", "~.", sep = ""))
# model.adult <- svm(f, data=data.train, kernel="linear", type="C-classification")
# model.blood <- svm(f, data=cbind(data, response), kernel="linear")
# ans <- read.table(paste("./output/ans_bloodsvm.txt", sep=""))
# w <- ans[1,c(1:(ncol(ans)-1))]
# b <- ans[1,c(ncol(ans))]
# ans <- t(as.matrix(w) %*% t(as.matrix(data)) - 0)
# ans <- sapply(ans, function(x){
#   if(x>0){
#     return(1)
#   }else{
#     return(-1)
#   }
# })
# filename <-"./output/adult.model"
# content <- readLines(filename)
# index <- which(content=="SV")
# SV <- content[(index+1):length(content)]
# xxx <- read.libsvm(SV, 104)
# sv.coeffs <- as.matrix(xxx[,1])
# sv.vectors <- xxx[, c(2:ncol(xxx))]
# w <- t(sv.coeffs) %*% sv.vectors
# test <- read.libsvm(readLines("./output/adult_libsvm_test.txt"), 104)
# decision <- t(w %*% t(test[, c(2:ncol(test))]) - 3.95737)
# decision[which(decision>=0)]=1
# decision[which(decision<0)]=-1
# length(which(decision!=test[,1]))/length(decision)
# ans<-read.table("./output/adult.predict")
