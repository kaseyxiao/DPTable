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

main()
