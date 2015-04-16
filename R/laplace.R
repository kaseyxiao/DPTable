source("./R/tool.R")
library(distr)
library(stringr)
# task.attr <- "gender"
# task.attr <- "marital"
task.attr <- "education"
# task.attr <- "salary"
tasks <- init_task()
data.id <- 2
data.name <- paste("Train", data.id, sep="")
predict.attr <- tasks[[data.name]][[task.attr]][['attr.name']]
train.filename <- paste("./output/adult_train_", task.attr, "_norm.txt", sep="")
ans <- readLines(train.filename, n=3)
d <- as.integer(unlist(strsplit(str_trim(ans[3]), split=" +"))[2])
train.filename <- paste("adult_train_", task.attr, "_norm.libsvm", sep="")
test.filename <- paste("adult_test_", task.attr, "_norm.libsvm", sep="")
train.file <- paste("./output/", train.filename, sep="")
test.file <- paste("./output/", test.filename, sep="")
data.train <- read.libsvm(train.file, d)
data.test <- read.libsvm(test.file, d)

# d: d-dimensional feature map
# kappa: max L2 norm
# L: L-Lipschitz
# C: ‘C’-constant of the regularization term
# n: data points
L <- 1
# C <- 1
C <- sqrt(n)
kappa <- 1
epsilons <- c(0.2, 0.4, 0.5, 0.8, 1.0)
# epsilon <- 0.5
n <- nrow(data.train)

# noises <- r(Lap)(d)
nrun <- 10
model.filename <- paste("./output/adult_", task.attr, "_norm.model", sep="")
model <- read_libsvm_model(model.filename, d)
max(model$w)
min(model$w)

ans <- compute_svm(model$w, data.test[, c(2:(d+1))], model$rho)
miss.rate <- compute_miss_rate(ans, data.test[,1])
miss.rate.laplace <- sapply(epsilons, function(x){
  epsilon <- x
  print(paste("epsilon", epsilon))
  b <- 4*L*C*kappa*sqrt(d)/(epsilon*n)
  Lap <- DExp(rate = 1 / b)
  miss.rates.noisy <- sapply(seq(nrun), function(x){
    print(paste("no. of run", x))
    curr.noises <- r(Lap)(d) 
    ans.noisy <- compute_svm(model$w+curr.noises, data.test[, c(2:(d+1))], model$rho)
    miss.rate.noise <- compute_miss_rate(ans.noisy, data.test[,1])
    return(miss.rate.noise)
  })
  return(mean(miss.rates.noisy))  
})
out.file <- paste("./output/laplace_adult_", task.attr, "_norm.txt", sep="")
cat("#first line: epsilon; second line: miss classification rates\n", file=out.file)
line <- paste(paste(epsilons, collapse = "\t"), "\n", sep="")
cat(line, file=out.file, append=TRUE)
line <- paste(paste(miss.rate.laplace, collapse = "\t"), "\n", sep="")
cat(line, file=out.file, sep="\t", append=TRUE)

