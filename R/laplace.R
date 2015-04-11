source("./R/tool.R")
library(distr)
# task.attr <- "gender"
# task.attr <- "marital"
task.attr <- "education"
# task.attr <- "salary"
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
# out.file <- paste("./output/adult_train_", task.attr, "_norm.txt", sep="")
# write_transformed_data(out.file, task.attr, predict.attr
#                        , data.train[,2:ncol(data.train)]
#                        , data.train[, 1])
# d: d-dimensional feature map
# kappa: max L2 norm
# L: L-Lipschitz
# C: ‘C’-constant of the regularization term
# n: data points
L <- 1
C <- 1
kappa <- 1
epsilon <- 0.2
n <- nrow(data.train)
b <- 4*L*C*kappa*sqrt(d)/(epsilon*n)
Lap <- DExp(rate = 1 / b)
noises <- r(Lap)(d)

model.filename <- paste("./output/adult_", task.attr, "_norm.model", sep="")
model <- read_libsvm_model(model.filename, d)
max(model$w)
min(model$w)
ans <- compute_svm(model$w, data.test[, c(2:(d+1))], model$rho)
miss.rate <- compute_miss_rate(ans, data.test[,1])
ans.noisy <- compute_svm(model$w+noises, data.test[, c(2:(d+1))], model$rho)
miss.rate.noise <- compute_miss_rate(ans.noisy, data.test[,1])
