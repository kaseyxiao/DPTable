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
source("./R/tool.R")

tasks <- init_task()
data.id = 2
domain <- read_domain(data.id)
task.attr <- "salary"
# task.attr <- "marital"
# task.attr <- "education"
# task.attr <- "gender"
data.name <- paste("Train", data.id, sep="")
predict.attr <- tasks[[data.name]][[task.attr]][['attr.name']]

data.train <- read_data("Train", data.id, domain)
data.test <- read_data("Test", data.id, domain)
data.scale <- transform_data(predict.attr
                             , data.train, data.test
                             , pos.values=tasks[[data.name]][[task.attr]][['predict.value']]
                             , normalize = TRUE, add.bias = TRUE)
epsilon <- 0.5
tag.bias <- "_bias"
out.file <- paste("./output/adult_", task.attr, tag.bias, "_norm_", epsilon, ".txt", sep="")
write_objective_data(out.file, data.scale$train$rows, data.scale$train$response, epsilon)
train.out.file <- paste("./output/adult_train_", task.attr, tag.bias, "_norm.libsvm", sep="")
write.libsvm(train.out.file, data.scale$train$rows, data.scale$train$response)
out.file <- paste("./output/adult_train_", task.attr, tag.bias, "_norm.txt", sep="")
write_transformed_data(out.file, task.attr, predict.attr
                       , data.scale$train$rows
                       , data.scale$train$response)
test.out.file <- paste("./output/adult_test_", task.attr, tag.bias, "_norm.libsvm", sep="")
write.libsvm(test.out.file, data.scale$test$rows, data.scale$test$response)
out.file <- paste("./output/adult_test_", task.attr, tag.bias, "_norm.txt", sep="")
write_transformed_data(out.file, task.attr, predict.attr, data.scale$test$rows
                       , data.scale$test$response)
