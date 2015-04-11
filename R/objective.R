source("./R/tool.R")
epsilon <- 0.5
task.attr <- "education"
task.attr <- "gender"
task.attr <- "marital"
out.file <- paste("./output/adult_", task.attr, "_norm_", epsilon, ".txt", sep = "")
train.filename <- paste("./output/adult_train_", task.attr, "_norm.txt", sep="")
ans <- readLines(train.filename, n=3)
d <- as.integer(unlist(strsplit(str_trim(ans[3]), split=" +"))[2])
# write_objective_data(out.file, data.train[, c(2:ncol(data.train))], data.train[, 1], epsilon)
# after processing
result.file <- paste("./output/ans_adult_", task.attr, "_norm_", epsilon, ".txt", sep = "")
result <- read_objective_result(result.file)
test.filename <- paste("./output/adult_test_", task.attr, "_norm.libsvm", sep="")
data.test <- read.libsvm(test.filename, d)
ans <- compute_svm(result$w, data.test[, c(2:ncol(data.test))], 0)
miss.rate <- compute_miss_rate(ans, data.test[,1])
ans.obj <- compute_svm(result$w_obj, data.test[, c(2:ncol(data.test))], 0)
miss.rate.obj <- compute_miss_rate(ans.obj, data.test[,1])
ans.output <- compute_svm(result$w_output, data.test[, c(2:ncol(data.test))], 0)
miss.rate.output <- compute_miss_rate(ans.output, data.test[,1])
