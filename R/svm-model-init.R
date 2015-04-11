svm_model_init <- function(){
  tasks <- list()

  tasks$Train3[['trade']] <- list()
  tasks$Train3[['trade']][['attr.name']]<-'A2'
  tasks$Train3[['trade']][['predict.value']]<-c(0) #predict whether is TLB
  tasks$Train3[['security']] <- list()
  tasks$Train3[['security']][['attr.name']] <- 'A11'
  tasks$Train3[['security']][['predict.value']] <- c(0)  #predict whether is COMMON
  tasks$Train3[['exchange']] <- list()
  tasks$Train3[['exchange']][['attr.name']] <- 'A12'
  tasks$Train3[['exchange']][['predict.value']] <- c(0) #predict whether is AMEX
  
  #Train2: A10: gender; A4: education; A15: salary; A6: marital
  tasks$Train2[['gender']] <- list()
  tasks$Train2[['gender']][['attr.name']]<-'A10'
  tasks$Train2[['gender']][['predict.value']]<-c(1) #predict whether is male
  tasks$Train2[['education']] <- list()
  tasks$Train2[['education']][['attr.name']] <- 'A4'
  tasks$Train2[['education']][['predict.value']] <- c(9, 10, 11, 12, 13, 14, 15)  #predict whether holds a post-secondary degree
  tasks$Train2[['salary']] <- list()
  tasks$Train2[['salary']][['attr.name']] <- 'A15'
  tasks$Train2[['salary']][['predict.value']] <- c(1) #predict whether makes over 50K per year
  tasks$Train2[['marital']] <- list()
  tasks$Train2[['marital']][['attr.name']] <- 'A6'
  tasks$Train2[['marital']][['predict.value']] <- c(4) #predict whether never married

  #   Train4 (NLTCS)--  
  #   1. Y12: if disabled on getting about outside
  #   2. Y14: if disabled on managing money
  #   3. Y6: if disabled on getting to the bathroom or using toilet
  #   4. Y4: if disabled on dressing
  #   5. Y13: if disabled on travelling
  #   6. Y8: if disabled on doing light housework
  tasks$Train4[['outside']]<-list()
  tasks$Train4[['outside']][['attr.name']] <- 'A16'
  tasks$Train4[['outside']][['predict.value']] <- c(1)
  tasks$Train4[['money']] <- list()
  tasks$Train4[['money']][['attr.name']] <-  'A3'
  tasks$Train4[['money']][['predict.value']] <- c(1)
  tasks$Train4[['bathing']] <- list()
  tasks$Train4[['bathing']][['attr.name']] <- 'A11'
  tasks$Train4[['bathing']][['predict.value']] <- c(1)
  tasks$Train4[['traveling']] <-  list()
  tasks$Train4[['traveling']][['attr.name']] <- 'A4'
  tasks$Train4[['traveling']][['predict.value']] <- c(1)
  
  return(tasks) 
}
