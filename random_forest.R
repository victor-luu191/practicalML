myPreProcess <- function(train, valid) {
  train <- train[ ,-(1:7)]
  valid <- valid[ ,-(1:7)] 
  
  nzv.info <- nearZeroVar(train, saveMetrics = T)
  nz.vars <- which(nzv.info$nzv == T)
  train <- train[ , -nz.vars]
  valid <- valid[ , -nz.vars]
  
  propNA <- function(col, ds) {
    prop.na <- sum(is.na(ds[ ,col]))/nrow(ds)
    data.frame("predictor" = names(ds)[col], "prop.na" = prop.na)
  }
  na.summary <- ldply(1: ncol(train), propNA, ds = train)
  too.many.nas <- which(na.summary$prop.na > 0.90)
  train <- train[ ,-too.many.nas]
  valid <- valid[ ,-too.many.nas]
  ## return processed train and test sets
  list(train = train, valid = valid)
}

runRF <- function(i) {
  valid <- ds[partition[[i]], ]
  train <- ds[-partition[[i]], ] 
  pair <- myPreProcess(train = train, valid = valid)
  train <- pair$train
  valid <- pair$valid
  cat("Start running random forest \n")
  rf.fit <- randomForest(y = train$classe, x = train[ ,-ncol(train)], ntree = 100)
  rf.pred <- predict(rf.fit, valid[ ,-ncol(valid)])
  conf.mat <- confusionMatrix(rf.pred, reference = valid$classe)
  data.frame("fold" = i, "accuracy" = conf.mat$overall["Accuracy"])
}
runRF(2)
res <- ldply(1:10, runRF, .progress = "time")
res
