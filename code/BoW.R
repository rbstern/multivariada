library(glmnet)
library(Matrix)
library(tidyverse)
library(tm)

dados <- read_csv("./data/b2w.csv")

corpus <- VCorpus(VectorSource(dados$review_text_processed)) 
dtm <- DocumentTermMatrix(
  corpus, 
  control = list(
    tolower = TRUE, 
    stemming = FALSE, 
    removeNumbers = TRUE, 
    removePunctuation = TRUE, 
    removeStripwhitespace = TRUE, 
    weighting =weightTf
  )
) 
XX <- Matrix::sparseMatrix(
  i=dtm$i, 
  j=dtm$j, 
  x=dtm$v, 
  dimnames=list(
    NULL,
    dtm$dimnames[[2]]), 
  dims=c(dtm$nrow, dtm$ncol)
)
n = nrow(XX)
tr = sort(sample(1:n, round(0.7*n), replace = FALSE))

###
# mq
###
ajuste_mq <- glmnet(XX[tr,], dados$rating[tr], alpha=0, lambda=0) 
predito_mq <- predict(ajuste_mq, newx = XX[-tr,]) 



###
# ridge 
###
ajuste_ridge <- cv.glmnet(XX[tr,], dados$rating[tr], alpha=0)
predito_ridge <- predict(ajuste_ridge, s = ajuste_ridge$lambda.min, newx = XX[-tr,]) 

#MAE
sum(abs(predito_mq - dados$rating[-tr]))
sum(abs(predito_ridge - dados$rating[-tr]))
#RMSE
sqrt(sum((predito_mq - dados$rating[-tr])^2))
sqrt(sum((predito_ridge - dados$rating[-tr])^2))

###
# lasso 
###
ajuste_lasso <- cv.glmnet(XX[tr,], dados$rating[tr],alpha=1) 
predito_lasso <- predict(ajuste_lasso, s = ajuste_lasso$lambda.min, newx = XX[-tr,])

