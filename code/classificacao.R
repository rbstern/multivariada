library(rpart)
library(tidyverse)
# library(glmnet)

# Geração dos dados
n = 10^4
x1 <- rnorm(n)
x2 <- rnorm(n)
plot(x1, x2)
y <- x1*x2 > 0
data <- tibble(x1, x2, y)

# Visualização dos dados
data %>% 
  ggplot(aes(x = x1, y = x2, color = y)) +
  geom_point()

# Tentando usar regressão logística
aux = data %>% 
  glm(y~x1+x2, family = "binomial", data = .)

pred = predict(aux, data, type = "response") > 0.5
mean(pred != y)

# Tentando usar árvores de classificação
aux = data %>% 
  rpart(y~x1+x2, data = .)
plot(aux)

pred = predict(aux, data) > 0.5
mean(pred != y)

install.packages("randomForest")
library(randomForest)
?randomForest
x = cbind(x1, x2)
randomForest(x, y=as.factor(y))
