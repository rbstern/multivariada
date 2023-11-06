library(groupdata2)
library(mvtnorm)

###
# Exemplo de seleção de variáveis com a normal multivariada
###

n = 10^3
d = 6*10^2
mu = c(rep(1, 10), rep(0, d-10))
X = rmvnorm(n, mean= mu, sigma=diag(5, length(mu)))

# Estimativas da média populacional via penalização ridge
lambda = seq(from = 0, to = 1000, by = 0.1)
estimativas <- function(X)
{
  barX = colMeans(X) %>% matrix(nrow = 1)
  red = matrix(n/(n + lambda), ncol = 1)
  red %*% barX
}

###
# Ilustração do Risco
###

# Em geral não sabemos calcular essa função 
# pois mu é desconhecido.
risco <- function(estimativa)
{
  sum((estimativa - mu)^2)
}

risco_est = X %>% 
  estimativas() %>% 
  apply(1, risco)

aux = tibble(lambda, risco_est)
aux %>% 
  ggplot(aes(x = lambda, y = risco_est)) +
  geom_line()

###
# Estimação via Data splitting
###
p = 0.7
n1 = round(n*p)
idx_treino = sort(sample(1:n, replace = FALSE, n1))
idx_teste = (1:n)[-idx_treino]

X_treino = X[idx_treino, ]
X_teste = X[idx_teste, ]

###
# Forma alternativa de Data splitting
# via pacote groupadata2

# aux = data.frame(X) %>% 
#   partition(p = 0.8)
#
# X_treino = as.matrix(aux[[1]])
# X_teste  = as.matrix(aux[[2]])
###

est = X_treino %>% estimativas()

risco = rep(NA, length(lambda))
for(ii in 1:nrow(est))
{
  print(ii)
  aux = 0
  for(jj in 1:nrow(X_teste))
  {
    aux = aux + sum((X_teste[jj,] - est[ii, ])^2)
  }
  risco[ii] = aux
}

aux = tibble(lambda, risco)
aux %>% 
  ggplot(aes(x = lambda, y = risco)) +
  geom_line()

lambda_otim = lambda[which.min(risco)]
lambda_otim

###
# Estimação via Validação Cruzada k-fold
###

###
# Exemplo usando o pacote groupdata2
aux = X %>% 
  data.frame() %>% 
  fold(k = 4)
###

grupos = aux$.folds %>% 
  unique() %>% 
  sort()
 
erros_por_grupo = matrix(NA, nrow = length(lambda), ncol = length(grupos))
for(grupo in grupos)
{
  print(grupo)
  
  X_treino = aux[aux$.folds != grupo, ] %>% 
    ungroup() %>% 
    select(-.folds) %>% 
    as.matrix()
  
  
  X_teste = aux[aux$.folds == grupo, ] %>% 
    ungroup() %>% 
    select(-.folds) %>% 
    as.matrix()
  
  # Treinamento
  est = X_treino %>%
    estimativas()
  

  # Estimar o erro de predição
  for(ii in 1:length(lambda))
  {
    mu_lambda = est[ii, ]
    erros = rep(NA, nrow(X_teste))
    for(jj in 1:nrow(X_teste))
    {
      erros[jj] = sum((X_teste[jj, ] - mu_lambda)^2)
    }
    erros_por_grupo[ii, grupo] = mean(erros)
  }
}


erro_lambda = rowMeans(erros_por_grupo)
tibble(lambda, erro_lambda) %>% 
  ggplot(aes(x = lambda, y = erro_lambda)) +
  geom_line()

