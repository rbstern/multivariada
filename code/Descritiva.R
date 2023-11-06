library(aplpack)
library(car)
library(HSAUR3)
library(fmsb)
library(reshape2)
#library(ggradar)
library(tidyverse)
library(heatmaply)

# Dados
iris
USairpollution

# Medidas resumo univariadas
summary(iris)

aux = iris %>% 
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#aux = iris %>% 
#  select(-Species)


# Matriz de covariância
aux %>% 
  cov()

# Matriz de correlação
aux %>% 
  cor()

# mapa de calor da correlação
heatmaply_cor(x = cor(aux), xlab = "Covariáveis",
              ylab = "Covariáveis", k_col = 2, k_row = 2)

# Matriz de gráficos de dispersão
scatterplotMatrix(aux)

# Mais de 2 variáveis em gráfico de dispersão
USairpollution %>% 
  ggplot(aes(x = temp, y = wind, color = SO2)) +
  geom_point()

# Boxplot multivariado
iris %>% 
  ggplot(aes(y = Petal.Length, x = Species)) +
  geom_boxplot()

# Gráfico de estrelas
stars(USairpollution)

# Faces de Chernoff
faces(aux[sample(1:150, 50), ])
