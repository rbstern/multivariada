library(patchwork)
library(tidyverse)

# Exemplo 1: inspirado em: k41m4n/eigenfaces
# PCA para redução de dimensionalidade em imagens
# Face images taken between April 1992 and April 1994 at AT&T Laboratories Cambridge
# Each row contains data of one image quantized to 256 grey levels between 0 and 1
data_faces <- "./data/olivetti.csv" %>%  
  read_csv(col_names=FALSE)

# Visualizar um rosto
showFace <- function(x){
  x %>%
    as.numeric() %>%
    matrix(nrow = 64, byrow = TRUE) %>%
    apply(2, rev) %>% # Inverter imagem no eixo y
    t() %>% # Girar imagem 90 graus
    image(col = grey(seq(0, 1, length=256)), xaxt="n", yaxt="n")
}

# Visualizar alguns rostos
par(mfrow=c(4, 10))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (ii in 1:40) {
  showFace(data_faces[ii, ])
}

# Compute and display the average face (mean by each column) #### 
avg_face <- colMeans(data_faces)
dev.off()
showFace(avg_face)

# Retirar face média
data_faces_scale <- scale(data_faces, center = TRUE, scale = FALSE)

# A) PCA pela definição em aula 
# data_cov <- t(data_faces_scale) %*% data_faces_scale / (nrow(data_faces_scale)-1)
# eig <- eigen(data_cov) # diagonalização da matriz de covariância
# eig_vec <- eig$vectors # autovetores
# eig_val <- eig$values  # autovalores

# B) Conduct svd (more numerically stable )####
data_faces_svd <- svd(data_faces_scale) # Decomposição de X em valores singulares
eig_vec <- data_faces_svd$v                                # autovetores à direita
eig_val <- (data_faces_svd$d)^2/(ncol(data_faces_scale)-1) # valores singulares

# Compute and display the proportions of variance explained by the principal components ####
var_prop <- eig_val/sum(eig_val)
var_cum_prop <- cumsum(eig_val)/sum(eig_val)

dev.off()
p1 = tibble(x = 1:length(var_prop), y = var_prop) %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_line() +
  ylab("Proporção da variância explicada") +
  xlab("Componente")
p2 = tibble(x = 1:length(var_cum_prop), y = var_cum_prop) %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_line() +
  ylab("Variância acumulada explicada") +
  xlab("Componente")
p1 + p2

n_comp <- min(which(var_cum_prop > 0.95)) # Numero de componentes principais para explicar 95% da Var
eig_vec_sub <-  eig_vec[, 1:n_comp]       # Reduzir a dimensão

# Visualização dos primeiros 16 autovetores
dev.off()
par(mfrow=c(5, 5))
par(mar=c(0.05, 0.05, 0.05, 0.05))
for (i in 1:25) {
  showFace(eig_vec_sub[, i])
}

# Projeção das imagens nos autovetores selecionados
dev.off()
data_proj <- (data_faces_scale) %*% eig_vec_sub
barplot(data_proj[1, ], ylim = c(-8, 4))

# Face projetada
dev.off()
par(mfrow=c(1, 2))
par(mar=c(0.05, 0.05, 0.05, 0.05))
showFace((data_faces[1, ]))
(data_proj[1, ] %*% t(eig_vec_sub) + avg_face) %>%
  showFace()
