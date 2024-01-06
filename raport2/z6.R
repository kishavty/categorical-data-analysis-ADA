library(DescTools)
library(ca)
library(FactoMineR)
library(expm)
library(ggplot2)


dane <- matrix(c(32, 44, 60, 70,
                 22, 38, 104, 125,
                 13, 48, 61, 113,
                 3, 18, 54, 96), nrow = 4, byrow = TRUE)

rownames(dane) <- c("poniżej 6000", "6000-15000","15000-25000","powyżej 25000")
colnames(dane) <- c("b.niezadow.","niezadow.","zadow.","b. zadow.")


analiza_korespondencji <- function(dane, rysuj = TRUE){
  P <- dane / sum(dane)
  suma_r <- rowSums(P)
  suma_k <- colSums(P)
  D_r <- diag(suma_r)
  D_k <- diag(suma_k)
  A <- solve(sqrt(D_r)) %*% (P - suma_r %*% t(suma_k)) %*% solve(sqrt(D_k))
  U <- svd(A)$u
  V <- svd(A)$v
  
  F <- U %*% diag(sqrt(svd(A)$d))
  G <- V %*% diag(sqrt(svd(A)$d))
  A
  F
  G
  if (rysuj){
    x_F <- F[, 1]
    y_F <- F[, 2]
    x_G <- G[, 1]
    y_G <- G[, 2]
    plot(x_F, y_F, 
         xlim = c(min(c(x_F, x_G)) - 0.1, max(c(x_F, x_G)) + 0.1),
         ylim = c(min(c(y_F, y_G)) - 0.1, max(c(y_F, y_G)) + 0.1), 
         col = "blue",
         xlab = "Dimension 1", ylab = "Dimension 2",
         main = "Analiza Korespondencji metoda wlasna")
    
    
    points(x_G, y_G, col = "red", pch = 17)
    abline(v = 0, h = 0, col = c("black", "black"))
    text(F[, 1], F[, 2], labels = rownames(dane))
    text(G[, 1], G[, 2], labels = colnames(dane))
  }
  
  return(list(A, F, G))
}

analiza_korespondencji(dane, rysuj = TRUE)

#wbudowana 
analiza_korespondencji_wbud <- ca(dane)
summary(analiza_korespondencji_wbud)

#Wykresy
plot(analiza_korespondencji_wbud, col.row = "red", col.col = "blue",
     main = "Analiza Korespondencji metoda wbudowana")

#Inercja
inercja <- function(table){
  P <- table/sum(table) 
  r <- apply(P, 1, sum)
  c <- apply(P, 2, sum)
  
  
  D_r <- diag(r)
  D_c <- diag(c)
  A <- solve(sqrt(D_r)) %*% (P - r %*% t(c)) %*% solve(sqrt(D_c))
  
  
  Gamma <- svd(A)$d
  lambda <- sum(Gamma^2)
  return(lambda)
}
cat("Funkcji własna:", inercja(dane))
cat("Funkcji wbudowana:", sum(analiza_korespondencji_wbud$colinertia))

#miara wspolzmiennosci
cat("Miara wspolzmiennosci:", GoodmanKruskalGamma(dane, direction = "column"))