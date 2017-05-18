## Carregando os pacotes necessários
library(readr)

## Lendo o conjunto de dados
protein <- read.table('protein.txt', header = T, sep = '\t')

## Sumário básico
summary(protein)

## Mudando a escala
pmatrix <- scale(protein[,-1])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

## Criando o cluster
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward")
plot(pfit)

## Desenhando a separação dos clusters
rect.hclust(pfit, k=5)

## Obtendo os grupos para cada cluster
groups <- cutree(pfit, k=5)

## Função para imprimir os cluster
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

## Imprimindo os cluster
print_clusters(groups, 5)

## Visualizando os clusters
library(ggplot2)
princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, newdata = pmatrix)[,1:nComp]

## Criando uma tabela com os dados transformados
project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      country=protein$Country)

## Fazendo o gráfico
library(ggplot2)
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=country), hjust=0, vjust=1)
  
## Fazendo em 3D
## Fazendo o plot em 3D
## Projetando em 3 dimensões
project <- predict(princ, newdata = pmatrix)[,1:3]
library(rgl)
plot3d(project, col=project.plus$cluster, pch = 19)

## Hack para fazer em 3D algo similar ao que foi feito
plot3d(project, col=project.plus$cluster, pch = 19)
text3d(project,texts=project.plus$country)

###################### K-MEANS #####################################
## Gerando um conjunto de dados fictício
n = 100
g = 6 
set.seed(g)
d <- data.frame(x = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))), 
                y = unlist(lapply(1:g, function(i) rnorm(n/g, runif(1)*i^2))))
plot(d)

## Método do cotovelo
mydata <- d
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Aplicando o k-means e o método do cotovelo para os dados
## da proteína
mydata <- pmatrix
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
