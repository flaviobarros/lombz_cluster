## Carregando o conjunto de dados
shopping <- read.csv('shopping.csv')
shopping <- shopping[,-1]

## Análise descritiva
summary(shopping)

## Obtendo a média por variável
apply(shopping, 2, mean)

## Mudando a escala
pmatrix <- scale(shopping)
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

## Criando o cluster
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward")
plot(pfit)

## Desenhando a separação dos clusters
rect.hclust(pfit, k=3)

## K-means pelo método do cotovelo
## da proteína
mydata <- pmatrix
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
abline(v=3)

## Pegando os cluster do kmeans
clusters <- kmeans(pmatrix, centers = 3)

## Juntando a informação do cluster
shopping$grupos <- factor(clusters$cluster)

## Perfil das variáveis por grupo
shopping_segments <- split(shopping[,-9], shopping$grupos)
sapply(shopping_segments, function(x) apply(x, 2, mean))
