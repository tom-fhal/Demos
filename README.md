#INTRODUCTION

L’objectif de ce projet est d’explorer et d’analyser la relation entre les caractéristiques socio-économiques des départements français et leur taux d'absentéisme à travers des méthodes d’analyse statistiques et de machine learning. Le taux d'absentéisme, souvent considéré comme un indicateur de diverses tensions sociales ou économiques, peut être influencé par des facteurs tels que le revenu moyen, le taux de chômage, le niveau de diplôme, et la composition socioprofessionnelle.


Quelles sont les variables socio-économiques qui influencent le plus le taux d'absentéisme dans les départements français, et comment peut-on regrouper ces départements selon des profils socio-économiques similaires pour mieux comprendre les dynamiques locales ?

# Analyse Univariée
```{r}

library(tidyverse)

data = read.csv("data_abs.csv")

#Statistique descriptives
summary(data)

# Histogrammes pour certaines variables
hist(data$Salairemoy, main="Distribution du Salaire Moyen", xlab="Salaire moyen", col="lightblue")
hist(data$txabs, main="Distribution du Taux d'Absentéisme", xlab="Taux d'absentéisme", col="lightgreen")

# Boxplot pour la variable taux de chomage
boxplot(data$txcho, main="Boxplot du Taux de Chômage", ylab="Taux de Chômage")

```
# Interprétation 
La moyenne du salaire est 12,5. Cela indique que, dans l'ensemble, les personnes dans les différents départements gagnent en moyenne 12.5 unités monétaires.


les extrémitées  montrent des valeurs extrêmes, cela révèle que certains départements sont très affectés par le chômage comparé à d'autres.
#Analyse bivariée

```{r}

# Corrélation entre deux variables
cor(data$txcho, data$txabs)

# Graphique de dispersion relation entre txcho et txabs
plot(data$txcho, data$txabs, main="Taux de chômage vs Taux d'Abstention", xlab="Taux de chomage", ylab="Tauxd'abstention", col="blue", pch=16)

# régression
abline(lm(txabs ~ txcho, data=data), col="red")

# Corrélation matricielle variable numérique
cor_matrix = cor(data[,sapply(data, is.numeric)])
print(cor_matrix)

# Visualisation de la matrice de corrélation (optionnelle)
library(corrplot)
corrplot(cor_matrix, method="circle")

```
#interprétation 

la corrélation entre le salaire moyen et le taux d'absentéisme est négative et faible, cela ne nous permet pas de déterminer si réellement si le salaire moyen moins élevé permet de diminuer le taux d'absenteisme 

#Réduction de dimension avec ACP

```{r}
library(FactoMineR)
library(factoextra)

#  retrait des colonnes non numériques
data_numerique =data[, c("HLM", "Salairemoy", "Ouvrier", "Employe", "PI", "Cadres", "Artisant", "Agri", "TxPauv", "NonDiplome", "txcho", "txabs")]

#ACP sur les variables numériques
res.pca <- PCA(data_numerique, scale.unit = TRUE, ncp = 5, graph = TRUE)

#contribution des variables aux composantes principales
fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

# Obtenir les contributions des variables à chaque composante principale
print(res.pca$var$contrib)



```

#interprétation
A la vu des contributions, on peut voir que les variables les plus significative en dim 1 et deux sont le tx chomage et le tx de pauvreté.
Pour faire simple la dimension 1 a été choisi car elle explique une part significative de la variance (42.23%)

#une régression multiple pour modéliser la relation entre le taux d'abstention et les autres variables socio-économiques.

```{r} 

library(car)

#régression multiple
modele <- lm(txabs ~ Salairemoy + HLM + Ouvrier + Employe + PI + Cadres + Artisant + Agri + TxPauv + NonDiplome + txcho, data = data)

# Résumé du modèle pour voir les coefficients et les statistiques
summary(modele)

# teste de la colinéarité avec le facteur d'inflation de la variance
vif(modele)


```
#interpretation

reg multiple-summary: p-value significative pour les variables tx de pauvreté et tx de chomage.La P value du nondiplome se raproche aussi de la valeur significative.
Ce qui signifie que plus les tx de pauvreté et de chomage sont élevés, plus il y a augmentation du tx d'absentéisme.


VIF (entre 5 et 10): On peut remarqué une colinearité avec le tx de pauvreté, et la part des non ou peu diplômés.

#Cluster

```{r}
library(factoextra)
library(cluster)
# exclure les variables qualitatives
data_numerique = data[, c("HLM", "Salairemoy", "Ouvrier", "Employe", "PI", "Cadres", "Artisant", "Agri", "TxPauv", "NonDiplome", "txcho", "txabs")]

# Normaliser les données pour le clustering
data_norm = scale(data_numerique)
# Calculer l'inertie intra-cluster pour différents nombres de clusters
fviz_nbclust(data_norm, kmeans, method = "wss") # Méthode du coude


# Calculer l'indice de silhouette pour différents nombres de clusters
fviz_nbclust(data_norm, kmeans, method = "silhouette")
# Appliquer K-Means avec le nombre optimal de clusters (par exemple, k=3)
set.seed(123)  # Pour garantir la reproductibilité
kmeans_result=kmeans(data_norm, centers = 3, nstart = 25)

# Visualiser les clusters
fviz_cluster(kmeans_result, data = data_norm)


# Calcul du dendrogramme avec le clustering hiérarchique
hc = hclust(dist(data_norm), method = "ward.D2")

# Visualisation du dendrogramme
plot(hc, labels = FALSE)

# Couper l'arbre pour obtenir 3 clusters (par exemple)
rect.hclust(hc, k = 3, border = "red")
clusters_hc <- cutree(hc, k = 3)

# Calcul et affichage de l'indice de silhouette
silhouette_score <- silhouette(kmeans_result$cluster, dist(data_norm))
fviz_silhouette(silhouette_score)

# Ajouter le numéro de cluster à tes données
data$cluster = kmeans_result$cluster


```
#Intrprétatio
Les clusters obtenus ont été interprétés selon les variables socio-économiques dominantes. Chaque cluster représente un profil distinct de départements : certains sont marqués par un haut taux de pauvreté et de chômage, tandis que d'autres présentent un haut salaire moyen et un faible taux d'absentéisme.

#CONCLUSION

L’analyse a révélé que certaines variables, comme le taux de chômage, le salaire moyen et le niveau d’éducation (NonDiplôme), ont un effet significatif sur le taux d’absentéisme. Les départements présentant des conditions économiques plus précaires tendent à avoir des taux d'absentéisme plus élevés.