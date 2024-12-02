# INTRODUCTION

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
#  Analyse univariée et bivariée des données

Nous avons d’abord effectué une analyse descriptive des différentes variables socio-économiques, telles que le taux de chômage, le salaire moyen, et le pourcentage de personnes sans diplôme. Cela a permis de comprendre les distributions et corrélations entre ces variables et le taux d’absentéisme.



# Réduction de dimension et sélection de variables


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

Pour optimiser la sélection des variables les plus pertinentes, nous avons utilisé des techniques de réduction de dimension telles que l'Analyse en Composantes Principales (ACP) et des méthodes basées sur des algorithmes comme le Random Forest. Ces techniques nous ont permis de réduire le nombre de variables tout en conservant celles qui expliquaient le mieux la variabilité du taux d'absentéisme.

# Une régression multiple pour modéliser la relation entre le taux d'abstention et les autres variables socio-économiques.

```{r} 

library(car)

#régression multiple
modele <- lm(txabs ~ Salairemoy + HLM + Ouvrier + Employe + PI + Cadres + Artisant + Agri + TxPauv + NonDiplome + txcho, data = data)

# Résumé du modèle pour voir les coefficients et les statistiques
summary(modele)

# teste de la colinéarité avec le facteur d'inflation de la variance
vif(modele)


```

Nous avons ensuite appliqué un modèle de régression linéaire multiple pour modéliser la relation entre le taux d'absentéisme et les autres variables. Ce modèle nous a permis d’identifier les facteurs significatifs qui influencent le taux d'absentéisme. L’évaluation du modèle a été réalisée à travers le coefficient de détermination (R²) et les valeurs p associées à chaque variable.


# Clustering et classification des départements

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


Nous avons appliqué des méthodes de clustering, comme l'algorithme de K-Means et le clustering hiérarchique, pour regrouper les départements en fonction de leurs caractéristiques socio-économiques et de leur taux d’absentéisme. Le nombre optimal de clusters a été déterminé à l'aide de l'indice de silhouette et de la méthode du coude

# CONCLUSION

L’analyse a révélé que certaines variables, comme le taux de chômage, le salaire moyen et le niveau d’éducation (NonDiplôme), ont un effet significatif sur le taux d’absentéisme. Les départements présentant des conditions économiques plus précaires tendent à avoir des taux d'absentéisme plus élevés.

Le clustering a permis de diviser les départements en groupes homogènes, chacun ayant des caractéristiques socio-économiques distinctes.

- Le Cluster 1 regroupe des départements à faible niveau de diplôme et taux de pauvreté élevé, corrélés à un absentéisme plus important.
- Le Cluster 2 rassemble des départements plus favorisés avec un salaire moyen plus élevé, où le taux d'absentéisme est plus faible.

