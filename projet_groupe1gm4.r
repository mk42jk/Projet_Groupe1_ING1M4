#Ouverture du fichier
projet = read.table("MentalHealth.csv", header = T, sep=",", dec=".")

#Nettoyage de la base de données

projet$User_ID <- NULL #Supression de la première colonne 

#Renommage simple des colonnes
colnames(projet) <- c(
  "Age",
  "Genre",
  "Temps_Ecran",
  "Qualite_Sommeil",
  "Stress",
  "Jours_Sans_Reseaux",
  "Sport",
  "Plateforme",
  "Bonheur"
)

# Vérification
names(projet)

#Passage de la variable age de quantitative en qualitative
projet$Age <- cut(
  projet$Age,
  breaks = c(16, 20, 30, 40, 50),
  labels = c("ados", "jeunes adultes", "adultes", "seniors"),
  include.lowest = TRUE
)

projet

numcols <- unlist(lapply(projet, is.numeric))
plot(projet[,numcols]) #graphique résumant les liens entre les différentes variables



### Analyse univariée ###

#Resumé des variables quantitatives
summary(projet$Temps_Ecran)
summary(projet$Qualite_Sommeil)
summary(projet$Stress)
summary(projet$Bonheur)
summary(projet$Sport)
summary(projet$Jours_Sans_Reseaux)

#Resumé des valeurs qualitatives

#variable age
eff_type_pers = table(projet$Age)
eff_type_pers #effectif total variable Age

#ajout pour calculer pourcentage age
pct_age <- prop.table(eff_type_pers) * 100
pct_age

# Camembert avec repartition variable Age
pie(pct_age,
    main = "Répartition par tranche d'âge",
    col = rainbow(length(pct_age)),
    labels = paste0(names(pct_age), "\n", round(pct_age, 1), "%"))

#variable genre
gender_counts <- table(projet$Genre)
gender_counts
pourcentage_g <- prop.table(gender_counts) * 100
pourcentage_g

#Camembert avec repartition variable Genre
pie(gender_counts, main="Répartition du genre",
    col= c("magenta","dodgerblue","grey"),
    labels = paste0(names(pourcentage_g), " (", round(pourcentage_g, 1), "%)"))

# variable reseaux sociaux
freq_reseau <- table(projet$Plateforme)
freq_reseau
pourcentage_reseau <- prop.table(freq_reseau) * 100
pourcentage_reseau

# Camembert avec pourcentages
pie(pourcentage_reseau,
    main = "Répartition des réseaux sociaux",
    col = rainbow(length(pourcentage_reseau)),
    labels = paste0(names(pourcentage_reseau), " (", round(pourcentage_reseau, 1), "%)"))
mfrow=c(1,1)


#histogramme des valeurs quantitatives

#Temps d'écran
hist(projet$Temps_Ecran,
     main = "Histogramme du temps d'écran",
     xlab = "Heures par jour",
     col="orange", breaks = 4)

#Stress
hist(projet$Stress,
     main = "Histogramme du niveau de stress",
     xlab = "Stress (1 à 10)",
     col="yellow")

#Sommeil
hist(projet$Qualite_Sommeil,
     main = "Histogramme de la qualité du sommeil",
     xlab = "Qualité du sommeil (1 à 10)",
     col="red")


### Analyse bivariée ###

# temps d'écran x stress

# Corrélation

mod <- lm(projet$Stress ~ projet$Temps_Ecran)
summary(mod) #R^2 = 0.547

cor(projet$Stress, projet$Temps_Ecran)# 0.739


# nuage de points
plot(projet$Temps_Ecran, projet$Stress,
     main = "Stress en fonction du Temps d'écran",
     xlab = "Temps d'écran (heures/jour)",
     ylab = "Stress (1-10)",
     pch = 19, col = "steelblue")

abline(lm(Stress ~ Temps_Ecran, data = projet), col = "red", lwd = 4)
#on voit clairement que le stress augmente avec le temps d'écran, elles sont très correlées

# stress x plateforme

# Boxplot : Stress selon Plateforme 
boxplot(projet$Stress ~ projet$Plateforme,
        main="Stress selon Plateforme",
        ylab="Stress",
        xlab="Plateforme")

fit = lm(projet$Stress ~ as.factor(projet$Plateforme))
anova(fit)
eta = 13.46/(13.46 + 1174.58)
eta

table(projet$Plateforme)

FAC = subset(projet$Stress, projet$Plateforme == "Facebook")
INS = subset(projet$Stress, projet$Plateforme == "Instagram")
LIN = subset(projet$Stress, projet$Plateforme == "LinkedIn")
TIT = subset(projet$Stress, projet$Plateforme == "TikTok")
TWI = subset(projet$Stress, projet$Plateforme == "X (Twitter)")
YOU = subset(projet$Stress, projet$Plateforme == "YouTube")

ybar = mean(projet$Stress, na.rm = TRUE)


vinter = 1/500 * (
  81*(mean(FAC)-ybar)^2 + 
    74*(mean(INS)-ybar)^2 + 
    87*(mean(LIN)-ybar)^2 + 
    95*(mean(TIT)-ybar)^2 + 
    88*(mean(TWI)-ybar)^2 + 
    75*(mean(YOU)-ybar)^2
)
vinter

vintra = ((var(FAC) * (81 - 1)) + 
            (var(INS) * (74 - 1)) + 
            (var(LIN) * (87 - 1)) + 
            (var(TIT) * (95 - 1)) + 
            (var(TWI) * (88 - 1)) + 
            (var(YOU) * (75 - 1))) / (500 - 6)
vintra

vtotal = vinter + vintra
n = vinter / vtotal
n

# stress x qualité du sommeil

cor(projet$Qualite_Sommeil, projet$Stress) # -0.584 
R2 = cor(projet$Qualite_Sommeil, projet$Stress)^2
R2 # 0.342

plot(projet$Stress, projet$Qualite_Sommeil,
     main = "Qualité du sommeil en fonction du stress",
     xlab = "Stress (1-10)",
     ylab = "Qualité du sommeil (1-10)",
     pch = 19, col = "darkgreen")

abline(lm(Qualite_Sommeil ~ Stress, data = projet), col = "red", lwd = 4)
#corrélés négativement, plus le stress est bas, plus l'individu dort bien

##conclusion : temps d'écran influe sur la qualité du sommeil !!


# stress x bonheur

cor(projet$Stress, projet$Bonheur) # -0.73721
R3=cor(projet$Stress, projet$Bonheur)^2
R3 # 0.543

plot(projet$Stress, projet$Bonheur,
     xlab="Stress", ylab="Bonheur",
     main="Bonheur en fonction du stress",
     col="purple", pch=19)
abline(lm(Bonheur ~ Stress, data=projet), col="red", lwd=4)
#leger, mais on remarque que, plus le stress monte plus le bonheur diminue


# temps d'écran x sommeil

cor(projet$Temps_Ecran, projet$Qualite_Sommeil) # -0.7589
R4=cor(projet$Temps_Ecran, projet$Qualite_Sommeil)^2
R4 # 0.576

plot(projet$Temps_Ecran, projet$Qualite_Sommeil,
     xlab="Temps d'écran (h/j)", ylab="Qualité du sommeil",
     main="Sommeil en fonction du temps d'écran",
     col="darkorange", pch=19)
abline(lm(Qualite_Sommeil ~ Temps_Ecran, data=projet), col="red", lwd=4)

#catégories de temps d'écran (coupes)
projet$Temps_Ecran_Cat <- cut(projet$Temps_Ecran,
                              breaks=c(-Inf, 3, 6, Inf),
                              labels=c("Faible","Moyen","Élevé"))

# Histogramme par catégories sommeil x écran
par(mfrow=c(1,3))
hist(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat=="Faible"],
     main="Sommeil - Faible écran",
     xlab="Qualité du sommeil", col="skyblue", breaks=8)

mean(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat == "Faible"])

hist(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat=="Moyen"],
     main="Sommeil - Moyen écran",
     xlab="Qualité du sommeil", col="dodgerblue", breaks=8)

mean(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat == "Moyen"])

hist(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat=="Élevé"],
     main="Sommeil - Écran élevé",
     xlab="Qualité du sommeil", col="royalblue", breaks=8)

mean(projet$Qualite_Sommeil[projet$Temps_Ecran_Cat == "Élevé"])

par(mfrow=c(1,1))
##conclusion : temps d'écran est un facteur corrélé négativement avec la qualité du sommeil

# bonheur x réseau sociaux

boxplot(Bonheur ~ Plateforme, data = projet,
        main = "Bonheur selon la plateforme",
        xlab = "Plateforme",
        ylab = "Bonheur",
        col = c("dodgerblue", "magenta", "navyblue", "darkgrey","skyblue","red"))


mean(projet$Bonheur[projet$Plateforme == "Facebook"])
mean(projet$Bonheur[projet$Plateforme == "Instagram"])
mean(projet$Bonheur[projet$Plateforme == "LinkedIn"])
mean(projet$Bonheur[projet$Plateforme == "TikTok"])
mean(projet$Bonheur[projet$Plateforme == "X (Twitter)"])
mean(projet$Bonheur[projet$Plateforme == "YouTube"])

##le bonheur n'a pas l'air de varier selon les réseaux

# bonheur selon les 3 valeurs quantitatives (temps d'ecran, sommeil, jours san reseaux)

# Corrélation entre bonheur et variables continues
cor(projet$Bonheur, projet$Temps_Ecran)# -0.705
cor(projet$Bonheur, projet$Qualite_Sommeil)# 0.67
cor(projet$Bonheur, projet$Jours_Sans_Reseaux)# 0.0635


projet$Temps_Ecran_cat <- cut(projet$Temps_Ecran, breaks = 3, labels = c("Bas", "Moyen", "Élevé"))
projet$Qualite_Sommeil_cat <- cut(projet$Qualite_Sommeil, breaks = 3, labels = c("Bas", "Moyen", "Élevé"))
projet$Jours_Sans_Reseaux_cat <- cut(projet$Jours_Sans_Reseaux, breaks = 3, labels = c("Peu", "Moyen", "Beaucoup"))

# Disposition des graphiques : 1 ligne, 3 colonnes
par(mfrow = c(1,3))

# Bonheur vs Temps d'écran
boxplot(Bonheur ~ Temps_Ecran_cat, data = projet, col="lightgreen",
        main="Bonheur vs Temps d'écran", ylab="Bonheur")

# Bonheur vs Qualité du sommeil
boxplot(Bonheur ~ Qualite_Sommeil_cat, data = projet, col="green",
        main="Bonheur vs Qualité du sommeil", ylab="Bonheur")

# Bonheur vs Jours sans réseaux
boxplot(Bonheur ~ Jours_Sans_Reseaux_cat, data = projet, col="darkgreen",
        main="Bonheur vs Jours sans réseaux", ylab="Bonheur")
##conclusion : le bonheur n'est pas du tout lié au jours sans réseau mais au temps d'écran et qualité sommeil oui.

# genre en fonction de 3 valeurs quantitaives (stress, sommeil, bonheur)

# Genre vs Stress
boxplot(Stress ~ Genre, data=projet,
        main="Stress selon le genre",
        xlab="Genre", ylab="Stress", 
        col=c("dodgerblue","magenta","grey"))

# Genre vs Sommeil
boxplot(Qualite_Sommeil ~ Genre, data=projet,
        main="Sommeil selon le genre",
        xlab="Genre", ylab="Qualité du sommeil", 
        col=c("dodgerblue","magenta","grey"))

# Genre vs Bonheur
boxplot(Bonheur ~ Genre, data=projet,
        main="Bonheur selon le genre",
        xlab="Genre", ylab="Bonheur", 
        col=c("dodgerblue","magenta","grey"))
par(mfrow=c(1,1))


### ACP ###
library(FactoMineR)
library(factoextra)

vars_acp <- projet[, c("Bonheur", "Temps_Ecran", "Qualite_Sommeil", "Jours_Sans_Reseaux", "Stress", "Sport")]

str(vars_acp)
summary(vars_acp)
vars_acp_scaled <- scale(vars_acp)  # moyenne=0, écart-type=1
acp <- prcomp(vars_acp_scaled, center = TRUE, scale. = TRUE)
summary(acp)
acp$rotation
loadings <- acp$rotation 
sd_pc <- acp$sdev        
coord_var <- t(t(loadings[,1:2]) * sd_pc[1:2])
coord_var
couleurs <- c("red", "orange", "dodgerblue", "#4DAF4A", "purple", "#F781BF")

theta <- seq(0, 2*pi, length=100)
plot(cos(theta), sin(theta),
     type = "l", asp = 1,
     xlab = paste0("PC1 (", round(summary(acp)$importance[2,1] * 100, 1), " %)"),
     ylab = paste0("PC2 (", round(summary(acp)$importance[2,2] * 100, 1), " %)"),
     main = "Cercle des corrélations")

arrows(0, 0, coord_var[,1], coord_var[,2], length=0.1, col=couleurs, lwd=3)

pos_vect <- c(3, 4, 2, 1, 4, 3)
text(coord_var[,1], coord_var[,2], labels=rownames(coord_var), pos=pos_vect, col=couleurs)
fviz_eig(acp,addlabels = T)

####conclusion : sport et jours sans reseaux ne sont pas pertinentes

### AFC ###


# age x plateforme

# Tableau d'age x plateforme

tab = table(projet$Age, projet$Plateforme)
tab_totaux <- addmargins(tab)
tab_totaux
mat <- as.matrix(tab)
row.names(mat) <- c("ados", "jeunes_adultes", "adultes", "seniors")
colnames(mat) <- c("Facebook", "Instagram", "LinkedIn", "TikTok", "X_Twitter", "YouTube")
addmargins(mat)
prop.table (mat) # frequences
Lin =prop.table (mat, 1) # profils lignes
Col = prop.table (mat, 2) # profils colonnes
Lin
Col

res=chisq.test(mat) #test de khi-2
res$statistic # khi 2
res$parameter # ddl
qchisq(0.95, res$parameter) # seuil, on fixe alpha=0.05
resp$p.value #0.4595 > 0.05

#khi-2 = 14.88 < 24.99579 les deux variables sont indépendantes 

#vérifier conditions test
sum(tab) # n= 500 > 30 fiable
res$expected # effectifs théoriques t_ij > 5
min(res$expected) # 11.1 > 5
#le test est fiable 

moyLin = colSums(mat)/sum(mat)
moyLin

rbind(Lin, moyLin)
mosaicplot(rbind(Lin, moyLin))

moyCol = rowSums(mat)/sum(mat)
moyCol

cbind(Col, moyCol)
mosaicplot(t(cbind(Col, moyCol))) 


tab_pct <- prop.table(tab, margin = 1) * 100   # Pourcentage par ligne (tranche d'âge)
par(mar = c(5, 4, 4, 10) + 0.1) # Agrandir l'espace à droite pour mettre la légende à l'extérieur

barplot(t(tab_pct), 
        main = "Utilisation des réseaux sociaux par tranche d'âge",
        xlab = "Tranche d'âge",
        ylab = "Pourcentage (%)",
        col = rainbow(ncol(tab_pct)),
        beside = TRUE)

legend("right",
       legend = colnames(tab_pct),
       fill = rainbow(ncol(tab_pct)),
       title = "Réseaux sociaux",
       inset = c(-0.35, 0),  # Plus décalé
       xpd = TRUE)
## Il n’existe pas de dépendance significative entre les tranches d’âge et la plateforme.

# afc age et réseaux

install.packages("factoextra")

library(FactoMineR)
library(factoextra)

mat.ca =CA(mat)
mat.ca$eig #on a besoin de 4 axes pour avoir >70% khi-2
fviz_eig(mat.ca,addlabels = T)


mat.ca$row$contrib[,1] #numérique
fviz_contrib(mat.ca, choice = "row", axes =1) #graphique 

fviz_contrib(mat.ca,choice="col",axes=1)

fviz_contrib(mat.ca,choice="row",axes=2)
fviz_contrib(mat.ca,choice="col",axes=2)

# afc stress et temps d'écran 

projet$Stress_cat <- cut(
  projet$Stress,
  breaks = c(-Inf, 3, 6, Inf),
  labels = c("faible", "moyen", "élevé")
)

projet$TempsEcran_cat <- cut(
  projet$Temps_Ecran,
  breaks = c(-Inf, 3, 6, Inf),
  labels = c("mauvais", "moyen", "bon")
)

projet_clean <- projet[complete.cases(projet$Stress_cat, projet$TempsEcran_cat), ]
tab_afc <- table(projet_clean$Stress_cat, projet_clean$TempsEcran_cat)
chisq.test(tab_afc)

library(FactoMineR)
library(factoextra)

res.afc <- CA(tab_afc, graph = FALSE)
fviz_ca_biplot(res.afc, repel = TRUE)
fviz_eig(res.afc,addlabels = T)


res.afc$row$contrib[,1] #numérique
fviz_contrib(res.afc, choice = "row", axes =1) #graphique 

fviz_contrib(res.afc,choice="col",axes=1)

fviz_contrib(res.afc,choice="row",axes=2)
fviz_contrib(res.afc,choice="col",axes=2)

#####Les modalités proches sont associées
