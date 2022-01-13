# td méta-analyse ESA
library(Rcmdr)
library(ggplot2)
library(dplyr)
library(FactoMineR)

####################
# étude de : 
# pH : pH moyen du rumen 
# C2 : proportion molaire d'acétate
# C3 : proportion molaire de propionate
# en fct de
# NDFF : Fibres (NDF) dans la ration (g/kg MS) apportées par le fourrage
# C_MOdeg : Matière organique dégradable dans le concentré (g/kg MS)

# le code Exp_fact regroupe les traitements au sein d'un essai 
# entre lesquels ne varie que la nature et/ou la proportion de concentré 

# Code_base : il y a en fait 3 sous-bases 
# base nature du concentré (base 1), 
# base proportion de concentré (base 2), 
# base nature/proportion de concentré (base 3)

# Objectifs : étudier les relations :
# pH = f(NDFF) est curvilinéaire (à tester plutôt sur la base 2, 
# qui fait varier la proportion de concentré donc F_NDF)
# pH = f(C_MOdeg) est linéaire
# C2 = f(C_MOdeg) et C3 = f(C_MOdeg) sont curvilinéaires.

###########
# importer et résumer
acidose <- 
  readXL("D:/Kuzulia/Formations/2022_meta-analyse_PA_ESA/jdd_TD2/acidose.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="acidose", stringsAsFactors=TRUE)

acidose <- within(acidose, {
  Code_base <- as.factor(Code_base)
  Code_espece <- as.factor(Code_espece)
  Code_publi <- as.factor(Code_publi)
  Exp <- as.factor(Exp)
  Exp_fact <- as.factor(Exp_fact)
  N.trait_publi <- as.factor(N.trait_publi)
  Trait <- as.factor(Trait)
})

summary(acidose)

# graphiques

ggplot(acidose) + theme_bw() +
  aes(x=NDFF, y=pH, col=Exp_fact) +
  geom_point() + 
  geom_line() +
  theme(legend.position ="none") + 
  facet_grid(~Base)  +
  labs(x="Fibres dans la ration (g/kg)",
       title="Variation du pH en fonction du NDF apporté par le fourrage") + 
  theme(legend.position = "none")

# ne travailler que sur la base "Proportion"
acidose2 = droplevels(filter(acidose, 
                             Base=="Proportion", 
                             Espece%in%c("Dairy_cow", "Steer")))
#acidose2 = acidose %>% filter(Base=="Proportion") %>% droplevels()

dim(acidose)
dim(acidose2)

summary(acidose2)

ggplot(acidose2) + theme_bw() +
  aes(x=NDFF, y=pH, col=Exp_fact) +
  geom_point() + 
  geom_line() +
  theme(legend.position ="none") +
  facet_grid(~Espece)

ggplot(acidose2) + theme_bw() +
  aes(x=NDFF, y=pH, group=Exp_fact, col=Espece) +
  geom_point() + 
  geom_line() 

#####
# ajustement du modèle curvilinéaire

ggplot(acidose2) + theme_bw() +
  aes(x=NDFF, y=pH) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE,
              formula = y~poly(x,2))

acidose2$NDFF
acidose2$NDFF2 = acidose2$NDFF^2
summary(acidose2)

LinearModel.1 <- lm(pH ~ NDFF + NDFF2, data=acidose2)
summary(LinearModel.1)

library(FactoMineR)
AovSum(pH ~ NDFF + NDFF2, data=acidose2)


LinearModel.1 <- lm(pH ~ NDFF , data=acidose2)
summary(LinearModel.1)

# exporter les données
write.table(acidose2, 
            "D:/Kuzulia/Formations/2022_meta-analyse_PA_ESA/jdd_TD2/acidose2.csv",
            sep=";", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")

# graphique final

ggplot(acidose2) + theme_bw() +
  aes(x=NDFF, y=pH) +
  geom_point(aes(col=Espece)) + 
  geom_line(aes(group=Exp_fact, col=Espece)) +
  geom_smooth(method=lm, se=FALSE,
              formula = y~poly(x,2), col="black")

##########
# que les vaches laitieres

vaches_laitieres = droplevels(filter(acidose2, Espece=="Dairy_cow"))

ggplot(vaches_laitieres) + theme_bw() +
  aes(x=NDFF, y=pH) +
  geom_point() + 
  geom_line(aes(group=Exp_fact)) +
  geom_smooth(method=lm, se=FALSE, col="red")

LinearModel.1 <- lm(pH ~ NDFF + NDFF2 , data=vaches_laitieres)
summary(LinearModel.1) # effet quadratique non significatif

LinearModel.1 <- lm(pH ~ NDFF  , data=vaches_laitieres)
summary(LinearModel.1) # ok

library(FactoMineR)
AovSum(pH ~ NDFF * Exp_fact, data=vaches_laitieres) # interaction non significative
AovSum(pH ~ NDFF + Exp_fact, data=vaches_laitieres)

###
# graphe final pour les vaches laitières uniquement
ggplot(vaches_laitieres) + theme_bw() +
  aes(x=NDFF, y=pH) +
  geom_point() + 
  geom_line(aes(group=Exp_fact)) +
  geom_smooth(method=lm, se=FALSE, col="red") + 
  geom_abline(intercept = 5.723, slope = 0.0018, col="blue", size=1.5)

####################################################################################################
# archives ancien TD

par(mfrow=c(2,2))
plot(pH~NDFF, data=acidose, pch=20, xlab="Fibres dans la ration (g/kg)", col=4)
plot(pH~C_MOdeg, data=acidose, pch=20, xlab="Matière organique dégradable dans le concentré", col=4)
plot(C2~C_MOdeg, data=acidose, pch=20, xlab="Matière organique dégradable dans le concentré", ylab="Acetate", col=4)
plot(C3~C_MOdeg, data=acidose, pch=20, xlab="Matière organique dégradable dans le concentré", ylab="Propionate", col=4)
