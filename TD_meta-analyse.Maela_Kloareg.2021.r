# TD de méta-analyse de Maela Kloareg pour l'ESA d'Angers

# Délai de reprise de cyclicité chez la vache laitière
# Etude bibliographique réalisée par Erwan Cutullic

#Le but est d'évaluer l'effet d'une augmentation du niveau de production laitière du début de lactation (moyenne sur les 100 premiers jours environ) sur le délai de reprise de cyclicité (mesuré en jours après le vêlage)
#1 - lorsqu'à conduite équivalente l'augmentation de PL est permise par la génétique des animaux (potentiel laitier + VS potentiel laitier -)
#2 - lorsqu'à génétique équivalente, l'augmentation de PL est permise par l'alimentation (alim + VS alim -)

setwd("D:\\Kuzulia\\Formations\\2012_03_meta-analyse_PA_ESA\\jdd")

library(ggplot2)
library(FactoMineR)

###########
# genet : à conduite équivalente l'augmentation de PL est permise par la génétique des animaux (potentiel laitier + VS potentiel laitier -)

# Importation
genet <- read.csv2("jdd1_genet.csv")

genet$publication = as.factor(genet$publication)
genet$conduite = as.factor(genet$conduite)
genet$genetique = as.factor(genet$genetique)

genet$groupe = as.factor(paste(genet$publication, genet$conduite, sep="_"))

# ne garder que certaines variables
genet= genet[ ,c("pldeb","reprise","groupe")]

# Vérification
dim(genet)
summary(genet)

# graphiques

ggplot(genet) + aes(y=reprise, x=pldeb) + theme_bw() +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)+
  geom_smooth(method=lm, se=FALSE, aes(y=reprise, x=pldeb, col=groupe))

ggplot(genet) + theme_bw() + 
  geom_point(aes(y=reprise, x=pldeb, col=groupe)) + 
  geom_smooth(method=lm, se=FALSE, aes(y=reprise, x=pldeb, col=groupe)) + 
  #geom_smooth(method=lm, se=FALSE, aes(y=reprise, x=pldeb), color="grey") + 
  ggtitle("Augmentation de PLdeb en jouant sur la génétique, conduite constante") +
  labs(x="production laitière en début de lactation", 
       y="reprise de cyclicité",
       subtitle="concurrence entre production laitière et reproduction") +
  geom_abline(intercept=11.3290989, slope=0.7543331)

AovSum(reprise~pldeb*groupe, data=genet)$Ftest

# l'interaction entre PL et le groupe n'est pas significtive
# quel que soit le groupe, l'effet de PL sur reprise est similaire
# (attention, puissance de test très faible)

AovSum(reprise~pldeb + groupe, data=genet)$Ftest

AovSum(reprise~pldeb + groupe, data=genet)$Ttest

# la pente vaut 0.75, donc si on augmente la PLdeb de 1 kg/j, 
# alors ça augmente la reprise de cyclicité de 0.75 jour en moyenne

####################
# alim : à génétique équivalente, l'augmentation de PL est permise par l'alimentation (alim + VS alim -)

# Importation
alim <- read.csv2("jdd2_alim.csv")

alim$publication = as.factor(alim$publication)
alim$alimentation = as.factor(alim$alimentation)
alim$genetique = as.factor(alim$genetique)

alim$groupe = as.factor(paste(alim$publication, alim$genetique, sep="_"))
summary(alim$groupe)

# ne garder que certaines variables
alim= alim[ ,c("pldeb","reprise","groupe")]

# Vérification
dim(alim)
summary(alim)

# graphiques

ggplot(alim) + aes(y=reprise, x=pldeb) + theme_bw() +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)

ggplot(alim) + theme_bw() + 
  geom_point(aes(y=reprise, x=pldeb, col=groupe)) + 
  geom_smooth(method=lm, se=FALSE, aes(y=reprise, x=pldeb, col=groupe)) + 
  #geom_smooth(method=lm, se=FALSE, aes(y=reprise, x=pldeb), color="grey") + 
  ggtitle("Augmentation de PLdeb en jouant sur l'alimentation, génétique constante") +
  labs(x="production laitière en début de lactation", 
       y="reprise de cyclicité",
       subtitle="concurrence entre production laitière et reproduction") 

# on visualise trois scénarii : 
# A/ l'augmentation de la PL améliore la repro!
# B/ l'augmentation de la PL n'impacte pas la repro!
# B/ l'augmentation de la PL dégrade la repro

# ici, je propose d'arreter l'analyse sur cette interprétation graphique.

# en effet, les résultats du modèle d'analyse de covariance ne sont pas d'une grande aide : 

AovSum(reprise~pldeb*groupe, data=alim)$Ftest
AovSum(reprise~pldeb+groupe+pldeb:groupe, data=alim)$Ftest

# selon ce modèle, l'interaction entre PL et le groupe n'est pas significtive
# quel que soit le groupe, l'effet de PL sur reprise est similaire
# (attention, puissance de test très faible)

#modèle alternatif pour augmenter la puissance : 
AovSum(reprise~pldeb:groupe, data=alim)

# car ce modèle n'est pas convaincant!
AovSum(reprise~pldeb + groupe, data=alim)$Ftest

