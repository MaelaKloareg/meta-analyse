# TD de m�ta-analyse de Maela Kloareg pour l'ESA d'Angers

# D�lai de reprise de cyclicit� chez la vache laiti�re
# Etude bibliographique r�alis�e par Erwan Cutullic

#Le but est d'�valuer l'effet d'une augmentation du niveau de production laiti�re du d�but de lactation (moyenne sur les 100 premiers jours environ) sur le d�lai de reprise de cyclicit� (mesur� en jours apr�s le v�lage)
#1 - lorsqu'� conduite �quivalente l'augmentation de PL est permise par la g�n�tique des animaux (potentiel laitier + VS potentiel laitier -)
#2 - lorsqu'� g�n�tique �quivalente, l'augmentation de PL est permise par l'alimentation (alim + VS alim -)

setwd("D:\\Kuzulia\\Formations\\2022_meta-analyse_PA_ESA\\jdd")

library(ggplot2)
library(FactoMineR)
library(Rcmdr)

# 1/ augmentation de la PL grace � la g�n�tique, � alimentation constante
# importation des donn�es
genet <- readXL("jdd1_genet.xls",
                rownames=FALSE, header=TRUE, na="", stringsAsFactors=TRUE)
# cr�ation du facteur groupe : concatener la publi et l'alim (car l'alim doit etre la m�me au sein d'un groupe)
genet$groupe = as.factor(paste(genet$publication, genet$alimentation, sep="_"))
summary(genet)

# option : avec Rcmdr
scatterplot(reprise~pldeb | groupe, regLine=TRUE, 
            smooth=FALSE, boxplots=FALSE, by.groups=TRUE, data=genet)

LinearModel.4 <- lm(reprise ~ pldeb * groupe, data=genet)
summary(LinearModel.4)
Anova(LinearModel.4, type="II")


# l'interaction entre PL et le groupe n'est pas significtive
# quel que soit le groupe, l'effet de PL sur reprise est similaire
# (attention, puissance de test tr�s faible)

LinearModel.4 <- lm(reprise ~ pldeb + groupe, data=genet)
summary(LinearModel.4)
Anova(LinearModel.4, type="II")

# la pente vaut 0.75, donc si on augmente la PLdeb de 1 kg/j, 
# alors �a augmente la reprise de cyclicit� de 0.75 jour en moyenne

#### graphique avec ggplot
ggplot(genet) + theme_classic() +
  aes(x=pldeb, y=reprise, col=groupe) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE)
  
# pour Cyril
ggplot(genet) + theme_classic() +
  aes(x=pldeb, y=reprise, col=groupe) +
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE)

# mod�le de covariance

AovSum(reprise ~ pldeb + groupe, data=genet)

ggplot(genet) + theme_classic() +
  aes(x=pldeb, y=reprise, col=groupe) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) + 
  geom_abline(intercept = 11.31526, slope = 0.75198, col="red", size=1.5)

# 2/ augmentation de la PL grace � l'alim, � genetique constante

# importation des donn�es
alim <- readXL("jdd2_alim.xls",
                rownames=FALSE, header=TRUE, na="", stringsAsFactors=TRUE)
alim$groupe = as.factor(paste(alim$publication, alim$genetique, sep="_"))

summary(alim)

ggplot(alim) + theme_classic() +
  aes(x=pldeb, y=reprise, col=groupe) +
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE)

# on visualise trois sc�narii : 
# A/ l'augmentation de la PL am�liore la repro!
# B/ l'augmentation de la PL n'impacte pas la repro!
# B/ l'augmentation de la PL d�grade la repro

# ici, je propose d'arreter l'analyse sur cette interpr�tation graphique.

# en effet, les r�sultats du mod�le d'analyse de covariance ne sont pas d'une grande aide : 

AovSum(reprise~pldeb*groupe, data=alim)$Ftest
AovSum(reprise~pldeb+groupe+pldeb:groupe, data=alim)$Ftest

# selon ce mod�le, l'interaction entre PL et le groupe n'est pas significtive
# quel que soit le groupe, l'effet de PL sur reprise est similaire
# (attention, puissance de test tr�s faible)

#mod�le alternatif pour augmenter la puissance : 
AovSum(reprise~pldeb:groupe, data=alim)

# car ce mod�le n'est pas convaincant!
AovSum(reprise~pldeb + groupe, data=alim)$Ftest

