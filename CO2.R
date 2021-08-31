#Importation du jeu de donnée construit avec Excel,
#compilant 5 sources de données

library(readxl)
CO2 <- read_excel("CO2.xlsx")
View(CO2)

#Appel aux librairies de Manipulation & de Visualisation de données 

library(tidyverse)
library(scales)      
library(broom)
library(stargazer)
library(lmtest)
library(AER)
library(rdrobust)
library(rddensity)

install.packages("tidyverse")
install.packages("scales")
install.packages("broom")
install.packages("stargazer")
install.packages("lmtest")
install.packages("AER")
install.packages("rdrobust")
install.packages("rddensity")

setwd("C:/Users/qkzr3361/Desktop/Empreinte Carbone/CO2")

#Découverte du jeu de donnée comme décrite dans le mémoire à l'aide de l'Excel

sapply(CO2, class)
summary(CO2)
str(CO2)
glimpse(CO2)
is.data.frame(CO2)

#Création de Fichiers Intermédiaires pour Faciliter le partage de connaissance

belgium <- CO2 %>% 
  filter(Country == "Belgium")

# 1. Connaître les caractéristiques du produit le plus émetteur de CO² 
#son importance pour Orange, les pays et sa Type/Catégorie de rattachement

belgium %>% slice_max('2019 Impact')

#Regression Linéraire sur les Variables d'origine
model1 <- lm(Price ~ `2019 Sales`, data = CO2)
  summary(model1)

#Création & Opérations sur colonnes de Colonnes personnalisés 
  
CO2$IPMoyen <- paste((CO2$`2019 Impact`+CO2$`2020 Impact`)/2)
CO2$RevMoyen <- paste((CO2$`T-Cumulated Revenues`-CO2$`2021  Q1 Revenues`)/2)

#On s'assure du format (Data.Frame & Numérique) de ces colonnes 

CO2$IPMoyen <- as.numeric(CO2$IPMoyen)
CO2$RevMoyen <- as.numeric(CO2$RevMoyen)

is.data.frame(CO2$IPMoyen)

#2. Modèle de regression autour de la compilation des données Carbone
    #Limpact est naturellement positivement correlé au revenue
    #que l'"objet génère pour Orange
model2 <- lm(RevMoyen ~ IPMoyen, data = CO2)
stargazer(model2, type = 'text')

    #L'augmentation des revenus générés par un objets augmente son impact moyen
model2b <- lm(IPMoyen ~ RevMoyen, data = CO2)
stargazer(model2b, type = 'text')

#3. Prédiction l'impact moyen d'un objet random 
    #Les ventes d'un objet générant 250k€ annuel émet environ 0.37 T CO2
predict(model2b, newdata=data.frame(RevMoyen=250000),
se.fit=TRUE, interval = "prediction", level = 0.99)

    #Les ventes d'un objet émettant 25k CO2 annuel génère environ 38k€/an
predict(model2, newdata=data.frame(IPMoyen=25118.6404), se.fit=TRUE,
interval = "prediction", level = 0.99)

    #Prédire les futurs valeurs de la regression
predicted_CO2 <- data.frame(IPred = predict(model2b, CO2), IPMoyen=CO2$IPMoyen)

#4. GGPLOT2
ggplot(data = CO2, aes(x = `2020 Impact`, y = `2020 Sales`)) + 
  geom_point(color='blue') +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE)


#5. Prédiction l'impact moyen des prochains écouteurs en France ? 
    #Plusieurs Filtres

HearableFrance <- CO2[CO2$Country == "France" & CO2$`Product Type` == "Hearable",]
HearableFrance

    #Regression
    #Parmis les écouteurs distribués par Orange lorsque le prix augmente 
    #de 1€, le volume annuel des objets émet + 234.107 KgO2

HF.Model <- lm(IPMoyen ~ Price + RevMoyen, data = HearableFrance)
stargazer(HF.Model, type = 'text')

    #Prediction sur la regression

HF.Pred <- data.frame(HFP = predict(HF.Model, HearableFrance), Price=HearableFrance$Price)

ggplot(data = HearableFrance, aes(x = IPMoyen, y = Price)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = HF.Pred, aes(x=HFP, y=Price))

predict(HF.Model, newdata=data.frame(Price=252),
  se.fit=TRUE, interval = "prediction", level = 0.99)


#6. Regression & Prediction sur les variables Orange fixes !!

    #Test avec plusieurs variabes 
Fixes <- lm(IPMoyen ~ `GDP/hab` + `Blended ARPU` + `FBB Penetration` + `FTTx Penetration`, data = CO2)
stargazer(Fixes, type = 'text')

    #Penetration de la connectivité en France
    #Les Investissements en Connectivité des foyers sont positivement correlés
    #avec l'augmentation de l'impact des objets connectés 
      #
Fixes.Pourcent <- lm(IPMoyen ~ `FBB Penetration` + `FTTx Penetration`, data = CO2)
stargazer(Fixes.Pourcent, type = 'text')

    #Les POurcentages MS, Connectivité 
    #a. l'impact des objets dans un pays donnée est correlé positivement
    #avec l'urbanisation de la population (+1% urba ==> +555Kg CO2)
    #Ainsi que le déploiement de la fibre, (+1% pénétration ==> + 50Kg CO2)
Fixes.Pourcent2 <- lm(IPMoyen ~ `FBB Penetration` + `FTTx Penetration` + `MS Fixed BB` + `MS Mobile` + `Urban_ Pop`, data = CO2)
stargazer(Fixes.Pourcent2, type = 'text')

    #Cross-model analysis 
stargazer(Fixes.Pourcent, Fixes.Pourcent2, type="text",
  dep.var.labels=c("IPMoyen /Kg CO2"),
  covariate.labels=c("FBB Penetration","FTTx Penetration","MS Fixed BB","MS Mobile", "Urban_ Pop"))

    #Prédiction 
pred <- predict(Fixes.Pourcent, interval = "prediction")
  mydata <- cbind(CO2, pred)
  
p <- ggplot(mydata, aes(`FTTx Penetration`, IPMoyen)) +
  geom_point() +
  stat_smooth(method = lm)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")

#5C Contenu additionnel sur les émissions
FitnessFrance <- CO2[CO2$Country == "France" & CO2$`Product Type` == "FitnessBand",]
FF.Model <- lm(`2020 Impact` ~ Price, data = FitnessFrance )
GPSFrance <- CO2[CO2$Country == "France" & CO2$`Product Type` == "GPS Tracker",]
GF.Model <- lm(`2020 Impact` ~ Price, data = GPSFrance )
SmartwatchFrance <- CO2[CO2$Country == "France" & CO2$`Product Type` == "Smartwatch",]
SF.Model <- lm(`2020 Impact` ~ Price, data = SmartwatchFrance )


############"

#6. Début des analyses AVEC effets d'aubène

#6.1 Vérification du dataset: Multi correlation
    #Creation d'un nouveau dataset uniquement variables 

CO2_check <- CO2[,-1:-13]
cor(CO2_check1, method = c("pearson", "kendall", "spearman"))

cor(CO2_check1, use = "complete.obs")
Ormcor <- cor(CO2_check1)
mcor

    #Creation d'un nouveau data-set plus propre _Variables
library(Hmisc)
rcorr(as.matrix(Variables[,1:7]))

cor(Variables, use = "complete.obs")
mcor <- cor(Variables)
mcor

V<-cor(Variables)
head(round(V,2))
library(corrplot)
corrplot(V, method="circle")

#7. Regression & Prediction sur les variables Orange fixes !

#Test avec plusieurs variabes 
Fixes <- lm(IPMoyen ~ `GDP/hab` + `Blended ARPU` + `FBB Penetration` + `FTTx Penetration`, data = CO2)
stargazer(Fixes, type = 'text')

#Penetration de la connectivité en France
#Les Investissements en Connectivité des foyers sont positivement correlés
#avec l'augmentation de l'impact des objets connectés 
#
Fixes.Pourcent <- lm(IPMoyen ~ `FBB Penetration` + `FTTx Penetration`, data = CO2)
stargazer(Fixes.Pourcent, type = 'text')

#Les POurcentages MS, Connectivité 
#a. l'impact des objets dans un pays donnée est correlé positivement
#avec l'urbanisation de la population (+1% urba ==> +555Kg CO2)
#Ainsi que le déploiement de la fibre, (+1% pénétration ==> + 50Kg CO2)
Fixes.Pourcent2 <- lm(IPMoyen ~ `FBB Penetration` + `FTTx Penetration` + `MS Fixed BB` + `MS Mobile` + `Urban_ Pop`, data = CO2)
stargazer(Fixes.Pourcent2, type = 'text')

#Cross-model analysis 
stargazer(Fixes.Pourcent, Fixes.Pourcent2, type="text",
          dep.var.labels=c("IPMoyen /Kg CO2"),
          covariate.labels=c("FBB Penetration","FTTx Penetration","MS Fixed BB","MS Mobile", "Urban_ Pop"))

#Prédiction 
pred <- predict(Fixes.Pourcent, interval = "prediction")
mydata <- cbind(CO2, pred)

p <- ggplot(mydata, aes(`FTTx Penetration`, IPMoyen)) +
  geom_point() +
  stat_smooth(method = lm)

p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed")



#Cross-ountry analysis (stargaer multiple avec modèle par pays)
CO2b$Position[CO2b$Position==0]<-FALSE
CO2b$Position[CO2b$Position==1]<-TRUE

ggplot(data = CO2b, mapping = aes(x = `dev environnement-related tech`, y = `FTTx Penetration`, color = Position)) +
  geom_point(size = 0.5, alpha = 0.5) +
geom_vline(xintercept = 0.5) +
geom_smooth(method = "lm")

CO2b$ENR[CO2b$`dev environnement-related tech`>=0.1]<-TRUE
CO2b$ENR[CO2b$`dev environnement-related tech`< O.1]<-FALSE

ggplot(data = CO2b, mapping = aes(x = RM, y = Country, color = ENR)) +
  geom_point(size = 2, alpha = 10) +
  geom_vline(xintercept = 25000) +
  geom_smooth(method = "lm")



#8. Recherche d'arguments au niveau des categories
Nomade <- CO2b[CO2b$Category == "Wearables",]
Domicile <- CO2b[CO2b$Category == "Home Entertainment",]

tab <- table(CO2$Category)
tab

stargazer(Nomadisme, Domiciliation, Security, type="text",
          column.labels=c("Nomadisme", "Domiciliation", "Security"),
dep.var.labels=c("Impact Fibre sur les émissions"),
ovariate.labels=c("FTTx Penetration"))

#9. CrossCountry Final Analysis
Pays <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = CO2b)
stargazer(Pays, type = 'text')

France <- CO2b[CO2b$Country == "France",]
France.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = France)
Spain <- CO2b[CO2b$Country == "Spain",]
Spain.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Spain)
Luxembourg <- CO2b[CO2b$Country == "Luxembourg",]
Luxembourg.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Luxembourg)
Poland <- CO2b[CO2b$Country == "Poland",]
Poland.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Poland)
Romania <- CO2b[CO2b$Country == "Romania",]
Romania.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Romania)
Belgium <- CO2b[CO2b$Country == "Belgium",]
Belgium.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Belgium)
Slovakia <- CO2b[CO2b$Country == "Slovakia",]
Slovakia.P <- lm(IPM ~ `Environmental Performance Index` + `Environmental Policy Stringency Index` + `renewable energy public RD&D budget` + `dev environnement-related tech`, data = Slovakia)