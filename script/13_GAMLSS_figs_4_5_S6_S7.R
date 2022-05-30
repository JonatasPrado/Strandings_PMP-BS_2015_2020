
## 
## Intensive and wide-ranging beach surveys uncover 
## temporal and spatial stranding patterns of marine megafauna
##
## ICES Journal of Marine Science - 2022
##
## Code by Jonatas F. H. Prado & Nicholas W. Daudt

## This script runs GAMLSS models to assess temporal and spatial patterns 
## of the 14 select species (i.e. numeric frequency > 1 and n > 100). See 
## the main text for rationale and detailed explanations

################################################################################

# Libraries ####
library(gamlss)
library(dplyr)

################################################################################

## Open dataset
dfModel <- read.csv2("./data_out/dfSumFinal.csv")

# Rename and set some column types
dfModel <-
  dfModel %>%
  dplyr::rename(Month = month, Polygon = id_polygon, Year = year_n)

dfModel$Month <- as.numeric(dfModel$Month)
dfModel$Polygon <- as.numeric(dfModel$Polygon)
dfModel$Year <- as.factor(dfModel$Year)

#----------------------------Reptilia

########### Caretta caretta

## Exploratory data analyses
par(mfrow = c(2, 2))
hist(dfModel$Caretta_caretta_Reptilia)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Month)
boxplot(dfModel$Caretta_caretta_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model 
distFit <- gamlss::fitDist(y = Caretta_caretta_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: WARING BNB

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("WARING","BNB","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Cc <- list()

for(fam in 1:length(fams)) {
  dist.test.Cc[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Caretta_caretta_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel))}

unlist(dist.test.Cc) #lowest AIC BNB 

## Modeling "mu" (Model M) using the selected distribution BNB
CcM1 <- gamlss(Caretta_caretta_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = BNB, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution BNB
CcM2 <- update(CcM1,sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC (CcM1,CcM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow=c(1,1))
gamlss::wp(CcM2)
gamlss::wp(CcM2, ylim.all = 4)
plot(CcM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function 

# "mu"
dropCcM2_mu <- drop1(CcM2) %>%
  as.data.frame() 

# "sigma"
dropCcM2_si <- drop1(CcM2, what = "sigma") %>%
  as.data.frame()

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropCcM2_mu <-
  dropCcM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Caretta caretta", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("BNB", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CcM1)) %>%
  dplyr::mutate(Model2 = AIC(CcM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropCcM2_si <-
  dropCcM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Caretta caretta", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("BNB", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CcM1)) %>%
  dplyr::mutate(Model2 = AIC(CcM2)) 

dropCcM2 <-rbind(dropCcM2_mu, dropCcM2_si)

## Generalized R-squared
gamlss::Rsq(CcM2)

########### Chelonia mydas

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Chelonia_mydas_Reptilia)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Month)
boxplot(dfModel$Chelonia_mydas_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Chelonia_mydas_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
              # the ones with less than two units of difference: GEOM 
              # GEOMo ZISICHEL ZASICHEL WARING NBI NBII

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GEOM","GEOMo","ZISICHEL","ZASICHEL","WARING",
         "PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Cm <- list()

for(fam in 1:length(fams)) {
  dist.test.Cm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Chelonia_mydas_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel))}

unlist(dist.test.Cm) #lowest AIC NBII

## Modeling "mu" (Model M) using the selected distribution NBII
CmM1 <- gamlss(Chelonia_mydas_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = NBII, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution NBII
CmM2 <- update(CmM1,sigma.fo= ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(CmM1,CmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(2,2))
gamlss::wp(CmM2)
gamlss::wp(CmM2, ylim.all = 1)
plot(CmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropCmM2_mu <- drop1(CmM2) %>%
  as.data.frame() 

# "sigma"
dropCmM2_si <- drop1(CmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropCmM2_mu <-
  dropCmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Chelonia mydas", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CmM1)) %>%
  dplyr::mutate(Model2 = AIC(CmM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropCmM2_si <-
  dropCmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Chelonia mydas", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(CmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(CmM1)) %>%
  dplyr::mutate(Model2 = AIC(CmM2)) 

dropCmM2 <-rbind(dropCmM2_mu, dropCmM2_si)

## Generalized R-squared
gamlss::Rsq(CmM2) 

########### Lepidochelys olivacea

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Lepidochelys_olivacea_Reptilia)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Polygon)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Month)
boxplot(dfModel$Lepidochelys_olivacea_Reptilia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Lepidochelys_olivacea_Reptilia, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
              # the ones with less than two units of difference: 
              # NBII NBI GPO ZANBI ZINBI ZALG ZIPIG ZAPIG SICHEL NBF SI DEL BNB
 
## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","ZALG","SICHEL","NBF","SI","DEL","BNB",
         "NBII","NBI","PO","ZINBI","ZANBI","ZIPIG","ZAPIG")

dist.test.Lo <- list()

for(fam in 1:length(fams)) {
  dist.test.Lo[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Lepidochelys_olivacea_Reptilia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Lo) #lowest AIC GPO 

## Modeling "mu" (Model M) using the selected distribution GPO
LoM1 <- gamlss(Lepidochelys_olivacea_Reptilia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = GPO , data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
LoM2 <- update(LoM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(LoM1,LoM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(LoM1)
gamlss::wp(LoM1, ylim.all = 4)
plot(LoM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropLoM1_mu <- drop1(LoM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropLoM1_mu <-
  dropLoM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Lepidochelys olivacea", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LoM1)*100) %>%
  dplyr::mutate(Model1 = AIC(LoM1)) %>%
  dplyr::mutate(Model2 = AIC(LoM2))

dropLoM1 <- dropLoM1_mu

## Generalized R-squared
gamlss::Rsq(LoM1)

#---------------------------------------Aves

########### Spheniscus magellanicus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Spheniscus_magellanicus_Aves)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Month)
boxplot(dfModel$Spheniscus_magellanicus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Spheniscus_magellanicus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference:  
             # DEL SI SICHEL ZALG 

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("DEL","SI","SICHEL","ZALG","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Sm <- list()

for(fam in 1:length(fams)) {
  dist.test.Sm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Spheniscus_magellanicus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +  
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cycy = 200))}

unlist(dist.test.Sm) #lowest AIC SI

## Modeling "mu" (Model M) using the selected distribution SI
SmM1 <- gamlss(Spheniscus_magellanicus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = SI , data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SI
SmM2 <- update(SmM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(SmM1,SmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SmM2)
gamlss::wp(SmM2, ylim.all = 4)
plot(SmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSmM2_mu <- drop1(SmM2) %>%
  as.data.frame() 

# "sigma"
dropSmM2_si <- drop1(SmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSmM2_mu <-
  dropSmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Spheniscus magellanicus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SmM1)) %>%
  dplyr::mutate(Model2 = AIC(SmM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSmM2_si <-
  dropSmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Spheniscus magellanicus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SmM1)) %>%
  dplyr::mutate(Model2 = AIC(SmM2)) 

dropSmM2 <-rbind(dropSmM2_mu, dropSmM2_si)

## Generalized R-squared
gamlss::Rsq(SmM2)

########### Larus dominicanus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Larus_dominicanus_Aves)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Month)
boxplot(dfModel$Larus_dominicanus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Larus_dominicanus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits  # Selected distributions with lowest AIC and 
              # the ones with less than two units of difference: 
              # GPO WARING ZANBI SI SICHEL

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","SICHEL","SI","WARING","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Ld <- list()

for(fam in 1:length(fams)) {
  dist.test.Ld[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Larus_dominicanus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Ld) #lowest AIC GPO 

## Modeling "mu" (Model M) using the selected distribution GPO
LdM1 <- gamlss(Larus_dominicanus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
LdM2 <- update(LdM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(LdM1,LdM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(LdM2)
gamlss::wp(LdM2, ylim.all = 6)
plot(LdM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropLdM2_mu <- drop1(LdM2) %>%
  as.data.frame() 

# "sigma"
dropLdM2_si <- drop1(LdM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropLdM2_mu <-
  dropLdM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Larus dominicanus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LdM2)*100) %>%
  dplyr::mutate(Model1 = AIC(LdM1)) %>%
  dplyr::mutate(Model2 = AIC(LdM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropLdM2_si <-
  dropLdM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Larus dominicanus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(LdM2)*100) %>%
  dplyr::mutate(Model1 = AIC(LdM1)) %>%
  dplyr::mutate(Model2 = AIC(LdM2)) 

dropLdM2 <- rbind(dropLdM2_mu, dropLdM2_si)

## Generalized R-squared
gamlss::Rsq(LdM2)

########### Puffinus puffinus 

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Puffinus_puffinus_Aves)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Polygon)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Month)
boxplot(dfModel$Puffinus_puffinus_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Puffinus_puffinus_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: GPO SI SICHEL  

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("SI","GPO","SICHEL","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Pp <- list()

for(fam in 1:length(fams)) {
  dist.test.Pp[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Puffinus_puffinus_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pp) #lowest AIC SI

## Modeling "mu" (Model M) using the selected distribution SICHEL
PpM1 <- gamlss(Puffinus_puffinus_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)),
               family = SI, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SICHEL
PpM2 <- update(PpM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon,df = 7))

AIC(PpM1,PpM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PpM2)
gamlss::wp(PpM2, ylim.all = 6)
plot(PpM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPpM2_mu <- drop1(PpM2) %>%
  as.data.frame() 

# "sigma"
dropPpM2_si <- drop1(PpM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPpM2_mu <-
  dropPpM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Puffinus puffinus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PpM2)*100) %>%
  dplyr::mutate(Model1 = AIC(PpM1)) %>%
  dplyr::mutate(Model2 = AIC(PpM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropPpM2_si <-
  dropPpM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Puffinus puffinus", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PpM2)*100) %>%
  dplyr::mutate(Model1 = AIC(PpM1)) %>%
  dplyr::mutate(Model2 = AIC(PpM2)) 

dropPpM2 <- rbind(dropPpM2_mu, dropPpM2_si)

## Generalized R-squared  
gamlss::Rsq(PpM2)

########### Thalassarche chlororhynchos

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Thalassarche_chlororhynchos_Aves)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves~ dfModel$Polygon)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves ~ dfModel$Month)
boxplot(dfModel$Thalassarche_chlororhynchos_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Thalassarche_chlororhynchos_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # GPO ZALG SI SICHEL  

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","SI","SICHEL","PO","NBI","NBII","ZINBI","ZANBI","ZALG")

dist.test.Tc <- list()

for(fam in 1:length(fams)) {
  dist.test.Tc[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Thalassarche_chlororhynchos_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel,n.cyc = 200))}

unlist(dist.test.Tc) #lowest AIC NBII

## Modeling "mu" (Model M) using the selected distribution NBII
TcM1 <- gamlss(Thalassarche_chlororhynchos_Aves ~ 
                 cs(Month,df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = NBII, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution NBII
TcM2 <- update(TcM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(TcM1,TcM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TcM2)
gamlss::wp(TcM2, ylim.all = 6)
plot(TcM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTcM2_mu <- drop1(TcM2) %>%
  as.data.frame() 

# "sigma"
dropTcM2_si <- drop1(TcM2, what = "sigma") %>%
  as.data.frame()

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTcM2_mu <-
  dropTcM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Thalassarche chlororhynchos", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(TcM1)) %>%
  dplyr::mutate(Model2 = AIC(TcM2))

Parameter <- c("Si_none", "Si_month", "Si_polygon")

dropTcM2_si <-
  dropTcM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Thalassarche chlororhynchos", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("NBII", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TcM2)*100) %>%
  dplyr::mutate(Model1 = AIC(TcM1)) %>%
  dplyr::mutate(Model2 = AIC(TcM2)) 

dropTcM2 <- rbind(dropTcM2_mu, dropTcM2_si)

## Generalized R-squared
gamlss::Rsq(TcM2)

########### Thalassarche melanophris

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Thalassarche_melanophris_Aves)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Polygon)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Month)
boxplot(dfModel$Thalassarche_melanophris_Aves~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Thalassarche_melanophris_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # ZALG GPO NBII NBI DEL SICHEL SI ZANBI

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("ZALG","GPO","DEL","SICHEL","SI","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Tm <- list()

for(fam in 1:length(fams)) {
  dist.test.Tm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Thalassarche_melanophris_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     random(as.factor(Year)) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Tm) #lowest AIC DEL 

## Modeling "mu" (Model M) using the selected distribution DEL
TmM1 <- gamlss(Thalassarche_melanophris_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = DEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution DEL
TmM2 <- update(TmM1, sigma.fo= ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(TmM1,TmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TmM1)
gamlss::wp(TmM1, ylim.all = 6)
plot(TmM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTmM1_mu <- drop1(TmM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTmM1_mu <-
  dropTmM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Thalassarche melanophris", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("DEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TmM1)*100) %>%
  dplyr::mutate(Model1 = AIC(TmM1)) %>%
  dplyr::mutate(Model2 = AIC(TmM2))

dropTmM1 <- dropTmM1_mu

## Generalized R-squared
gamlss::Rsq(TmM1)

########### Procellaria aequinoctialis

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Procellaria_aequinoctialis_Aves)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Polygon)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Month)
boxplot(dfModel$Procellaria_aequinoctialis_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Procellaria_aequinoctialis_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # GPO BNB ZALG ZIPIG ZAPIG WARING

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","BNB","ZALG","WARING","PO",
         "NBI","NBII","ZINBI","ZANBI","ZIPIG","ZAPIG")

dist.test.Pa <- list()

for(fam in 1:length(fams)) {
  dist.test.Pa[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Procellaria_aequinoctialis_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pa) #lowest AIC GPO

## Modeling "mu" (Model M) using the selected distribution GPO
PaM1 <- gamlss(Procellaria_aequinoctialis_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 200)


## Modeling "mu" and "sigma" (Model MS) using the selected GPO
PaM2 <- update(PaM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(PaM1,PaM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PaM1)
gamlss::wp(PaM1, ylim.all = 6)
plot(PaM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPaM1_mu <- drop1(PaM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPaM1_mu <-
  dropPaM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Procellaria aequinoctialis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PaM1)*100) %>%
  dplyr::mutate(Model1 = AIC(PaM1)) %>%
  dplyr::mutate(Model2 = AIC(PaM2))

dropPaM1 <- dropPaM1_mu

## Generalized R-squared
gamlss::Rsq(PaM1)

########### Fregata magnificens

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Fregata_magnificens_Aves)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Polygon)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Month)
boxplot(dfModel$Fregata_magnificens_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Fregata_magnificens_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # ZALG WARING PIG GPO ZAPIG ZIPIG SICHEL SI BNB ZANBI

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("ZALG","WARING","PIG","GPO","ZAPIG","ZIPIG","SICHEL", 
         "SI","BNB","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Fm <- list()

for(fam in 1:length(fams)) {
  dist.test.Fm[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Fregata_magnificens_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Fm) #lowest AIC SICHEL

## Modeling "mu" (Model M) using the selected distribution SICHEL
FmM1 <- gamlss(Fregata_magnificens_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = SICHEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution SICHEL
FmM2 <- update(FmM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(FmM1,FmM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(FmM2)
gamlss::wp(FmM2, ylim.all = 6)
plot(FmM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropFmM2_mu <- drop1(FmM2) %>%  
  as.data.frame() 

# "sigma"
dropFmM2_si <- drop1(FmM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropFmM2_mu <-
  dropFmM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Fregata magnificens", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(FmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(FmM1)) %>%
  dplyr::mutate(Model2 = AIC(FmM2))

Parameter <- c("Si_none", "Si_month", "Si_polygon")

dropFmM2_si <-
  dropFmM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Fregata magnificens", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(FmM2)*100) %>%
  dplyr::mutate(Model1 = AIC(FmM1)) %>%
  dplyr::mutate(Model2 = AIC(FmM2)) 

dropFmM2 <- rbind(dropFmM2_mu, dropFmM2_si)

## Generalized R-squared
gamlss::Rsq(FmM2)

########### Sula leucogaster

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Sula_leucogaster_Aves)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Polygon)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Month)
boxplot(dfModel$Sula_leucogaster_Aves ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Sula_leucogaster_Aves, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # PIG SICHEL SI ZISICHEL ZASICHEL ZIPIG ZAPIG

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("PIG","SI","SICHEL","ZISICHEL","ZIPIG","ZAPIG","PO","NBI",
         "NBII","ZINBI","ZANBI","ZASICHEL")

dist.test.Sl <- list()

for(fam in 1:length(fams)) {
  dist.test.Sl[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Sula_leucogaster_Aves ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) + 
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Sl) #lowest AIC SICHEL and ZINBI

#---------
# However, when running Model MS, SICHEL did not converged. 
# Thus we used the second lowest GAIC ZINBI, which was less than two 
# units 
#---------

## Modeling "mu" (Model M) using the selected distribution ZINBI
SlM1 <- gamlss(Sula_leucogaster_Aves ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = ZINBI, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution ZINBI             
SlM2 <- update(SlM1, sigma.fo = ~ 
                 cs(Month, df = 3) +  
                 cs(Polygon, df = 7))

AIC(SlM1,SlM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SlM2)
gamlss::wp(SlM2, ylim.all = 6)
plot(SlM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSlM2_mu <- drop1(SlM2) %>%
  as.data.frame() 

# "sigma"
dropSlM2_si <- drop1(SlM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSlM2_mu <-
  dropSlM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Sula leucogaster", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("ZINBI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SlM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SlM1)) %>%
  dplyr::mutate(Model2 = AIC(SlM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSlM2_si <-
  dropSlM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Sula leucogaster", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("ZINBI", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SlM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SlM1)) %>%
  dplyr::mutate(Model2 = AIC(SlM2)) 

dropSlM2 <- rbind(dropSlM2_mu, dropSlM2_si)

## Generalized R-squared 
gamlss::Rsq(SlM2)

#---------------------------------Mammalia

########### Pontoporia blainvillei

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Pontoporia_blainvillei_Mammalia)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Polygon)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Month)
boxplot(dfModel$Pontoporia_blainvillei_Mammalia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Pontoporia_blainvillei_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # GPO ZANBI DEL SICHEL SI WARING

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","DEL","SICHEL","SI","WARING","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Pb <- list()

for(fam in 1:length(fams)) {
  dist.test.Pb[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Pontoporia_blainvillei_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +  
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Pb) #lowest AIC SICHEL

## Modeling "mu" (Model M) using the selected distribution SICHEL
PbM1 <- gamlss(Pontoporia_blainvillei_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) + 
                 re(random = ~1|Year) +
                 offset(log(effort_km)), 
               family = SICHEL, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected SICHEL
PbM2<-update(PbM1, sigma.fo = ~ 
               cs(Month, df = 3) + 
               cs(Polygon, df = 7))

AIC(PbM1,PbM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(PbM1)
gamlss::wp(PbM1, ylim.all = 6)
plot(PbM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropPbM1_mu <- drop1(PbM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropPbM1_mu <-
  dropPbM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Pontoporia blainvillei", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("SICHEL",4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(PbM1)*100) %>%
  dplyr::mutate(Model1 = AIC(PbM1)) %>%
  dplyr::mutate(Model2 = AIC(PbM2))

dropPbM1 <- dropPbM1_mu

## Generalized R-squared
gamlss::Rsq(PbM1)

########### Sotalia guianensis

## Exploratory analyses
par(mfrow = c(1, 1))
hist(dfModel$Sotalia_guianensis_Mammalia)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Polygon)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Month)
boxplot(dfModel$Sotalia_guianensis_Mammalia~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Sotalia_guianensis_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # GPO ZALG ZIPIG ZAPIG BNB NBI NBII

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("GPO","ZALG","ZIPIG","BNB","PO","NBI","NBII", "ZINBI","ZANBI","ZAPIG")

dist.test.Sg <- list()

for(fam in 1:length(fams)) {
  dist.test.Sg[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Sotalia_guianensis_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) + 
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Sg) #lowest AIC GPO

## Modeling "mu" (Model M) using the selected distribution GPO
SgM1 <- gamlss(Sotalia_guianensis_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = GPO, data = dfModel, n.cyc = 100)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution GPO
SgM2 <- update(SgM1, sigma.fo = ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7))

AIC(SgM1,SgM2)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(SgM2)
gamlss::wp(SgM2, ylim.all = 6)
plot(SgM2)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropSgM2_mu <- drop1(SgM2) %>%  #Model with term  cs(Month, df = 3) has failed
  as.data.frame()  

# "sigma"
dropSgM2_si <- drop1(SgM2, what = "sigma") %>%
  as.data.frame() 

# Combine both objects 'dropCcM2_mu' and 'dropCcM2_si' for building table1
Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropSgM2_mu <-
  dropSgM2_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Sotalia guianensis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SgM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SgM1)) %>%
  dplyr::mutate(Model2 = AIC(SgM2))

Parameter <- c("Si_none","Si_month","Si_polygon")

dropSgM2_si <-
  dropSgM2_si %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::add_row() %>%
  dplyr::mutate(Species = rep("Sotalia guianensis", 4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("GPO", 4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(SgM2)*100) %>%
  dplyr::mutate(Model1 = AIC(SgM1)) %>%
  dplyr::mutate(Model2 = AIC(SgM2)) 

dropSgM2 <- rbind(dropSgM2_mu, dropSgM2_si)

## Generalized R-squared
gamlss::Rsq(SgM2)

########### Tursiops trucatus

## Exploratory analyses
par(mfrow = c(2, 2))
hist(dfModel$Tursiops_truncatus_Mammalia)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Polygon)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Month)
boxplot(dfModel$Tursiops_truncatus_Mammalia ~ dfModel$Year, varwidth = TRUE)

## Test all relevant parametric count distributions supported by the GAMLSS 
## in a null model
distFit <- gamlss::fitDist(y = Tursiops_truncatus_Mammalia, 
                           data = dfModel, type = "counts")

distFit$fits # Selected distributions with lowest AIC and 
             # the ones with less than two units of difference: 
             # YULE WARING PIG ZALG GPO

## Testing the selected distributions, including traditional count 
## distributions, using the Model M
fams <- c("PIG","YULE","WARING","ZALG","GPO","PO","NBI","NBII","ZINBI","ZANBI")

dist.test.Tt <- list()

for(fam in 1:length(fams)) {
  dist.test.Tt[[fams[fam]]] <- gamlss::GAIC(
    gamlss::gamlss(Tursiops_truncatus_Mammalia ~ 
                     cs(Month, df = 3) + 
                     cs(Polygon, df = 7) +
                     re(random = ~1|Year) +
                     offset(log(effort_km)), 
                   family = fams[fam], data = dfModel, n.cyc = 200))}

unlist(dist.test.Tt) #lowest AIC PIG

#---------
# When running Model MS, PIG did not converged. 
# Thus we used the second lowest GAIC GPO, which was less than two 
# units. However, when compared model M and MS using GPO with model M using PIG,
# the later had the lowest AIC
#---------

## Modeling "mu" (Model M) using the selected distribution PIG
TtM1 <- gamlss(Tursiops_truncatus_Mammalia ~ 
                 cs(Month, df = 3) + 
                 cs(Polygon, df = 7) +
                 re(random = ~1|Year) + 
                 offset(log(effort_km)), 
               family = PIG, data = dfModel, n.cyc = 200)

## Modeling "mu" and "sigma" (Model MS) using the selected distribution PIG 
TtM2 <- update(TtM1, sigma.fo = ~      # not converged
                 cs(Month, df = 3) +  
                 cs(Polygon, df = 7), 
               n.cyc = 100)

AIC(TtM1)

## Model check using diagnostic plots (QQ- and warm-plots)
par(mfrow = c(1,1))
gamlss::wp(TtM1)
gamlss::wp(TtM1, ylim.all = 6)
plot(TtM1)

## Determine the significance of each smoother through Chi-square test using 
## stats::drop1() function

# "mu"
dropTtM1_mu <- drop1(TtM1) %>%
  as.data.frame() 

Parameter <- c("Mu_none","Mu_month","Mu_polygon","Mu_random_effect")

dropTtM1_mu <-
  dropTtM1_mu %>%
  dplyr::mutate(Parameter = Parameter) %>%
  dplyr::mutate(Species = rep("Tursiops truncatus",4)) %>%
  dplyr::rename(pvalue = c(4)) %>%
  dplyr::select(Species,Parameter,pvalue) %>%
  dplyr::mutate(Distribution = rep("PIG",4)) %>%
  dplyr::mutate("Deviance explained"= Rsq(TtM1)*100) %>%
  dplyr::mutate(Model1 = AIC(TtM1)) %>%
  dplyr::mutate(Model2 = "not_converged")

dropTtM1 <- dropTtM1_mu

## Generalized R-squared
gamlss::Rsq(TtM1) 

##----------------------------------------------------------------------------##
##                             Smothers GAMLSS: Mu                            ##
##-------------------------------Figures 4 and 5------------------------------##
##----------------------------------------------------------------------------##

##
## Per Month
##

tiff("./results/figure4.tiff", height = 170 , width = 170, 
     units = 'mm', res = 400, compression ='lzw')

par(mfrow = c(4, 4),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Caretta caretta", col.term = "black", font.main = 3)
gamlss::term.plot(CmM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Chelonia mydas", col.term = "black", font.main = 3)
gamlss::term.plot(LoM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Lepidochelys olivacea", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Spheniscus magellanicus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(LdM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Larus dominicanus", col.term = "black", font.main = 3)
gamlss::term.plot(SlM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Sula leucogaster", col.term = "black", font.main = 3)
gamlss::term.plot(TcM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Thalassarche chlororhynchos", col.term = "black",
                  font.mai = 3)  
gamlss::term.plot(TmM1, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Thalassarche melanophris", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PaM1, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Procellaria aequinoctialis", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PpM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Puffinus puffinus", col.term = "black", font.main = 3)
gamlss::term.plot(FmM2, what = "mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Fregata magnificens", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(PbM1, what ="mu",ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Pontoporia blainvillei", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SgM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Sotalia guianensis", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(TtM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 1,
                  main = "Tursiops truncatus", col.term = "black", 
                  font.main = 3, sub = "Month", cex.sub = 1.5)

dev.off()

##
##Per polygon
##

tiff("./results/figure5.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(4, 4),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Caretta caretta", col.term = "black", font.main = 3)
gamlss::term.plot(CmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Chelonia mydas", col.term = "black", font.main = 3)
gamlss::term.plot(LoM1, what ="mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Lepidochelys olivacea", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Spheniscus magellanicus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(LdM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Larus dominicanus", col.term = "black", font.main = 3)
gamlss::term.plot(SlM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Sula leucogaster", col.term = "black", font.main = 3)
gamlss::term.plot(TcM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Thalassarche chlororhynchos", col.term = "black",
                  font.main = 3)
gamlss::term.plot(TmM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Thalassarche melanophris", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PaM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Procellaria aequinoctialis", col.term = "black",
                  font.main = 3)
gamlss::term.plot(PpM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Puffinus puffinus", col.term = "black", font.main = 3)
gamlss::term.plot(FmM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Fregata magnificens", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(PbM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Pontoporia blainvillei", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SgM2, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Sotalia guianensis", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(TtM1, what = "mu", ylim = 'free', scheme = 'shaded', term = 2,
                  main = "Tursiops truncatus", col.term = "black", 
                  font.main = 3, sub = "Polygon", cex.sub = 1.5)

dev.off()

##----------------------------------------------------------------------------##
##                           Smothers GAMLSS: Sigma                           ##
##----------------------------------S6 and S7---------------------------------##
##----------------------------------------------------------------------------##

##
## Per Month
##

tiff("./results/figureS6.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(3, 3),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Caretta caretta", col.term = "black",
                  font.main = 3)
gamlss::term.plot(CmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Chelonia mydas", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Spheniscus magellanicus", 
                  col.term = "black", font.main = 3)
gamlss::term.plot(LdM2, what = "sigma", ylim = 'free', scheme = 'shaded', 
                  term = 1, main = "Larus dominicanus", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SlM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Sula leucogaster", col.term = "black", 
                  font.main=3)
gamlss::term.plot(TcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Thalassarche chlororhynchos",
                  col.term = "black", font.main=3)
gamlss::term.plot(PpM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Puffinus puffinus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(FmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 1, main = "Fregata magnificens", col.term = "black",
                  font.main = 3, sub = "Month", cex.sub = 1.5)
gamlss::term.plot(SgM2, what = "sigma", ylim = 'free', scheme = 'shaded', 
                  term = 1, main = "Sotalia guianensis", col.term = "black",
                  font.main = 3)

dev.off()

##
##Per polygon
##

tiff("./results/figureS7.tiff", height = 170 , width = 170, units = 'mm',
     res = 400, compression ='lzw')

par(mfrow = c(3, 3),cex.axis = 0.8,cex.lab = 0.8,cex.main = 1)

gamlss::term.plot(CcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Caretta caretta", col.term = "black",
                  font.main = 3)
gamlss::term.plot(CmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Chelonia mydas", col.term = "black", 
                  font.main = 3)
gamlss::term.plot(SmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Spheniscus magellanicus", 
                  col.term = "black", font.main = 3)
gamlss::term.plot(LdM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Larus dominicanus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(SlM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Sula leucogaster", col.term = "black",
                  font.main = 3)
gamlss::term.plot(TcM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Thalassarche chlororhynchos",
                  col.term = "black", font.main = 3)
gamlss::term.plot(PpM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Puffinus puffinus", col.term = "black",
                  font.main = 3)
gamlss::term.plot(FmM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Fregata magnificens", col.term = "black",
                  cex.sub = 1.5, sub = "Polygon", font.main = 3)
gamlss::term.plot(SgM2, what = "sigma", ylim = 'free', scheme = 'shaded',
                  term = 2, main = "Sotalia guianensis", col.term = "black",
                  font.main = 3)

dev.off()

##----------------------------------------------------------------------------##
##                             Summaries GAMLSS                               ##
##----------------------------------Table 1-----------------------------------##
##----------------------------------------------------------------------------##

## Combine the drop1 objects
options(digits = 2)
dropModels <- rbind.fill(dropCcM2,dropCmM2,dropLoM1,dropSmM2,dropLdM2,dropPpM2,
                         dropTcM2,dropTmM1,dropPaM1,dropFmM2,dropSlM2,dropPbM1,
                         dropSgM2,dropTtM1)
dropModels  <-
  dropModels  %>%
  tidyr::pivot_wider(names_from = c(Parameter), 
                     values_from = pvalue) %>%
  dplyr::relocate(Model1,.after = Distribution) %>%
  dplyr::relocate(Model2,.after = Model1) %>%
  dplyr::select(c(1:5,7,8,11,12)) %>%
  dplyr::mutate_at(c(3:4), as.numeric) %>%
  dplyr::mutate_at(c(6:9), round, digits = 3)

# write.csv2(dropModels,"./results/Table1.1.csv", row.names = FALSE)
