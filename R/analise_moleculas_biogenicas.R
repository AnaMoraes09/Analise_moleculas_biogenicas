#Instalando as bibliotecas necessarias

install.packages('dslabs')
library(dslabs)
install.packages('readr')
library(readr)
install.packages('tidyverse')
library(tidyverse)
data <- read_csv2('biogenic_molecules.csv')
library(dplyr)

#Analises primaria 
class(data)
str(data) #OBS: nosso TPSA não está no type correto 
head(data) 
names(data)

#Observamos na nossa estrutura que o TPSA está como Caracter, não queremos isso.
#portando vamos fazer uma conversão para numerico.

data <- transform(data,
                  TPSA = as.numeric(TPSA))
str(data)

# Análise exploratoria dos dados


##MolWt

summary(data$MolWt)

data$MolecularFormula[which.max(data$MolWt)] #Molecula de maior massa
data$MolecularFormula[which.min(data$MolWt)] #Molecula de menor massa

#Criando um boxplot
boxplot(data$MolWt)

#com filtro 
data %>%
  filter(MolWt < 2*10^12) %>%
  ggplot(aes(x=MolWt)) +
  geom_histogram()


## MolLogp
summary(data$MolLogp)

data$MolecularFormula[which.max(data$MolLogp)] #Molecula de maior LogP
data$MolecularFormula[which.min(data$MolLogp)] #Molecula de menor LogP

##grafico da solubidade em água 
data %>%
  ggplot(aes(x=MolLogp)) + geom_histogram() 


##HAcceptors

summary(data$HAcceptors)

data$MolecularFormula[which.max(data$HAcceptors)] #Molecula que mais aceita H
data$MolecularFormula[which.min(data$HAcceptors)] #Molécula que menos aceita H


##HDonors
summary(data$HDonors)

data$MolecularFormula[data$HDonors == 7] #Moléculas que mais doam H
data$MolecularFormula[data$HDonors == 0] #Moléculas que menos doam H

#grafico dos doadores de elétrons e receptores de elétrons
hist(data$HDonors)
hist(data$HAcceptors)
boxplot(data$HDonors, data$HAcceptors, 
        names = c('eletron donor', 'receptor de eletron'),
        col = c('red', 'blue'))


##RotableBonds

summary(data$RotableBonds)

data$MolecularFormula[which.max(data$RotableBonds)]
data$MolecularFormula[data$RotableBonds == 0]

obrigacao_rotativa_min <- length(data$MolecularFormula[data$RotableBonds == 0])
obrigacao_rotativa_min

ggplot(data = data) + geom_histogram(mapping = aes(x=data$RotableBonds))


##rind_count

summary(data$RindCount)

data$MolecularFormula[data$RindCount == 4]
data$MolecularFormula[data$RindCount == 0]

quantidade_aneis_min <- length(data$MolecularFormula[data$RindCount == 0])
quantidade_aneis_min

ggplot(data = data) + geom_histogram(mapping = aes(x=data$RindCount)) 

#grafico de relação 

ggplot(data = data) + geom_col(mapping = aes(x=data$RotableBonds, y = data$RindCount)) 


##TPSA

#top 5 moleculas mais polares

moleculas_polares <- sort(data$TPSA, decreasing = TRUE)
data$MolecularFormula[moleculas_polares]

cinco_moleculas_polares <- data$MolecularFormula[moleculas_polares[1:5]]
cinco_moleculas_polares

#grafico area_de_superfície_polar
ggplot(data = data) + geom_histogram(mapping = aes(x=data$TPSA)) 

