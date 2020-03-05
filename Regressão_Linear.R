getwd()
setwd("C:\\Users\\dB Predictor\\Desktop\\Vanessa")

?read.csv()
kickstarter.data <- read.csv("ks-projects-201801-x.csv",header=T,sep=",")
kickstarter.data

install.packages("ggplot2")
library("ggplot2")

install.packages("dplyr")
library("dplyr")

summary(kickstarter.data)
names(kickstarter.data)

###Identificar uma situação problema
###Selecionar ou construir uma base de dados á escolha da dupla
###Identificar a variável a ser modelada

#Para o trbaalho intermediário de estatísticas escolheu-se estudar a base de dadso de projetos do KickStarter disponível em (https://www.kaggle.com/kemical/kickstarter-projects#ks-projects-201801.csv)
#A varíavel a ser modelada é a condição de sucesso ou fracasso do projeto

###Identificação e classificação das variáveis
#ID = qualitativa nominal
#name = qualitativa nominal
#category = qualitativa nominal
#main_cateogry = qualitativa nominal
#currency = quantitativa discreta
#deadline = qualitativa ordinal
#goal = quantitativa discreta
#launched = qualitativa ordinal
#pledged = quantitativa ordinal
#state = qualitativa ordinal
#backers = quantitativa discreta
#country = qualitativa nominal
#usd.pledged = quantitativa ordinal
#usd_pledged_real = quantitativa ordinal
#usd_goal_real = quantitiativa ordinal

###Análise descritiva
?geom_histogram()
?geom_boxplot()
?by()
maincategory.hist <- ggplot(kickstarter.data, aes(main_category))
maincategory.hist + geom_histogram(stat="count")

goal.hist <- ggplot(kickstarter.data,aes(goal))
goal.hist + geom_histogram()
goal.boxplot <- ggplot(kickstarter.data,aes(,goal))
goal.boxplot + geom_boxplot()

usdpledged.hist <- ggplot(kickstarter.data,aes(usd.pledged))
usdpledged.hist + geom_histogram()

currency.hist <- ggplot(kickstarter.data, aes(currency))
currency.hist + geom_histogram(stat="count")

state.hist <- ggplot(kickstarter.data, aes(state))
state.hist + geom_histogram(stat="count")

backers.hist <- ggplot(kickstarter.data,aes(backers))
backers.hist + geom_histogram()

country.hist <- ggplot(kickstarter.data,aes(country))
country.hist + geom_histogram(stat="count")

#Modelo final ajustado e conclusões
#?levels()
#levels(kickstarter.data$state) <- c(0,1,2,3,4,5)

?subset()
successful <- subset(kickstarter.data,kickstarter.data$state=="successful")
ggplot(successful,aes(x=main_category,y=state)) + geom_bar(stat="identity")
ggplot(successful,aes(x=country,y=state)) + geom_bar(stat="identity")

plot(successful$backers,successful$usd.pledged)
plot(successful$goal,successful$usd.pledged)

cor(successful$backers,successful$usd.pledged)
cor.test(successful$backers,successful$usd.pledged)
modelo1 <- lm(successful$backers~successful$usd.pledged,successful)
summary(modelo1)
plot(modelo1)


