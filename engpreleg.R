engpremleg <- read.csv("D:/APU/R programming/engpremleg.csv", header=FALSE)
View(engpremleg)
engpremleg <- read.csv("D:/APU/R programming/engpremleg.csv", header=True)
View(engpremleg)
engpremleg <- read.csv("D:/APU/R programming/engpremleg.csv", header=True)
View(engpremleg)
engpremleg <- read.csv("D:/APU/R programming/engpremleg.csv", header=FALSE)
View(engpremleg)
View(engpremleg)
engpremleg <- read.csv("D:/APU/R programming/engpremleg.csv", header=TRUE)
str(engpremleg)
str(engpremleg)
engpremleg$np <-(engpremleg$Gls - engpremleg$PK)
engpremleg$npG <- (engpremleg$Gls - engpremleg$PK)
engpremleg%>%
select(HomeTeam, FTHG, FTR, HS)%>%
filter(HomeTeam == HomeTeam)%>%
ggplot(aes(reorder(HomeTeam, -FTHG),FTR, fills=HS))
engpremleg%>%
+   select(HomeTeam, FTHG, FTR, HS)%>%
+   filter(HomeTeam == HomeTeam)%>%
+   ggplot(aes(reorder(HomeTeam, -FTHG),FTR, fills=HS))
engpremleg$sub0 <- subset(engpremleg, FTHG > FTAG, select = c("FHR","HS"))
cor(engpremleg[c("HomeTeam","FTHG","FTR")], use = "complete")
cor(engpremleg[c("FTHG","FTR")], use = "complete")
cor(engpremleg[c("HomeTeam","FTHG","FTR")])
cor(engpremleg[c("FTHG","FTR","HS")])
engpremleg$Date <-as.Date(engpremleg$Date,"%d/%m/%")
str(engpremleg)
colMeans(engpremleg[,6])
colMeans(engpremleg[,6:7])
engpremleg$Date <-as.Date(engpremleg$Date,"%d/%m/%y")
str(engpremleg)
engpremleg$Date <-as.Date(engpremleg$Date,"%d/%m/%y")
summary(engpremleg)
total_teams <- sort(unique(as.character(as.factor(engpremleg$AwayTeam))))
toatal_teams
total_teams
total_Referee <- sort(unique(as.character(as.factor(engpremleg$Referee))))
total_Referee
total_HY <- sort(unique(as.character(as.factor(engpremleg$HY))))
total_HY
total_HC <- sort(unique(as.character(as.factor(engpremleg$HC))))
total_HC
cor(engpremleg[c("FTHG","HC")])
cor(engpremleg[c("FTHG","HS")])
engpremleg %>% group(Referee) %>% summarise(appearance=n())
library(tidyverse)
install.packages(tidyverse)
library(rlang)
install.packages("tidyverse")
library(dplyr)
library(dtplyr)
library(dbplyr)
engpremleg %>% group(Referee) %>% summarise(appearance=n())
engpremleg %>% group_by(Referee) %>% summarise(appearance=n())
#created a variable name ept4 to store our new referee data
engpremleg <-engpreleg1 %>% group_by(Referee) %>% summarise(appearance = n())
rf<- ept4$Referee # rf stands for referee
ra<- ept4$appearance #ra stands for number of appearance
#plotting a bar graph of referee against the number of appearance.
barplot(ra, names.arg = rf, xlab ="Referee", ylim = c(0, 20),
ylab ="appearance", col ="green",
main ="number of times a referee has appeared in different games.")
#created a variable name ept4 to store our new referee data
engpremleg <-engpreleg1 %>% group_by(Referee) %>% summarise(appearance = n())
rf<- engpremleg$Referee        # rf stands for referee
ra<- engpremleg$appearance     #ra stands for number of appearance
#plotting a bar graph of referee against the number of appearance.fyuxy
barplot(ra, names.arg = rf, xlab ="Referee", ylim = c(0, 20),
ylab ="appearance", col ="green",
main ="number of times a referee has appeared in different games.")
engpremleg <-engpreleg1 %>% group_by(Referee) %>% summarise(appearance = n())
engpremleg <-engpreleg1 %>% group_by(Referee) %>% summarise(appearance = n())
engpremleg1 <-engpremleg  %>% group_by(Referee) %>% summarise(appearance = n())
ts<- engpremleg$Referee
at<- engpremleg$appearance
ts
ts<- engpremleg1$Referee
ts
at<- engpremleg1$appearance
at
barplot(at, names.arg = ts, xlab ="Name of Referees", ylim = c(0, 25),
+         ylab ="appearance", col ="rainbow",
+         main ="number of appearance.")
barplot(at, names.arg = ts, xlab ="Name of Referees", ylim = c(0, 25),
+         ylab ="appearance", col ="rainbow",
+         main ="number of appearance.")
barplot(at, names.arg = ts, xlab ="Name of Referees", ylim = c(0, 25),
+         ylab ="appearance", col ="rainbow",
+         main ="number of appearance")
barplot(at, names.arg = ts, xlab = "Name of Referees", ylim = c(0, 25),
+         ylab = "appearance", col = "rainbow",
+         main = "number of appearance")
barplot(ra, names.arg = rf, xlab ="Referee", ylim = c(0, 20),
+         ylab ="appearance", col ="green",
+         main ="number of times a referee has appeared in different games.")
engpremleg1 <-engpremleg  %>% group_by(Referee) %>% summarise(appearance = n())
ts<- engpremleg1$Referee        # ts stands for referee
at<- engpremleg1$appearance     #at stands for number of appearances
# plotting a bar graph of referee against the number of appearances.> barplot(at, names.arg = rf, xlab ="Referee", ylim = c(0, 20), # nolint
+         ylab ="appearance", col ="rainbow",
+         main ="number of times a referee appeared for a game.")
# plotting a bar graph of referee against the number of appearances.                             barplot(at, names.arg = rf, xlab = "Referee", ylim = c(0, 20),
+         ylab ="appearance", col ="rainbow",
+         main = "number of times a referee appeared for a game.")
barplot(at, names.arg = ts, xlab ="Referee", ylim = c(0, 20),
+         ylab ="appearance", col ="rainbow",
+         main ="number of times a referee appeared for a game.")
save.image("~/engpremleg.RData")



