
adult <- read.csv("C:/Users/Pulkit/Desktop/New folder (2)/adult.csv")

df <- unique(adult)
adult1 <- subset(df, select = c(1,2,4,6,7,8,9,10,13,14,15))
sapply(adult1, function(x) sum(is.na(x)))

quantile(adult1$age, probs = seq(0,1,0.05))
adult1 <- subset(adult1, adult1$age<=63)

quantile(adult1$hours.per.week, probs = seq(0,1,0.05))
adult1 <- subset(adult1, adult1$hours.per.week>=20 |adult1$hours.per.week<=60)
ggplot(adult1, aes(x = adult1$age)) + geom_bar()

table(adult1$workclass)
table(adult1$education)
table(adult1$marital.status)
table(adult1$occupation)
table(adult1$relationship)
table(adult1$race)
table(adult1$sex)
table(adult1$Income)

dummy <- data.frame(model.matrix(~adult1$workclass, data = adult1))  
dummy <- dummy[,-1]

dummy1 <- data.frame(model.matrix(~adult1$education, data = adult1))  
dummy1 <- dummy1[,-1]

dummy2 <- data.frame(model.matrix(~adult1$marital.status, data = adult1))  
dummy2 <- dummy2[,-1]

dummy3 <- data.frame(model.matrix(~adult1$occupation, data = adult1))  
dummy3 <- dummy3[,-1]

dummy4 <- data.frame(model.matrix(~adult1$relationship, data = adult1))  
dummy4 <- dummy4[,-1]

dummy5 <- data.frame(model.matrix(~adult1$race, data = adult1))  
dummy5 <- dummy5[,-1]

dummy6 <- data.frame(model.matrix(~adult1$sex, data = adult1))  
dummy6 <- dummy6[,-1]

dummy8 <- data.frame(model.matrix(~adult1$Income, data = adult1))  
dummy8 <- dummy8[,-1]

adult2 <- cbind(adult1[, c(1,9)], dummy8,dummy7,dummy6,dummy5,dummy4,dummy3,dummy2,dummy1,dummy) 

