
adult <- read.csv("C:/Users/Pulkit/Desktop/New folder (2)/adult.csv")      #reading dataset

df <- unique(adult)                                      #extracting unique rows from dataset
adult1 <- subset(df, select = c(1,5,6,7,8,9,10,13,15))   #selecting columns as per the requirement
sum(is.na(adult1))                                       #counting NA values in dataset
sapply(adult1, function(x) sum(is.na(x)))                #counting NA values column wise

quantile(adult1$age, probs = seq(0,1,0.05))              #checking quantile of age variable
adult1 <- subset(adult1, adult1$age<=63)                 #removing outliers 

quantile(adult1$hours.per.week, probs = seq(0,1,0.05))   
adult1 <- subset(adult1, adult1$hours.per.week>=20 |adult1$hours.per.week<=60)
ggplot(adult1, aes(x = adult1$age)) + geom_bar()

table(adult1$workclass)                     #analysing the content of variables
table(adult1$education.num)
table(adult1$marital.status)
table(adult1$occupation)
table(adult1$relationship)
table(adult1$race)
table(adult1$sex)
table(adult1$Income)

dummy <- data.frame(model.matrix(~adult1$workclass, data = adult1))   #creating dummy variables of categorical variables
dummy <- dummy[,-1]

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

adult2 <- cbind(adult1[, c(1,9)], dummy8,dummy6,dummy5,dummy4,dummy3,dummy2,dummy)  #creating dataset with dummy variables and integer variables

clus <- kmeans(adult2, centers = 4, nstart = 50)   #creating 4 clusters throught k means
str(clus)       #checking structure of the clusters

r_sq<- rnorm(20)

for (number in 1:20)                                        #calculation for checking optimum no. of clusters
{clus <- kmeans(adult2, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss }

plot(r_sq)                   #plot to check the no. of clusters suitable as per data

adult3 <- cbind(adult2, clus$cluster)          #creating dataset with cluster no.
colnames(adult3)[46] <- "ClusterID"

adult_cluster <- group_by(adult3, ClusterID)
library("dplyr")

tab <- summarise(adult_cluster, mean(age))           #checking mean age of each cluster
tab1 <- summarise(adult_cluster, mean(hours.per.week))   #checking mean hours per week of each cluster

table(adult_cluster$age, adult_cluster$ClusterID)                   #creating table of age vs cluster no.
table(adult_cluster$ClusterID, adult_cluster$hours.per.week)        #creating table of cluster no. vs hours per week
table(adult_cluster$ClusterID, adult_cluster$adult1.sex.Male)       #creating table of cluster no. vs male
plot(adult_cluster[c("age", "hours.per.week")], col=adult_cluster$ClusterID)   #plotting age vs hours per week where colour represents cluster
plot(adult_cluster[c("age", "ClusterID")], col=adult_cluster$ClusterID)

plot(adult_cluster[c("hours.per.week", "ClusterID")], col=adult_cluster$ClusterID)
--------------
set.seed(100)

index = sample(1:nrow(adult1),0.2*nrow(adult1))
train = adult1[index,]
test = adult1[-index,]
adult_dist1 <- dist(train)                  #creating dataset with 20% values of original dataset
adult_h1 <- hclust(adult_dist1, method="complete")  #performing hierarchical clustering with complete method
plot(adult_h1)                                    #plotting dendogram

rect.hclust(adult_h1, h=110, border = "red")        
clusterc <- cutree(adult_h1,k=5)                   #cutting the dendogram at height 5
adult_hc <- cbind(train, clusterc)                 #merging cluster no. with dataset
colnames(adult_hc)[12] <- "ClusterID"              
adult_cluster1 <- group_by(adult_hc, ClusterID)
tab2 <- summarise(adult_cluster1, mean(age), mean(hours.per.week))            #creating table of cluster no. with mean age and mean hours per week

table(adult_cluster1$age, adult_cluster1$ClusterID)
table(adult_cluster1$ClusterID, adult_cluster1$hours.per.week)
table(adult_cluster1$ClusterID, adult_cluster1$sex)

plot(adult_cluster1[c("age", "hours.per.week")], col=adult_cluster1$ClusterID) #plotting age vs hours per week where colour represents cluster
