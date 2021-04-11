library(fscaret)            #201611047 Irem ÖZTÜRK
library(MASS)               #201711006 Mert BAYRAMUSTA
library(dplyr)
library(tidyverse)
library(cluster)    
library(factoextra)
library(fpc)
library(NbClust)
library(dendextend) 
library(mlbench)
library(caret)
library(lattice)

##NOTE FOR THE FUTURE: DATAsu variable means that its our cleaned DATA
##NOTE FOR THE FUTURE2.0 : DATAasko_dtree variable means, our class tree variables from hierarchical cluster dataset
##we choose these names just for fun not for confusion :)

set.seed(12345) #random number generator, which is useful for creating simulations or random objects that can be reproduced.

setwd("C:/Users/Mert/Desktop")
MyDATA <- read.csv(file = "data1.csv", header = TRUE, sep = ",", na.strings = c("","NULL", "PrivacySuppressed", "NA"))
dim(MyDATA)

summary(MyDATA)

class(MyDATA)

MyDATA$OPEID<-as.numeric(MyDATA$OPEID)
MyDATA$OPEID6<-as.numeric(MyDATA$OPEID6)


for(i in 1:5){ #removing all characther attributes they are kind of useless.
  MyDATA <- MyDATA[-i] 
  
} 

DATAsu <- MyDATA[rowMeans(is.na(MyDATA))<.8,] #cleaning NA datas more than %80 in rows
DATAsu <- DATAsu[,colMeans(is.na(DATAsu)) < 0.2] #cleaning NA datas more than %80 in columns 


#NA's replaced with the mean of the column
for(i in 1:ncol(DATAsu)){
  DATAsu[is.na(DATAsu[,i]), i] <- mean(DATAsu[, i], na.rm = TRUE)
}

duplicated(DATAsu) #looking if there is a duplicated data
which(duplicated(DATAsu)) #double cheching the non-duplicated result

#OPEID AND INSTNM still remains in the data frame didn't deleted at first so we deleted it again
DATAsu$OPEID <- NULL
DATAsu$INSTNM <- NULL

for(i in 1:61){  #from first attribute to the last attribute scatterplot graphs
  plot(DATAsu[,i], main = names(DATAsu)[i])
  
}

#Whole attributes turned into numeric...
DATAsu[] <- lapply(DATAsu, function(x){
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(DATAsu, class)

#CORRELATION PART
correlationMatrix <- cor(DATAsu[,1:61])
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.75) #found the attributes that is highly correlated
print(highlyCorrelated) #printing highly correlated attributes because highly correlated = not good for calculating clusters
str(DATAsu)
#removing highly correlated attributes
DataCorrelated<-DATAsu[,-c(highlyCorrelated)]
str(DataCorrelated)

#removing outliers from out attributes  (OUTLIER PART)
for(i in 1:15) {
  testdata<-DataCorrelated[,i] #all attributes enteres this step
  
  #first we can see our boxplot WITH outliers
  b<-boxplot(testdata, main=names(DataCorrelated)[i],vertical = T)  
  
  #find extremes from the boxplot's stats output(which is lowerwhisker, Q1, Median, Q3, upperwhisker)
  lowerwhisker<-b$stats[1]
  upperwhisker<-b$stats[5]

  lh <- quantile(testdata,probs=0.25)	
  uh <- quantile(testdata,probs=0.75)	
  step<- 1.5 * (uh-lh)
  #remove the extremes
  testdata<-testdata[testdata>lowerwhisker & testdata<(upperwhisker-(1.5 * step))]
  #now, we can see our boxplot/plot/histogram/barplot WITHOUT outliers(also colored with our favourite color)
  par(mfrow = c(1, 4))
  boxplot(testdata, col = 6, main=names(DataCorrelated)[i],vertical = T)
  plot(density(testdata),main=names(DataCorrelated)[i])
  hist(DataCorrelated[,i], main=names(DataCorrelated)[i])
  barplot(table(DataCorrelated[,i]), main=names(DataCorrelated)[i])
}
#Mosaic plot but for our correlated data set
mosaicplot(DataCorrelated, shade=TRUE, legend=TRUE)


#To show scatter plot with chosen 2 variables 
library(ggplot2)

ggplot(DataCorrelated, aes(x=DataCorrelated$AGEGE24, y=DataCorrelated$PCT_BLACK)) + 
  geom_point()

#------CLUSTER PART------

library(factoextra)
library(cluster)

#VISUALIZING CLUSTER ALGORITHMS
#K-MEANS
#(Note for ourselves, wss means, for total within sum of square)
fviz_nbclust(DataCorrelated,kmeans, method = c( "wss"))+
  labs(title= "K-means") #4 is to optional one because we see a pretty clear elbow at k = 4, indicating that 4 is the best number of clusters.

#HIERARCHICAL 
fviz_nbclust(DataCorrelated,hcut, method = "wss")+
  labs(title= "Hierarchical") #3 is to optional one because we see a pretty clear elbow at k = 3, indicating that 3 is the best number of clusters.

#Compute and visualize k-means clustering
k_means_Data = DataCorrelated
k_means <- kmeans(k_means_Data, 4, nstart = 20) #4 clustering and 20 different random starting assignments 
aggregate(k_means_Data, by = list(DataCorrelated), FUN = mean)
k_means_Data <- data.frame(k_means_Data, k_means$cluster)
#Visualize
fviz_cluster(k_means, data = k_means_Data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#Compute and visualize Hierarchical Agglomerative clustering
Aggr_Data = DataCorrelated
dendogram=hclust(dist(Aggr_Data, method='euclidean'), method = 'ward.D')
#Visualize
plot(dendogram, #graph of dendogram 
     main=paste('plot'), #name of the plot
     xlab='x',  #x label
     ylab='y')  #ylabel
optimal=hclust(dist(Aggr_Data, method='euclidean'), method = 'ward.D')
clust=cutree(optimal, 3) #3 clusters
clusplot(Aggr_Data,
         clust,
         line=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE,
         main=paste('plot'))
Aggr_Data$Aggrcluster_labels <- clust

#Compute and visualize PAM(Partitioning Around Medoids)
Pam_Data = DataCorrelated
pamDat <- pam(Pam_Data, 4, metric = "euclidean", stand = FALSE)
aggregate(Pam_Data, by = list(DataCorrelated), FUN = mean)
Pam_Data <- data.frame(Pam_Data, pamDat$cluster)
#Visualize
fviz_cluster(pamDat, data = Pam_Data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

#CLVALID PART(to decide which cluster algorithm works the best for dataset)

library(clValid)
clmethods <- c("kmeans","pam","hierarchical")
intern <- clValid(DataCorrelated, nClust = 4:10, 
                  clMethods = clmethods, validation = "internal", maxitems = 6000)
summary(intern)


#------CLASSIFICATION PART------

classData = Aggr_Data
head(classData)
#creating both the training and testing data sets to develop a model.
inTrain = createDataPartition(y = classData$Aggrcluster_labels, p = .75, list = FALSE)
training = classData[inTrain,]
testing = classData[-inTrain,] 

#Classification with RPART

library(rpart)
myClass <- Aggrcluster_labels ~ AGEGE24 + PCT_BLACK + PCT_HISPANIC + PCT_GRAD_PROF + PCT_BORN_US + MEDIAN_HH_INC + UNEMP_RATE + SD_EARN_WNE_P10 + PCT75_EARN_WNE_P6 + COUNT_WNE_MALE0_P6 + PCT10_EARN_WNE_P8 + COUNT_NWNE_P9 + GT_25K_P9
DATAasko_dtree <- rpart(myClass, data = training, method="class")
summary(DATAasko_dtree)
# check the prediction
pred<-predict(DATAasko_dtree, training[, 1:15], type="class")
table(training$Aggrcluster_labels,pred)
confusionMatrix(table(training$Aggrcluster_labels,pred))
# plot tree 
plot(DATAasko_dtree, main="Classification Tree ")
text(DATAasko_dtree, use.n=TRUE, all=TRUE, cex=.7)
# predict on test data
testPred <- predict(DATAasko_dtree, testing[, 1:15],type="class")
table(testing$Aggrcluster_labels,testPred)

#classification with KNN

#cross validation 10 and k = 5
ctrl <- trainControl(method="repeatedcv",repeats = 5)
knnFit <- train(Aggrcluster_labels ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit #Output of kNN fit
knnPredict <- predict(knnFit,newdata = testing )
#Get the confusion matrix to see accuracy value and other parameter values
cmKNN = table(knnPredict, testing$Aggrcluster_labels ) #we are doing this because data and reference are not factors with the same levels, so normal confusionMAtrix method doesn't work
accuracyKNN = (sum(diag(cmKNN)))/sum(cmKNN)
accuracyKNN

#classification with NEURAL NETWORKS

NNModel <- train(Aggrcluster_labels ~ ., data = training, method = "nnet",preProcess=c("scale","center"), na.action = na.omit)
NNPredictions <-predict(NNModel, testing)
cmNN <-table(NNPredictions, testing$Aggrcluster_labels)
accuracyNN = (sum(diag(cmNN)))/sum(cmNN)
accuracyNN

#Writing to a csv
write.csv(classData, "C:/Users/Mert/Desktop/CENG464-ProjectGroup#-FinalProject-DataFile.csv", row.names=F)

