rm(list=ls(all=T))
setwd("E:/Python")
getwd()
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
#install.packages(x)
lapply(x, require, character.only = TRUE)
Rental = read.csv("day.csv", header = TRUE)
colnames(Rental)
Rental= within(Rental, rm(dteday,instant))
str(Rental)


################ Visualization of the numerical data ###################
Graph_Num = function(NCol) {
  ggplot(Rental)+
    geom_histogram(aes(x=NCol,y=..density..),fill= "grey",bins = 50)+
    geom_density(aes(x=NCol,y=..density..))}

G1 = Graph_Num(Rental$temp)+xlab("temp")
G2 = Graph_Num(Rental$atemp)+xlab("atemp")
G3 = Graph_Num(Rental$hum)+xlab("hum")
G4 = Graph_Num(Rental$windspeed)+xlab("windspeed")
G5 = Graph_Num(Rental$casual)+xlab("casual")
G6 = Graph_Num(Rental$registered)+xlab("registered")
G7 = Graph_Num(Rental$cnt)+xlab("cnt")

install.packages("cowplot")
library(cowplot)
plot_grid(G1, G2, G3, G4, G5, G6, G7, labels = "AUTO")
############ Categorical data visualization ###################

D1 = ggplot(Rental, aes_string(x = Rental$season )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Season") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D2= ggplot(Rental, aes_string(x = Rental$yr )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Year") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D3 = ggplot(Rental, aes_string(x = Rental$mnth )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Month") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D4 = ggplot(Rental, aes_string(x = Rental$holiday )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Holiday") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D5 = ggplot(Rental, aes_string(x = Rental$weekday )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Weekday") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D6= ggplot(Rental, aes_string(x = Rental$workingday )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("Workingday") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

D7 = ggplot(Rental, aes_string(x = Rental$weathersit )) +
  geom_bar(stat="count",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("weathersit") + ylab('Count') + scale_y_continuous(minor_breaks = 10)+geom_text(stat = "count",aes(label=..count..))

plot_grid(D1, D2, D3, D4, D5, D6, D7, labels = "AUTO")

#################################Bivariate Analysis####################

S1 = ggplot(Rental, aes_string(x= Rental$registered, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("Registered")

S2 = ggplot(Rental, aes_string(x= Rental$temp, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("temp")

S3 = ggplot(Rental, aes_string(x= Rental$casual, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("casual")

S4 = ggplot(Rental, aes_string(x= Rental$windspeed, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("windspeed")

S5 = ggplot(Rental, aes_string(x= Rental$hum, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("hum")

S6 = ggplot(Rental, aes_string(x= Rental$atemp, y= Rental$cnt))+geom_point(aes_string(colour = Rental$cnt))+ylab("Count")+xlab("atemp")

plot_grid(S1, S2, S3, S4, S5, S6, labels = "AUTO")

pairs(Rental[8:14])

##################################Missing Values Analysis###############################################

missing_val = data.frame(apply(Rental,2,function(x){sum(is.na(x))}))

missing_val$columns=row.names(missing_val)

missing_val
names(missing_val)[1] =  "Missing_percentage"

missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(Rental)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]

########################### Outlier Analysis ###################################

A1 = ggplot(Rental, aes_string( y = Rental$registered, 
                           fill = Rental$cnt)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks= 10) + 
  guides(fill=FALSE) + theme_bw() + ylab("Registered") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

A2 = ggplot(Rental, aes_string( y = Rental$cnt, 
                           fill = Rental$cnt)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks= 10) + 
  guides(fill=FALSE) + theme_bw() + ylab("Count") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

A3 = ggplot(Rental, aes_string( y = Rental$casual, 
                           fill = Rental$cnt)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks= 10) + 
  guides(fill=FALSE) + theme_bw() + ylab("Casual") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))

plot_grid(A1, A2, A3, labels = "AUTO")


ggplot(Rental, aes_string(x= Rental$cnt, y= Rental$casual))+geom_point(aes_string(colour = Rental$cnt))+ylab("Casual")+xlab("count")

######################### Imputing outlier's ##################################

Rental_Out= Rental
val = Rental_Out$casual[Rental_Out$casual %in% boxplot.stats(Rental_Out$casual)$out]

Rental_Out$casual[Rental_Out$casual %in% val]=NA

summary(Rental_Out$casual)

Rental_Out=knnImputation(Rental_Out ,k=5)

ggplot(Rental_Out, aes_string(x= Rental_Out$cnt, y= Rental_Out$casual))+geom_point(aes_string(colour = Rental_Out$cnt))+ylab("Casual")+xlab("count")

ggplot(Rental_Out, aes_string( y = Rental_Out$casual, 
                               fill = Rental_Out$cnt)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks= 10) + 
  guides(fill=FALSE) + theme_bw() + ylab("Casual") +
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=20))


corrgram(Rental_Out[,c('temp','atemp','hum','windspeed','registered','casual','cnt')], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

c= cor(Rental_Out[,c('temp','atemp','hum','windspeed','registered','casual','cnt')])
#######################Dimensionality reduction#############

Rental_Out = subset(Rental_Out, select = -c(hum, atemp))

######Normality check############
qqnorm(Rental_Out$registered)
qqnorm(Rental_Out$casual)
qqnorm(Rental_Out$windspeed)
qqnorm(Rental_Out$temp)
#######Normalisation################
cnames = c("registered","casual")

for(i in cnames){
  print(i)
  Rental_Out[,i] = (Rental_Out[,i] - min(Rental_Out[,i]))/
    (max(Rental_Out[,i] - min(Rental_Out[,i])))
}   

########################model development ########################

##-- Decision Tree
##-- Random Forest
##-- Linear Regression 

#Divide data into train and test
rmExcept('Rental_Out')
set.seed(2020)
train_index = sample(1:nrow(Rental_Out), 0.8 * nrow(Rental_Out))
train = Rental_Out[train_index,]
test = Rental_Out[-train_index,]


################## Desicion Tree ######################
fit = rpart(cnt ~ ., data = train, method = "anova")

test$perdict_DT = predict(fit, test[,-12])

MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}  

MAPE(test[,12], test$perdict_DT)
install.packages("rpart.plot")
library(rpart.plot)
prp(fit)
rpart.plot(fit)
regr.eval(test[,12], test$perdict_DT, stats = c("mae", "mse", "mape", "rmse"))
#######Tree pruning#######

Full_tree = rpart(cnt ~ ., data = train, method = "anova", control = rpart.control(cp = 0))
rpart.plot(Full_tree,box.palette = "RdBu", digits = -3 )
printcp(Full_tree)
plotcp(Full_tree)
test$prune = predict(Full_tree, test[,-12])
MAPE(test[,12], test$prune)

Full_tree$cptable
ocp = Full_tree$cptable[which.min(Full_tree$cptable[,"xerror"]),"CP"]
ocp
prune_fit = prune(Full_tree, ocp)

rpart.plot(prune_fit,box.palette = "RdBu", digits = -3 )
printcp(prune_fit)
predictions_DT1 = predict(prune_fit, test[,-12])
MAPE(test[,12], predictions_DT1)
###################Random forest######################33
require(randomForest)
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)

test$PredictRF = predict(RF_model, test[,-12])

MAPE(test[,12], test$PredictRF)

RF_model1 = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500, mtry= 6)
test$PredictRF1 = predict(RF_model1, test[,-12])
MAPE(test[,12], test$PredictRF1)
############Liner regression###########
LR= lm(cnt ~ ., train)
summary(LR)
test$LR = predict(LR, test[,-12])
MAPE(test[,12], test$LR)


# Overall Linear regression is best model compared to others 
# Linear regression gives best accuracy and low error rate 
# Accuracy =94.6% in test data 
# MAPE test =5.32 %