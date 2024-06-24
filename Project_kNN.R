library(dplyr)
library(class)
library(caret)
library(corrplot)

#Need to change data import
df <- read.csv("C:/Users/brett/OneDrive - University Of Houston/4323 Data Science and Stats Learn/diabetes_dataset__2019.csv",
               sep=',')

#Converting categorical variables into numeric values
#All columns are already numeric, binary, or ordered.
df$Age[df$Age=="less than 40"]=0
df$Age[df$Age=="40-49"]=1
df$Age[df$Age=="50-59"]=2
df$Age[df$Age=="60 or older"]=3

df$Gender[df$Gender=="Male"]=0
df$Gender[df$Gender=="Female"]=1

df$Family_Diabetes[df$Family_Diabetes=="no"]=0
df$Family_Diabetes[df$Family_Diabetes=="yes"]=1

df$highBP[df$highBP=="no"]=0
df$highBP[df$highBP=="yes"]=1

df$PhysicallyActive[df$PhysicallyActive=="none"]=0
df$PhysicallyActive[df$PhysicallyActive=="less than half an hr"]=1
df$PhysicallyActive[df$PhysicallyActive=="more than half an hr"]=2
df$PhysicallyActive[df$PhysicallyActive=="one hr or more"]=3

df$Smoking[df$Smoking=="no"]=0
df$Smoking[df$Smoking=="yes"]=1

df$Alcohol[df$Alcohol=="no"]=0
df$Alcohol[df$Alcohol=="yes"]=1

df$RegularMedicine[df$RegularMedicine=="no"]=0
df$RegularMedicine[df$RegularMedicine=="yes"]=1

df$JunkFood[df$JunkFood=="occasionally"]=0
df$JunkFood[df$JunkFood=="often"]=1
df$JunkFood[df$JunkFood=="very often"]=2
df$JunkFood[df$JunkFood=="always"]=3

df$Stress[df$Stress=="not at all"]=0
df$Stress[df$Stress=="sometimes"]=1
df$Stress[df$Stress=="very often"]=2
df$Stress[df$Stress=="always"]=3

df$BPLevel[df$BPLevel=="low"]=0
df$BPLevel[df$BPLevel=="Low"]=0
df$BPLevel[df$BPLevel=="normal "]=1
df$BPLevel[df$BPLevel=="normal"]=1
df$BPLevel[df$BPLevel=="high"]=2
df$BPLevel[df$BPLevel=="High"]=2

df$Pregancies=NULL
df$Pdiabetes=NULL

df$UriationFreq[df$UriationFreq=="not much"]=0
df$UriationFreq[df$UriationFreq=="quite often"]=1

df$Diabetic[df$Diabetic=="no"]=0
df$Diabetic[df$Diabetic==" no"]=0
df$Diabetic[df$Diabetic=="yes"]=1


for(x in 1:16){
  df[,x] <- as.integer(df[,x])
}
df <- na.omit(df)
summary(df)

res<- cor(df)
corrplot(res)
df<-df[,-c(4,7,10)]
res<- cor(df)
corrplot(res)


#LOOCV model 
x.train <- df[,-16]
y.train <- df[,"Diabetic"]

mn <- {}
for(i in 1:20){
  knn.pred <- knn.cv(train = x.train,
                    cl = y.train,
                    k=i)
  mn <- append(mn, mean(knn.pred != y.train))
}
plot(mn,type="o",xlab="# of K", ylab="Test Error")
knn.pred <- knn.cv(train = x.train,
                   cl = y.train,
                   k=2)
print(mean(knn.pred != y.train))

#K-folds
df$Diabetic[df$Diabetic==0]="no"
df$Diabetic[df$Diabetic==1]="yes"
train.control <- trainControl(method = "CV", 10)
train(Diabetic ~.,
      method = "knn",
      tuneGrid = expand.grid(k = 1:20),
      trControl = train.control,
      metric = "Accuracy",
      data = df)










