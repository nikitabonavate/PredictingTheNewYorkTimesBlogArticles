
traindata = read.csv("C:/Users/Nikita/Downloads/Rossman/train.csv",stringsAsFactors = T)
testdata = read.csv("C:/Users/Nikita/Downloads/Rossman/test.csv",stringsAsFactors = T)
storedata = read.csv("C:/Users/Nikita/Downloads/Rossman/store.csv",stringsAsFactors = T)

#Visualise the data 
str(traindata)
head(traindata, 10)
str(testdata)
head(testdata, 10)
str(storedata)
head(storedata,10)

#Removing the customer variable from the traindataing data set
traindata$Customers= NULL
traindata$Id= 0
testdata$Sales= 0

##as very few variables are numeric, we will check correlation individually.. 
str(traindata)
cor(traindata$DayOfWeek,traindata$Sales,use = "complete.obs")


#Merge all three files 
traindata$traindataFlag <- 1
testdata$traindataFlag <- 0
mergedata <- rbind(traindata,testdata)
Rossman <- merge(mergedata,storedata, by.x="Store",by.y = "Store")
str(Rossman)
head(Rossman,10)


## Convert variables into the factors
Rossman$Open <- as.factor(Rossman$Open)
Rossman$Promo <- as.factor(Rossman$Promo)
Rossman$SchoolHoliday <- as.factor(Rossman$SchoolHoliday)


#Check out for missing values 
length(Rossman[is.na(Rossman)])
Rossman[is.na(Rossman)] = 0


###Imputation of some of the variables 

Rossman$CompetitionOpenSinceMonth[Rossman$CompetitionOpenSinceMonth==0] = mean(Rossman$CompetitionOpenSinceMonth, na.rm=TRUE)
Rossman$CompetitionOpenSinceYear[Rossman$CompetitionOpenSinceYear==0] = mean(Rossman$CompetitionOpenSinceYear, na.rm=TRUE)
Rossman$Promo2SinceWeek[Rossman$Promo2SinceWeek==0] = mean(Rossman$Promo2SinceWeek, na.rm=TRUE)
Rossman$Promo2SinceYear[Rossman$Promo2SinceYear==0] = mean(Rossman$Promo2SinceYear, na.rm=TRUE)

str(Rossman)
#Extract values from the data variable
Rossman$Date= as.Date.factor(Rossman$Date,"%Y-%m-%d")
Rossman$month = as.integer(format(Rossman$Date, "%m"))
Rossman$year = as.integer(format(Rossman$Date, "%Y"))
Rossman$day = as.integer(format(Rossman$Date, "%d"))



cor(Rossman$DayOfWeek,Rossman$Sales)
# removing the date column since elements are extracted
Rossman$Date = NULL






str(Rossman)
#install.packages("Metrics")
library(Metrics)
Rossman= model.matrix(~0+.,Rossman)
Rossman= as.data.frame(Rossman)
str(Rossman)


###Lets divide the data again into train and test set

train <- Rossman[Rossman$traindataFlag==1,]
test <- Rossman[Rossman$traindataFlag==0,]

train$traindataFlag= NULL
test$traindataFlag= NULL

str(train)
Model1 = lm(Sales~., data=train)

summary(Model1)
str(Rossman)

PredTest = predict(Model1, newdata = test)

submission = data.frame(Id=test$Id, Sales=PredTest)
write.csv(submission, "Reg1.csv")


submission_file <- test[,c("Sales")]
write.csv(submission_file, "Reg1.csv")



mtrain = mtrain[, -c(19:21)]
mtest = mtest[, -c(19:21)]


library(randomForest)
RF2 = randomForest(Sales ~ ., data = train, ntree=170, imp=TRUE, sampsize=10000, do.trace=TRUE)







