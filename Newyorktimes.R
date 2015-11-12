NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

NewsTrain$Weekday = NewsTrain$PubDate$wday
NewsTest$Weekday = NewsTest$PubDate$wday

library(tm)
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = paste0("H", colnames(HeadlineWords))

CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, stopwords("english"))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)
dtm = DocumentTermMatrix(CorpusAbstract)
sparse = removeSparseTerms(dtm, 0.99)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = paste0("A", colnames(AbstractWords))

newdataframe= cbind(HeadlineWords,AbstractWords)
Train = head(newdataframe, nrow(NewsTrain))
Test = tail(newdataframe, nrow(NewsTest))


#### POPULAR AS FACTOR#######
Train$Popular = factor(NewsTrain$Popular)

Train$WordCount = log(NewsTrain$WordCount+1)
Test$WordCount = log(NewsTest$WordCount+1)

Train$NewsDesk = factor(NewsTrain$NewsDesk)
Test$NewsDesk = factor(NewsTest$NewsDesk)

Train$SectionName =factor(NewsTrain$SectionName)
Test$SectionName = factor(NewsTest$SectionName)

Train$SubsectionName = factor(NewsTrain$SubsectionName)
Test$SubsectionName = factor(NewsTest$SubsectionName)

Train$Weekday =NewsTrain$Weekday
Test$Weekday = NewsTest$Weekday


#### SAME LEVELS IN FACTORS #######
Test$NewsDesk <- factor(Test$NewsDesk, levels=levels(Train$NewsDesk))
Test$SectionName <- factor(Test$SectionName, levels=levels(Train$SectionName))
Test$SubsectionName <- factor(Test$SubsectionName, levels=levels(Train$SubsectionName))

library(randomForest)
######TIPE = PROB#######
StevensForest = randomForest(Popular ~ ., data = Train, ntree=200, nodesize=20)
PredictForest = predict(StevensForest, newdata = Test, type="prob")[,2]

MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredictForest)
write.csv(MySubmission, "Submission.csv", row.names=FALSE)
