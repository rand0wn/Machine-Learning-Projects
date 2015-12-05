#Spam Classifier using Naive Bayes

#Research Question: What is chance of message being a spam?

install.packages("tm")
install.packages("caret")
install.packages("MASS")
install.packages("klaR")
install.packages("pander")
install.packages("dplyr")
install.packages("doMC")
install.packages("e1071")

# libraries needed by caret
require(klaR)
require(MASS)
# for the Naive Bayes modelling
require(caret)
require(e1071)
# to process the text into a corpus
require(tm)
# to get nice looking tables
require(pander)
# to simplify selections
require(dplyr)
#Multicore
require(doMC)
registerDoMC(cores=4)

# a utility function for % freq tables
frqtab <- function(x, caption) {
  round(100*prop.table(table(x)), 1)
}
# utility function to summarize model comparison results
sumpred <- function(cm) {
  summ <- list(TN=cm$table[1,1],  # true negatives
               TP=cm$table[2,2],  # true positives
               FN=cm$table[1,2],  # false negatives
               FP=cm$table[2,1],  # false positives
               acc=cm$overall["Accuracy"],  # accuracy
               sens=cm$byClass["Sensitivity"],  # sensitivity
               spec=cm$byClass["Specificity"])  # specificity
  lapply(summ, FUN=round, 2)
}

#Datasets

if (!file.exists("smsspamcollection.zip")) {
  download.file(url="http://www.dt.fee.unicamp.br/~tiago/smsspamcollection/smsspamcollection.zip",
                destfile="smsspamcollection.zip", method="curl")
}
sms_raw <- read.table(unz("smsspamcollection.zip","SMSSpamCollection.txt"),
                      header=FALSE, sep="\t", quote="", stringsAsFactors=FALSE)
colnames(sms_raw) <- c("type", "text")
sms_raw$type <- factor(sms_raw$type)
# randomize it a bit
set.seed(12358)
sms_raw <- sms_raw[sample(nrow(sms_raw)),]
str(sms_raw)

#Refactoring Data for Analysis

sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus_clean <- sms_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#Sampling data using caret same as SVM model

train_index <- createDataPartition(sms_raw$type, p=0.75, list=FALSE)
sms_raw_train <- sms_raw[train_index,]
sms_raw_test <- sms_raw[-train_index,]
sms_corpus_clean_train <- sms_corpus_clean[train_index]
sms_corpus_clean_test <- sms_corpus_clean[-train_index]
sms_dtm_train <- sms_dtm[train_index,]
sms_dtm_test <- sms_dtm[-train_index,]

ft_orig <- frqtab(sms_raw$type)
ft_train <- frqtab(sms_raw_train$type)
ft_test <- frqtab(sms_raw_test$type)
ft_df <- as.data.frame(cbind(ft_orig, ft_train, ft_test))
colnames(ft_df) <- c("Original", "Training set", "Test set")
pander(ft_df, style="rmarkdown",
       caption=paste0("Comparison of SMS type frequencies among datasets"))

sms_dict <- findFreqTerms(sms_dtm_train, lowfreq=5)
sms_train <- DocumentTermMatrix(sms_corpus_clean_train, list(dictionary=sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_clean_test, list(dictionary=sms_dict))

# Converting numeric variables to factors: level problem

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("Absent", "Present"))
}
sms_train <- sms_train %>% apply(MARGIN=2, FUN=convert_counts)
sms_test <- sms_test %>% apply(MARGIN=2, FUN=convert_counts)


#Modeling 

#Model 1

ctrl <- trainControl(method="cv", 10)
set.seed(12358)
sms_model1 <- train(sms_train, sms_raw_train$type, method="nb",
                    trControl=ctrl)
sms_model1

#Model 2

set.seed(12358)
sms_model2 <- train(sms_train, sms_raw_train$type, method="nb", 
                    tuneGrid=data.frame(.fL=1, .usekernel=FALSE),
                    trControl=ctrl)
sms_model2

#Testing Models : Confusion Matrix

#Model 1

sms_predict1 <- predict(sms_model1, sms_test)
cm1 <- confusionMatrix(sms_predict1, sms_raw_test$type, positive="spam")
cm1

#Model 2

sms_predict2 <- predict(sms_model2, sms_test)
cm2 <- confusionMatrix(sms_predict2, sms_raw_test$type, positive="spam")
cm2

#Prediction Accuracy

tn=1203
tp=151
fn=32
fp=4
book_example1 <- list(
  TN=tn,
  TP=tp,
  FN=fn,
  FP=fp,
  acc=(tp + tn)/(tp + tn + fp + fn),
  sens=tp/(tp + fn),
  spec=tn/(tn + fp))


tn=1204
tp=152
fn=31
fp=3
book_example2 <- list(
  TN=tn,
  TP=tp,
  FN=fn,
  FP=fp,
  acc=(tp + tn)/(tp + tn + fp + fn),
  sens=tp/(tp + fn),
  spec=tn/(tn + fp))

b1 <- lapply(book_example1, FUN=round, 2)
b2 <- lapply(book_example2, FUN=round, 2)
m1 <- sumpred(cm1)
m2 <- sumpred(cm2)

model_comp <- as.data.frame(rbind(b1, b2, m1, m2))
rownames(model_comp) <- c("Book model 1", "Book model 2", "Caret model 1", "Caret model 2")
pander(model_comp, style="rmarkdown", split.tables=Inf, keep.trailing.zeros=TRUE,
       caption="Model results when comparing predictions and test set")

#Write results into file
my.df <- data.frame(lapply(model_comp, as.character), stringsAsFactors=FALSE) #Convert into 2D data frame

write.csv(my.df, file = "Results.csv")

