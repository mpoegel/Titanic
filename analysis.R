# Simple wrapper for the read csv built in
readData <- function(file.name, column.types, missing.types) {
  read.csv(file.name,
           colClasses=column.types,
           na.strings=missing.types)
}

train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c("integer",   # PassengerId
                        "factor",    # Survived
                        "factor",    # Pclass
                        "character", # Name
                        "factor",    # Sex
                        "numeric",   # Age
                        "integer",   # SibSp
                        "integer",   # Parch
                        "character", # Ticket
                        "numeric",   # Fare
                        "character", # Cabin
                        "factor"     # Embarked
                        )
test.column.types <- train.column.types[-2] # copy all except the second column because the
                                            # test data has no survived column

train.raw <- readData(train.data.file, train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(test.data.file, test.column.types, missing.types)
df.infer <- test.raw


# -----------------------------------------------------------------------------
# Data Munging

# create a map to show where the data is missing
require(Amelia)
missmap(df.train, main="Titanic Training Data - Missings Map",
        col=c("yellow", "black"), legend=FALSE)

# bar plot to compare number survived vs number perished
barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Passenger Fate", col="black")
# bar plot to compare the passenger numbers by class
barplot(table(df.train$Pclass),
        names.arg = c("First", "Second", "Third"),
        main = "Passenger Traveling Class", col="firebrick")
# bar plot to compare the gender ratio
barplot(table(df.train$Sex),
        main = "Gender",
        col = "darkviolet")
# histogram of ages
hist(df.train$Age,
     main = "Age",
     xlab = NULL,
     col = "brown")
# bar plot of siblings + spouses
barplot(table(df.train$SibSp),
        main = "Sibling + Spouses Aboard",
        col = "darkblue")
# bar plot of parents + children
barplot(table(df.train$Parch),
        main = "Parents + Children Aboard",
        col = "gray50")
# histogram of the fares paid
hist(df.train$Fare,
     main="Fare",
     xlab = NULL,
     breaks = 50,
     col = "darkgreen")
# bar plot of embarkation location
barplot(table(df.train$Embarked),
        names.arg = c("Cherbourg", "Queenstown", "Southamton"),
        main = "Port of Embarkation",
        col = "sienna")

require("vcd")
# mosaic plot of survival by class
mosaicplot(df.train$Pclass ~ df.train$Survived,
           main = "Passenger Fate by Traveling Class",
           color = TRUE,
           xlab = "Pclass", ylab = "Survived")
# mosaic plot of survival by gender
mosaicplot(df.train$Sex ~ df.train$Survived,
           main = "Passenger Fate by Gender",
           color = TRUE,
           xlab = "Gender", ylab = "Survived")

# box plot of survival by age
boxplot(df.train$Age ~ df.train$Survived,
        main = "Passenger Fate by Age",
        xlab = "Survived", ylab = "Age")
# mosaic plot of survival by port of embarkation
mosaicplot(df.train$Embarked ~ df.train$Survived,
           main = "Passenger Fate by Port of Embarkation",
           color = TRUE,
           xlab = "Embarked", ylab = "Survived")

# function to extract the honorific (title) from the name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start + attr(title.dot.start, "match.length") - 1
  data$Title <- substring(data$Name, title.dot.start + 2, title.comma.end - 1)
  return (data$Title)
}

df.train$Title <- getTitle(df.train)

require(Hmisc)
# examine the the titles as they relate to ages
title.ages.stats <- bystats(df.train$Age, df.train$Title,
                            fun = function(x)c(Mean=mean(x),Median=median(x)))
# extract the titles that have missing ages
titles.na.train <- names(which(title.ages.stats[,2] > 0))
# chop off "ALL"
titles.na.train <- titles.na.train[1:length(titles.na.train)-1]

# wrapper function to impute missing values based on the median
imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which(filter.var == v) ] <- impute(impute.var[
      which( filter.var == v )])
  }
  return (impute.var)
}

# impute the missing ages based on the medians of the titles
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, titles.na.train)

# replace the missing values in the embarked column with 'S'
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

# we conclude that the zero fares are most likely errors (they are not given to babies)
# so we again impute the missing fares base on the median on the passenger's class
df.train$Fare[ which(df.train$Fare == 0) ] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass,
                              as.numeric(levels(df.train$Pclass)))

# factor the titles, adding a new Noble level
df.train$Title <- factor(df.train$Title, c(unique(df.train$Title), "Noble"))
# box plot of the passenger ages by title
boxplot(df.train$Age ~ df.train$Title,
        main = "Passenger Age by Title",
        xlab = "Honorific", ylab = "Age")

# function to reassign titles (to group them together)
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which(data$Title == honorific) ] <- new.title
  }
  return (data$Title)
}

# consolidate the titles
df.train$Title <- changeTitles(df.train, c("Capt", "Col", "Don", "Dr", "Jonkheer",
                                           "Lady", "Major", "Rev", "Sir",
                                           "the Countess"), "Noble")
df.train$Title <- changeTitles(df.train, c("Mme", "Mlle", "Ms"), "Miss")

require(plyr)
# revalue the survived column to the Fate columns with 1s and 0s
df.train$Fate <- df.train$Survived
df.train$Fate <- revalue(df.train$Fate, c("1" = "Survived", "0" = "Perished"))

# engineer a new feature based on the idea of women and childen first
df.train$Boat.dibs <- "No"
df.train$Boat.dibs[ which(df.train$Sex == "female" | df.train$Age < 15)] <- "Yes"
df.train$Boat.dibs <- as.factor(df.train$Boat.dibs)

# engineer a 'family' feature
df.train$Family <- df.train$SibSp + df.train$Parch

# engineer a fare per person in case some of the fares were paid together as families
df.train$Fare.pp <- df.train$Fare / (df.train$Family + 1)

train.keeps <- c("Fate", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin",
                 "Embarked", "Title", "Boat.dibs", "Family", "Fare.pp")
df.train.munged <- df.train[train.keeps]


# -----------------------------------------------------------------------------
# Creating the Models

require(caret)
# split the data into training and testing sets 80/20
set.seed(27)
training.rows <- createDataPartition(df.train.munged$Survived,
                                    p = 0.8, list = FALSE)
train.batch <- df.train.munged[  training.rows, ]
test.batch  <- df.train.munged[ -training.rows, ]

# Logistic Regression Model (Generalized Linear Model)
Titanic.logit.1 <- glm(Fate ~ Sex + Pclass + Age + Embarked + Family + Fare, 
                       data = train.batch, family = binomial("logit"))
Titanic.logit.1.chisq <- pchisq(Titanic.logit.1$null.deviance - Titanic.logit.1$deviance,
                                Titanic.logit.1$df.null - Titanic.logit.1$df.residual)
anova(Titanic.logit.1, test = "Chisq")

Titanic.logit.2 <- glm(Fate ~ Sex + Pclass + Age + Family + Embarked + Fare.pp,
                       data = train.batch, family = binomial("logit"))
anova(Titanic.logit.2, test = "Chisq")

Titanic.logit.3 <- glm(Fate ~ Sex + Pclass + Age + Family + Embarked,
                       data = train.batch, family = binomial("logit"))


# Define a control function to handle optional arguments for train function
#   models to be assesed based on the largest absolute area under the ROC
#   curve
# 3 times 10 fold cross validation
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)
require(pROC)
# generate a new GLM
glm.tune.1 <- train(Fate ~ Sex + Fate + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.1)

#compress the embarked class to just the Southampton Port
glm.tune.2 <- train(Fate ~ Sex + Fate + Age + Family + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.2)

# try adding the Title factor
glm.tune.3 <- train(Fate ~ Sex + Pclass + Title + Age + Family + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.3)

# drop the Age factor and collapse the Title factor
glm.tune.4 <- train(Fate ~ Sex + Pclass + I(Title == "Mr") + I(Title == "Noble")
                      + Family + I(Embarked == "S"),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.4)

# emphasize the idea that young men in third class probably had a hard time 
#   finding their way out
glm.tune.5 <- train(Fate ~ Sex + Pclass + I(Title == "Mr") 
                      + I(Title == "Noble") + Age + Family 
                      + I(Embarked == "S") + I(Title == "Mr" & Pclass == 3),
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
summary(glm.tune.5)


# -----------------------------------------------------------------------------
# Evaluate the Models

require(e1071) # what a name for a library...

# make predictions using the linear model
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)




# -----------------------------------------------------------------------------
# Make Predictions on the Kaggle Test Data

# get the titles
df.infer$Title <- getTitle(df.infer)

# impute the missing age values
df.infer$Title <- changeTitles(df.infer, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.infer$Age <- imputeMedian(df.infer$Age, df.infer$Title, titles.na.test)

# consolidate the titles
df.infer$Title <- changeTitles(df.infer, c("Col", "Dr", "Rev"), "Noble")
df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme"), "Miss")
df.infer$Title <- as.factor(df.infer$Title)

#inpute the missing fares
df.infer$Fare[ which( df.infer$Fare == 0 )] <- NA
df.infer$Fare <- imputeMedian(df.infer$Fare, df.infer$Pclass,
                              as.numeric(levels(df.infer$Pclass)))

# engineer a new feature based on the idea of women and childen first
df.infer$Boat.dibs <- "No"
df.infer$Boat.dibs[ which(df.infer$Sex == "female" | df.infer$Age < 15)] <- "Yes"
df.infer$Boat.dibs <- as.factor(df.infer$Boat.dibs)

# engineer a 'family' feature
df.infer$Family <- df.infer$SibSp + df.infer$Parch

# engineer a fare per person in case some of the fares were paid together as families
df.infer$Fare.pp <- df.infer$Fare / (df.infer$Family + 1)

# grab the data used for the predictions
test.keeps <- train.keeps[-1]
test.keeps <- test.keeps[-13]
pred.these <- df.infer[test.keeps]

# use the logistic regression model to generate the predictions
Survived <- predict(glm.tune.5, newdata = pred.these)
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <-as.data.frame(Survived)
predictions$PassengerId <- df.infer$PassengerId

# write predictions to the csv to submit on Kaggle
write.csv(predictions[, c("PassengerId", "Survived")],
          file = "Titantic_Predictions.csv",
          row.names = FALSE,
          quote = FALSE)

