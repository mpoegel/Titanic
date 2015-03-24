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
                        "factor"    # Embarked
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




