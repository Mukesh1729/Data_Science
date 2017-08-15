# Titanic: Machine Learning from Disaster

train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
str(train)
head(train)
tail(train)

test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)
str(test)
head(test)
tail(test)

train$IsTrainSet <- TRUE
test$IsTrainSet <- FALSE

test$Survived <- NA

data <- rbind(train, test)

data[data$Embarked == '', 'Embarked'] <- 'S'
table(is.na(data$Age))

# Clean missing values of age
age.median <- median(data$Age, na.rm = TRUE)
data[is.na(data$Age), "Age"] <- age.median

# Clean missing values of fare
# Fare medians are commented to improve the accurcy of the linear regression model
# fare.median <- median(data$Fare, na.rm = TRUE)
# data[is.na(data$Fare), "Fare"] <- fare.median

boxplot(data$Fare)
boxplot.stats(data$Fare)
boxplot.stats(data$Fare)$stats

upper.whisker <- boxplot.stats(data$Fare)$stats[5]
outlier.filter <- data$Fare < upper.whisker

# Linear Regression Model to improve the accuracy.
fare.expr = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(formula = fare.expr, data = data[outlier.filter,])

fare.row <- data[is.na(data$Fare), c("Pclass", "Sex", "Age", " SibSp", "Parch", "Embarked")]

fare.prediction <- predict(fare.model, newdata = fare.row)
data[is.na(data$Fare,"Fare")] <- fare.prediction

# Categorical casting
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)

# Split dataset back out into train and test
train <- data[data$IsTrainSet == TRUE,]
test <- data[data$IsTrainSet == FALSE,]

train$Survived <- as.factor(train$Survived)

survived.expr <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked" 
Survived.formula <- as.formula(survived.expr)

# Simple Classsification Model
#install.packages("randomForest")
library(randomForest)

titanic.model <- randomForest(formula = Survived.formula, data = train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked" 

Survived <- predict(model, newdata = test)
Survived

PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
tail(output.df)

# Write output data in the file
write.csv(output.df, file = "Titanic_Sub.csv", row.names = FALSE)



