library(ggplot2)
#install.packages('cowplot')
library(cowplot)

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
head(data)
colnames(data) <- c("age", "sex", "chest_pain", "rest_bp", "chol", "fasting_bloodsugar", "restingecg", 
                    "maxhr", "exang", "STdep", "slope", "majorvessels", "thalium", "heartdisease")
head(data)
str(data)
data[data == "?"] <- NA

data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"
data$sex <- as.factor(data$sex)

data$chest_pain <- as.factor(data$chest_pain)
data$fasting_bloodsugar <- as.factor(data$fasting_bloodsugar)
data$restingecg <- as.factor(data$restingecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$majorvessels <- as.integer(data$majorvessels)
data$majorvessels <- as.factor(data$majorvessels)

data$thalium <- as.integer(data$thalium)
data$thalium <- as.factor(data$thalium)

data$heartdisease <- ifelse(test=data$heartdisease == 0, yes="Healthy", no="Unhealthy")
data$heartdisease <- as.factor(data$heartdisease)

str(data)

nrow(data[is.na(data$majorvessels) | is.na(data$thalium),])
data[is.na(data$majorvessels) | is.na(data$thalium),]

nrow(data)
data <- data[!(is.na(data$majorvessels) | is.na(data$thalium)), ]
nrow(data)

xtabs(~ heartdisease + sex, data=data)
xtabs(~ heartdisease + chest_pain, data=data)
xtabs(~ heartdisease + fasting_bloodsugar, data=data)
xtabs(~ heartdisease + restingecg, data=data)
xtabs(~ heartdisease + exang, data=data)
xtabs(~ heartdisease + slope, data=data)
xtabs(~ heartdisease + majorvessels, data=data)
xtabs(~ heartdisease + thalium, data=data)
xtabs(~ heartdisease + sex, data=data)

logistic <- glm(heartdisease ~ sex, data = data, family = "binomial")
summary(logistic)

predicted.data <- data.frame(
  probability.of.heartdisease=logistic$fitted.values,
  sex=data$sex)


ggplot(data=predicted.data, aes(x=sex, y=probability.of.heartdisease)) +
  geom_point(aes(color=sex), size=5) +
  xlab("Sex") +
  ylab("Predicted probability of getting heart disease")

xtabs(~ probability.of.heartdisease + sex, data=predicted.data)


logistic <- glm(heartdisease ~ ., data=data, family="binomial")
summary(logistic)



