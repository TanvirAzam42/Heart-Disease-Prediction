data <- read.csv("D:/DSprojekt/dsp/heart.csv")
library(caret)
library(rpart)



# Select different columns
data <- data[,c("age", "sex", "cp", "trtbps", "chol", "fbs", "restecg", "thalachh", "exng", "oldpeak", "slp", "caa", "thall", "output")]

# Split the data into training and test sets
set.seed(123)
index <- createDataPartition(data$output, p=0.7, list=FALSE)
train_set <- data[index,]
test_set <- data[-index,]

# Logistic Regression
logit_model <- glm(output ~ ., family=binomial(link='logit'), data=train_set)
summary(logit_model)

# Predict on test set
logit_pred <- predict(logit_model, newdata=test_set, type="response")
logit_pred <- ifelse(logit_pred > 0.5,1,0)

# Decision Tree
tree_model <- rpart(output ~ ., data=train_set, method="class")
summary(tree_model)

# Predict on test set
tree_pred <- predict(tree_model, newdata=test_set, type="class")

# Calculate Accuracy
logit_acc <- sum(diag(confusionMatrix(as.factor(logit_pred), as.factor(test_set$output))$table)) / sum(confusionMatrix(as.factor(logit_pred), as.factor(test_set$output))$table)
tree_acc <- sum(diag(confusionMatrix(as.factor(tree_pred), as.factor(test_set$output))$table)) / sum(confusionMatrix(as.factor(tree_pred), as.factor(test_set$output))$table)

print(paste("Logistic Regression Accuracy: ", logit_acc))
print(paste("Decision Tree Accuracy: ", tree_acc))




#to plot decisiontree-
# Load necessary library
library(rpart)

# Assuming 'data' is your data frame and 'output' is the target variable
fit <- rpart(output ~ age + sex + cp + trtbps + chol + fbs + restecg + thalachh + exng + oldpeak + slp + caa + thall, data = data, method = "class")

# Print the decision tree
printcp(fit)

# Plot the decision tree
plot(fit, uniform=TRUE, main="Decision Tree")
text(fit, use.n=TRUE, all=TRUE, cex=.8)




histogram---


  # Assuming 'data' is your dataframe
  # Load the necessary library
  library(ggplot2)

# Create a histogram for the 'age' column
ggplot(data, aes(x=age)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  labs(title="Histogram for Age", x="Age", y="Frequency")

# Create a histogram for the 'trtbps' column
ggplot(data, aes(x=trtbps)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  labs(title="Histogram for trtbps", x="trtbps", y="Frequency")

# Create a histogram for the 'chol' column
ggplot(data, aes(x=chol)) +
  geom_histogram(binwidth=1, color="black", fill="white") +
  labs(title="Histogram for chol", x="chol", y="Frequency")




frequency-

  # Assuming 'data' is your dataframe
  # Load the necessary library
  library(ggplot2)

# Create a density plot for the 'age' column
ggplot(data, aes(x=age)) +
  geom_density(fill="blue", alpha=0.3) +
  labs(title="Density Plot for Age", x="Age", y="Density")

# Create a density plot for the 'trtbps' column
ggplot(data, aes(x=trtbps)) +
  geom_density(fill="blue", alpha=0.3) +
  labs(title="Density Plot for trtbps", x="trtbps", y="Density")

# Create a density plot for the 'chol' column
ggplot(data, aes(x=chol)) +
  geom_density(fill="blue", alpha=0.3) +
  labs(title="Density Plot for chol", x="chol", y="Density")
