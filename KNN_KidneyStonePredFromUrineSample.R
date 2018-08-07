library(caret)

#Data importing and loading

# Data Import
UrineSample <- read.table("D:/Python/MI/urine.csv", sep = ",", header = T)


#Structure of wine dataset
str(UrineSample)

#Divide the dataset into training and testing
set.seed(3033)
intrain <- createDataPartition(y = UrineSample$r, p= 0.7, list = FALSE)
training <- UrineSample[intrain,]
testing <- UrineSample[-intrain,]

#Exploratory Analysis
dim(training); dim(testing);
anyNA(UrineSample)
summary(UrineSample)

#Convert the numerical variables into categorical variables 
training[["r"]] = factor(training[["r"]])

#Tarin the KNN model on our training data 
trctrl <- trainControl(method = "repeatedcv", number = 7, repeats = 3)
set.seed(3333)
knn_fit <- train(r ~., data = training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
knn_fit

#Put your model on to the test dataset
test_pred <- predict(knn_fit, newdata = testing)
test_pred


print(test_pred)
#Plot the confusion matrix and check the classes

test_pred= factor(test_pred)


testing[["r"]] = factor(testing[["r"]])

confusionMatrix(test_pred, testing$r)

