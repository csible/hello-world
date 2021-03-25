# K-Nearest Neighbors (K-NN)

# Importing the dataset
data = read.csv('Social_Network_Ads.csv')
data$Purchased = factor(data$Purchased)

# Splitting the dataset into the Training set and Test set
library('caTools')
set.seed(123)
split = sample.split(data$Purchased, SplitRatio = .75) ###split on the target variable
training_set = subset(data, split == TRUE) ###make sure the split makes sense and it's not just a dumb model getting lucky
test_set = subset(data, split == FALSE) 

### if you split into test and validation you might want a 50/50 split
#split = sample.split(data$Purchased, SplitRatio = .50) 
#validation_set = subset(data, split == TRUE) 
#test_set = subset(data, split == FALSE) 


# Feature Scaling ### really matters now, we ignore column 3 because that's what we want to predict
training_set[,-3] = scale(training_set[,-3])
test_set[,-3] = scale(test_set[,-3])

# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
### knn uses one command that fits and predicts

predictions = knn(train = training_set[,-3],
                  test = test_set[,-3],
                  cl = training_set$Purchased, ### target variable from the training set
                  k = 5,
                  prob = TRUE) ### gives you extra information when true


# Making the Confusion Matrix
#install.packages('caret')
library(caret)
confusionMatrix(test_set$Purchased, predictions)


# Visualising the Training set results
#library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

### the lower the k, the crazier the boundary, but you might overfit
### the higher the k, the smoother the boundary, but you might have more misclassifications

# Visualising the Test set results
#library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))