#saving the Wisconsin breast cancer data to the wbcd data frame

wbcd <- read.csv("C:/Users/vignesht/Downloads/wisc_bc_data (1).csv")

#Using the str(wbcd) command, we can confirm that the data is structured with  569 examples and 32 features as we expected.

wbcd

#The first variable is an integer variable named id. 
#As this is simply a unique identifier (ID) for each patient in the data, it does not provide useful information, and we will need to exclude it from the model.

wbcd <- wbcd[-1]

table(wbcd$diagnosis)

wbcd$diagnosis<- factor(wbcd$diagnosis, levels = c("B", "M"),        labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
#Transformation - normalizing numeric data

normalize <- function(x) {       return ((x - min(x)) / (max(x) - min(x))) } 

#check normalize function
normalize(c(1, 2, 3, 4, 5))
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean) 
#split the wbcd_n data frame into wbcd_train and wbcd_test
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
#When we constructed our normalized training and test datasets, we excluded the target variable, diagnosis. For training the k-NN model, we will need to store  these class labels in factor vectors, split between the training and test datasets:
wbcd_train_labels <- wbcd[1:469, 1] 
wbcd_test_labels <- wbcd[470:569, 1]
install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
install.packages("gmodels") 
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE) 
# Z-transformation 
wbcd_z <- as.data.frame(scale(wbcd[-1])) 

summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ] 
wbcd_train_labels <- wbcd[1:469, 1] 
wbcd_test_labels <- wbcd[470:569, 1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE) 
