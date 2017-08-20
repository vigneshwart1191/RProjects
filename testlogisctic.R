spam <- read.csv("C:/Users/vignesht/Downloads/spam.csv", header=T)

str(spam)
dim(spam)
str(spam)
summary(spam)
boxplot(spam)
x <- cor(spam)

which(x >= 0.7 & x != 1, arr.ind = TRUE)



#scaling

spam01 <- spam

min_max = function(x){
  x <- (x - min(x))/(max(x) - min(x))
}

spam_1 <- apply(spam01[,1:58], 2, min_max)
spam_1 <- as.data.frame(spam_1)
str(spam01)
cl1 <- subset(spam_1, spam_1$V58 == 1)
cl0 <- subset(spam_1, spam_1$V58 == 0)


inx1 <- floor(0.8 * nrow(cl1))
inx0 <- floor(0.8 * nrow(cl0))

tr1 <- sample(1:nrow(cl1), size = inx1)
tr0 <- sample(1:nrow(cl0), size = inx0)

train1 <- cl1[tr1,]
train0 <- cl0[tr0,]
train <- rbind(train1, train0)
#train


test1 <- cl1[-tr1,]
test0 <- cl0[-tr0,]
test <- rbind(test1, test0)
#test


# training and testing data after removing columns V31, V34, V36 and V40
spam1 <- spam
spam2 <- spam1[,-c(31, 34, 36, 40)]
spam2 <- as.data.frame(spam2)
#spam2


cl11 <- subset(spam2, spam2$V58 == 1)
cl00 <- subset(spam2, spam2$V58 == 0)


inx11 <- floor(0.8 * nrow(cl11))
inx00 <- floor(0.8 * nrow(cl00))

tr11 <- sample(1:nrow(cl11), size = inx11)
tr00 <- sample(1:nrow(cl00), size = inx00)

train11 <- cl11[tr11,]
train00 <- cl00[tr00,]
train_f <- rbind(train11, train00)
#train_f

test11 <- cl11[-tr11,]
test00 <- cl00[-tr00,]
test_f <- rbind(test11, test00)
#test_f

#model

glm_spam <- glm(spam$V58 ~ . , data = spam, family = binomial)
glm_spam

#improved model

glm_spam1 <- glm(train$V58 ~ . , data = train, family = binomial)
glm_spam1

prd_spam1 <- predict(glm_spam1, type = 'response', test)

prd_spam1[prd_spam1 < 0.5] = 0
prd_spam1[prd_spam1 >= 0.5] = 1
prd_spam1

true_pos1 <- (prd_spam1 == test$V58)
true_val1 <- true_pos1[true_pos1 == TRUE]
true_len1 <- length(true_val1)
true_len1

total_len1 <- length(test$V58)
total_len1

#Accuracy = (true positive + true negative) / total test data

acc1 = (true_len1 * 100)/total_len1 
acc1

# Accuracy is 93.37, true classified points = 860, AIC = 1515.

#model after droping V31, V34, V36, V40

glm_spam2 <- glm(train_f$V58 ~ ., data = train_f, family = binomial)
glm_spam2

prd_spam2 <- predict(glm_spam2, type = 'response', test_f)

prd_spam2[prd_spam2 < 0.5] = 0
prd_spam2[prd_spam2 >= 0.5] = 1
prd_spam2

true_pos2 <- (prd_spam2 == test_f$V58)
true_val2 <- true_pos2[true_pos2 == TRUE]
true_len2 <- length(true_val2)
true_len2

total_len2 <- length(test_f$V58)
total_len2

#Accuracy = (true positive + true negative) / total test data

acc2 = (true_len2 * 100)/total_len2 
acc2

# Accuracy with the removal of V31, V34, V36 & V40 is 93.26, True classified points = 859,
# 1 less than only scaled model. Though AIC is more than the only scaled model (1555).