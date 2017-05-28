require(plyr)
require(MASS)
require(glmnet)
require(ridge)
require(caret)
require(pracma)


sales = read.csv('UOW/kc_house_data.csv', header = TRUE)


#sort the sqft by price
sales <- arrange(sales,sqft_living,price)
#sales <- sales[order(sales$sqft_living, sales$price),]

#initial penalty to test
l2_small_penalty = 1.5e-5


#creating a polynomial dataset
polynomial_sframe <- function(feature , degree){
  df_frame = data.frame('power_1' = feature)
  if(degree > 2){
    for(i in 2:degree){
      title <- paste('power',i,sep="_")
      df_frame[,title] <- feature^i
    }
  }
  return(df_frame)
}

poly15_data = polynomial_sframe(sales$sqft_living,15)
poly15_data$price <- sales$price
head(poly15_data)


#lets check the model complexity 
lm_model <- lm(price~.,poly15_data)
summary(lm_model)
prediction <- predict(lm_model, newdata = poly15_data)
plot(poly15_data$power_1, poly15_data$price)
lines(poly15_data$power_1,prediction,col='red')

#simple ridge regression using CARET package
rr.ca <- train(poly15_data[,1:15],poly15_data[,16],'glmnet',tuneGrid = expand.grid(alpha=0, lambda=1e5))
rr.ca
coef(rr.ca$finalModel)
prediction <- predict(rr.ca,newdata = poly15_data[,1:15])
plot(poly15_data$power_1, poly15_data$price)
lines(poly15_data$power_1, prediction, col="red")

#implementing Ridge regression using glmnet
x_matrix <- model.matrix(price~.,poly15_data)[,-16]
y_matrix <- poly15_data$price

rr.glmnet <- glmnet(x_matrix,y_matrix,alpha = 0, nlambda = 2,lambda = l2_small_penalty,intercept=0)
rr.glmnet$lambda
coef(rr.glmnet)
plot(rr.glmnet)

prediction <- predict(rr.glmnet, newx = x_matrix)

plot(poly15_data$power_1, poly15_data$price)
lines(poly15_data$power_1, as.vector(prediction), col="red")

#simple function for ridge regression
# get_ridge_model <- function(dat,penalty,formula,target){
#   r <- which(colnames(dat)==target)
#   x_m <- model.matrix(formula,dat)[,-r];y_m <- dat[,target]
#   model <- glmnet(x_m,y_m,alpha = penalty, standardize = TRUE, intercept = 0)
#   return(list('model'=model,'x'=x_m))
# }


# get_caret_rr <- function(x,y,penalty){
#   #model <- train(x,y,method="ridge",preProcess = c("center","scale"),lambda = penalty)
#   model <- train(x,y,method = 'glmnet', preProcess = c("center","scale"), alpha=0, lambda=penalty)
#   return(model)
# }

# qq <- linearRidge(modelFormula,set_1_p,lambda = l2_penalty,scaling = 'scale')
# coef(qq)
# summary(qq)


get_poly_data <- function(dat, feature, target, degree){
  poly_data <- polynomial_sframe(dat[,feature],degree)
  poly_data[,target] <- dat[,target]
  return(poly_data)
}

get_plot <- function(c,x,y,p){
  prediction <- predict(c$model,s=p, newx = c$x)
  plot(x,y)
  lines(x,prediction, col='red')
}

#OBSERVING OVERFITTING  
l2_penalty = 1e-9

set_1 <- read.csv('UOW/wk3_kc_house_set_1_data.csv', header = TRUE)
set_2 <- read.csv('UOW/wk3_kc_house_set_2_data.csv', header = TRUE)
set_3 <- read.csv('UOW/wk3_kc_house_set_3_data.csv', header = TRUE)
set_4 <- read.csv('UOW/wk3_kc_house_set_4_data.csv', header = TRUE)


#set_1
modelFormula <- price~.
set_1_p <- get_poly_data(set_1, 'sqft_living','price',15)
set_1_model <- get_ridge_model(set_1_p,l2_penalty,modelFormula,'price')
coef(set_1_model$model)
plot(set_1_model$model,xvar = "lambda")
get_plot(set_1_model,set_1_p$power_1,set_1_p$price,l2_penalty)

#set_2
set_2_p <- get_poly_data(set_2, 'sqft_living','price',15)
set_2_model <- get_ridge_model(set_2_p,l2_penalty,modelFormula,'price')
coef(set_2_model$model)
get_plot(set_2_model,set_2_p$power_1,set_2_p$price,l2_penalty)
  
#set_3
set_3_p <- get_poly_data(set_3, 'sqft_living','price',15)
set_3_model <- get_ridge_model(set_3_p,l2_penalty,modelFormula,'price')
coef(set_3_model$model)
get_plot(set_3_model,set_3_p$power_1,set_3_p$price,l2_penalty)

#set_4
set_4_p <- get_poly_data(set_4, 'sqft_living','price',15)
set_4_model <- get_ridge_model(set_4_p,l2_penalty,modelFormula,'price')
coef(set_4_model$model)
get_plot(set_4_model,set_4_p$power_1,set_4_p$price,l2_penalty)


#Implementing large penalty
l2_penalty <- 1.23e2


#set_1
modelFormula <- price~.
set_1_p <- get_poly_data(set_1, 'sqft_living','price',15)
set_1_model <- get_ridge_model(set_1_p,l2_penalty,modelFormula,'price')
set_1_model$model$lambda
coef(set_1_model$model)
get_plot(set_1_model,set_1_p$power_1,set_1_p$price,l2_penalty)

#set_2
set_2_p <- get_poly_data(set_2, 'sqft_living','price',15)
set_2_model <- get_ridge_model(set_2_p,l2_penalty,modelFormula,'price')
coef(set_2_model$model)
get_plot(set_2_model,set_2_p$power_1,set_2_p$price,l2_penalty)

#set_3
set_3_p <- get_poly_data(set_3, 'sqft_living','price',15)
set_3_model <- get_ridge_model(set_3_p,l2_penalty,modelFormula,'price')
coef(set_3_model$model)
get_plot(set_3_model,set_3_p$power_1,set_3_p$price,l2_penalty)

#set_4
set_4_p <- get_poly_data(set_4, 'sqft_living','price',15)
set_4_model <- get_ridge_model(set_4_p,l2_penalty,modelFormula,'price')
coef(set_4_model$model)
get_plot(set_4_model,set_4_p$power_1,set_4_p$price,l2_penalty)


#using caret package
# modelFormula <- price~.
# set_1_p <- get_poly_data(set_1, 'sqft_living','price',15)
# #set_1_model <- get_ridge_model(set_1_p,l2_penalty,modelFormula,'price')
# tg <- expand.grid(alpha = 1e-5,lambda=0)
#set_1_mod <- train(set_1_p[,1:15],set_1_p[,16],'glmnet',tuneGrid = tg)


#using ridge package penalty=1e-9
#set1
set_1_m <- linearRidge(price~.,set_1_p, lambda = 1e-9)
prediction <- predict(set_1_m, newdata = set_1_p[,1:15])
plot(set_1_p$power_1, set_1_p$price)
lines(set_1_p$power_1, prediction, col="red")
coef(set_1_m)

#set2
set_2_m <- linearRidge(price~.,set_2_p, lambda = 1e-9)
prediction <- predict(set_2_m, newdata = set_2_p[,1:15])
plot(set_2_p$power_1, set_2_p$price)
lines(set_2_p$power_1, prediction, col="red")
coef(set_2_m)

#set3
set_3_m <- linearRidge(price~.,set_3_p, lambda = 1e-9)
prediction <- predict(set_3_m, newdata = set_3_p[,1:15])
plot(set_3_p$power_1, set_3_p$price)
lines(set_3_p$power_1, prediction, col="red")
coef(set_3_m)

#set4
set_4_m <- linearRidge(price~.,set_4_p, lambda = 1e-9)
prediction <- predict(set_4_m, newdata = set_4_p[,1:15])
plot(set_4_p$power_1, set_4_p$price)
lines(set_4_p$power_1, prediction, col="red")
coef(set_4_m)


penalty = 123
#penalty = 1e5
#using ridge package penalty=123 and 1e5
#set1
set_1_m <- linearRidge(price~.,set_1_p, lambda = penalty)
prediction <- predict(set_1_m, newdata = set_1_p[,1:15])
plot(set_1_p$power_1, set_1_p$price)
lines(set_1_p$power_1, prediction, col="red")
coef(set_1_m)

#set2
set_2_m <- linearRidge(price~.,set_2_p, lambda = penalty)
prediction <- predict(set_2_m, newdata = set_2_p[,1:15])
plot(set_2_p$power_1, set_2_p$price)
lines(set_2_p$power_1, prediction, col="red")
coef(set_2_m)

#set3
set_3_m <- linearRidge(price~.,set_3_p, lambda = penalty)
prediction <- predict(set_3_m, newdata = set_3_p[,1:15])
plot(set_3_p$power_1, set_3_p$price)
lines(set_3_p$power_1, prediction, col="red")
coef(set_3_m)

#set4
tgr <- expand.grid(lambda=c(1e-5,123))
set_4_m <- train(price~.,set_4_p,'ridge',tuneGrid=tgr)
prediction <- predict(set_4_m, newdata = set_4_p[,1:15])
plot(set_4_p$power_1, set_4_p$price)
lines(set_4_p$power_1, prediction, col="red")
coef(set_4_m)
set_4_m



######## selecting L2 penalty via cross-validation ########

train_valid_shuffle <- read.csv('UOW/wk3_kc_house_train_valid_shuffled.csv')
test_v <- read.csv('UOW/wk3_kc_house_test_data.csv')

#k-fold loop
n <- nrow(train_valid_shuffle)
k=10 #10-fold validation

# for(i in 1:10){
#   s = 1+((n*(i-1))%/%k)
#   e = (n*i)%/%k
#   print(c(i,c(s,e)))
# }

#creating a k-fold validation
# k_fold_cross_validation <- function(k,l2_penalty,data,output){
#   n = nrow(data)
#   kk <- k-1
#   RSS_list <- c()
#   for(i in 1:k){
#     s = 1+((n*(i-1))/k)
#     e = (n*i)/k
#     
#     validation_set <- data[s:e,]
#     training_set <- data[-c(s:e),]
#     
#     poly_train <- polynomial_sframe(training_set[,'sqft_living'],15)
#     poly_train[,output] <- training_set[,output]
#     poly_valid <- polynomial_sframe(validation_set[,'sqft_living'],15)
#     poly_valid[,output] <- validation_set[,output]
#     model <- linearRidge(price~.,poly_train,lambda = l2_penalty)
# 
#     prediction <- predict(model,poly_valid[1:15])
#     #Error in as.matrix(mm)%*%beta: non-conformable arguments
# 
#     RSS = sum((prediction - poly_valid[output])^2)
# 
#     RSS_list <- c(RSS_list,RSS)
#   }
#   average_rss <- mean(RSS_list)
#   return(average_rss)
# }
#facing issue with getting RSS for each validation further research needed


#just tried with caret
# k_fold_caret <- function(k,penalty,data,output){
#   tc <- trainControl(method="cv",number=k)
#   tg <- expand.grid(lambda = penalty)
#   poly_data_i <- polynomial_sframe(data[,'sqft_living'],15)
#   poly_data_o <- data[,output]
#   model <- train(poly_data_i,poly_data_o,'ridge',trControl = tc,tuneGrid=tg)
#   prediction <- predict(model, poly_data_i)
#   RSS <- sum((prediction - poly_data_o)^2)
#   return(RSS)
# }


#implementing cv.glmnet
cross_validation_ <- function(k,penalty,data,output){
  poly_data <- polynomial_sframe(data[,'sqft_living'],15)
  poly_data[,output] <- data[,output]
  p_x <- model.matrix(price~.,poly_data)[,-16]; p_y <- data[,output]
  model <- cv.glmnet(p_x,p_y,lambda = penalty,nfolds = k)
  prediction <- predict(model, p_x)
  RSS = sum((prediction - p_y)^2)
  return(list('model'=model,'rss'=RSS))
}


l2_penalty = logspace(3,9,13)

#k_fold_average <- k_fold_cross_validation(10,l2_penalty,train_valid_shuffle,'price')
#k_fold_average <- k_fold_caret(10,l2_penalty,train_valid_shuffle,'price')
k_fold<- cross_validation_(10,l2_penalty,train_valid_shuffle,'price')
new_l2_penalty <- k_fold$model$lambda.min

#training the shuffeled dataset
poly_15 <- polynomial_sframe(train_valid_shuffle[,'sqft_living'],15)
poly_15$price <- train_valid_shuffle$price

x <- model.matrix(price~.,poly_15)[,-16]; y <- poly_15$price

glmnet_model <- glmnet(x,y,alpha=0,lambda = new_l2_penalty)

#predicting the output
poly_test <- polynomial_sframe(test_v$sqft_living, 15)
poly_test$price <- test_v$price

x_test <- model.matrix(price~.,poly_test)[,-16];
prediction <- predict(glmnet_model,newx = x_test, s= new_l2_penalty, type="response")

#cost
RSS = sum((prediction - poly_test$price)^2)
RSS
