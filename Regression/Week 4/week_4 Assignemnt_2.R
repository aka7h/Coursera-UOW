#Implementing Ridge regression via Gradient Decent

sales <- read.csv('UOW/kc_house_data.csv', header = TRUE)


#get matrix data
get_matrix_data <- function(dataFrame,features,output){
  dataFrame$constant <- 1
  features <- c('constant',features)
  feature_frame <- dataFrame[,features]
  feature_matrix <- data.matrix(feature_frame)
  
  output_dframe <- dataFrame[,output]
  output_matrix <- data.matrix(output_dframe)
  return(list('fmatrix'=feature_matrix,'omatrix'=output_matrix))
}

predict_output <- function(feature_matrix, weights){
  predictions <- feature_matrix %*% weights
  return(predictions)
}

#gradient decent derivative
feature_derivative <- function(error,feature, weight, l2_penalty, feature_is_constant){
  #we don't penalize the constant term with the penalty.
  if(feature_is_constant){
    derivative = 2*(error*abs(feature))
  }else{
    derivative = 2*(error*feature) + 2*(l2_penalty*weight)
  }
  return(sum(derivative))
}

#lets test if the above function is working correctly or not
tmatrix <- get_matrix_data(sales,'sqft_living','price')
my_weights <- c(1,10)
test_predictions <- predict_output(tmatrix$fmatrix, my_weights)
errors <- test_predictions - tmatrix$omatrix

#the next two line should print the same values
print(feature_derivative(errors,tmatrix$fmatrix[,2],my_weights[2],1,FALSE)) #-5.655417e+13
print(sum(errors*tmatrix$fmatrix[,2])*2+20) #-5.655417e+13

#the above 2 lines prints the same 

#the next two line should print the same values
print(feature_derivative(errors,tmatrix$fmatrix[,1],my_weights[1],1,TRUE)) #-22446749330
print(sum(errors*tmatrix$fmatrix[,1])*2) #-22446749330

#the above also printing the same hence the feature_derivative function is working fine is working fine.


#Gradient Decent
ridge_regression_GD<-function(feature_matrix,output,initial_weight,step_size,l2_penalty,max_iterations){
  weight = initial_weight
  j_hist <- rep(0,max_iterations)
  for(t in 1:max_iterations){
    prediction = predict_output(feature_matrix,weight)
    errors = prediction - output
    
    for(i in 1:length(weight)){
      if(i==1){
        feature_is_constant = TRUE
      }else{
        feature_is_constant = FALSE
      }
      derivative=feature_derivative(errors,feature_matrix[,i],weight[i],l2_penalty,feature_is_constant)
      weight[i] = weight[i] - (step_size*derivative)
    }#end of for loop
  }#end of for loop
  return(list(weight,j_hist))
}

#Now lets visualize the effect of L2 Penalty

simple_feature = 'sqft_living'
simple_output = 'price'

gd_train <- read.csv('UOW/wk3_kc_house_train_data.csv')
gd_test <- read.csv('UOW/wk3_kc_house_test_data.csv')

tmatrix_train <- get_matrix_data(gd_train,simple_feature,simple_output)
tmatrix_test <- get_matrix_data(gd_test,simple_feature,simple_output)

step_size = 1e-12
max_iteration = 1000
initial_weight = c(0.0,0.0)

#low penalty
simple_weight_0_penalty <- ridge_regression_GD(tmatrix_train$fmatrix,tmatrix_train$omatrix,initial_weight,step_size,0.0,max_iteration)
simple_weight_0_penalty

#high penalty 1e7
simple_weight_high_penalty <- ridge_regression_GD(tmatrix_train$fmatrix,tmatrix_train$omatrix,initial_weight,step_size,1e7,max_iteration)
simple_weight_high_penalty

prediction_low <- predict_output(tmatrix_train$fmatrix, simple_weight_0_penalty[[1]])
prediction_high <- predict_output(tmatrix_train$fmatrix, simple_weight_high_penalty[[1]])

plot(gd_train$sqft_living,gd_train$price)
lines(gd_train$sqft_living, prediction_low, col="blue")
lines(gd_train$sqft_living, prediction_high, col="red")

#here the red is the with high penalty and the blue is with the low penalty.
#now lets check which of the following performs the best
# 1. initial weights(0)
# 2. GD without regularization
# 3. GD with reqularization

# question 1 - initial weight
initial_prediction <- predict_output(tmatrix_test$fmatrix,initial_weight)
initial_residual <- tmatrix_test$omatrix - initial_prediction
initial_RSS <- sum(initial_residual*initial_residual)
initial_RSS #9.279136e+14

# 2 - without regularization
GD_prediction <- predict_output(tmatrix_test$fmatrix,simple_weight_0_penalty[[1]])
GD_residual <- tmatrix_test$omatrix - GD_prediction
GD_RSS <- sum(GD_residual*GD_residual)
GD_RSS #1.43087e+14

#3 - with regularization
Reg_prediction <- predict_output(tmatrix_test$fmatrix, simple_weight_high_penalty[[1]])
Reg_residual <- tmatrix_test$omatrix - Reg_prediction
Reg_RSS <- sum(Reg_residual*Reg_residual)
Reg_RSS


#Running Multiple regression with L2
model_feature <- c('sqft_living','sqft_living15')
model_output <- 'price'
mmat <- get_matrix_data(gd_train, model_feature, model_output)
mmatT <- get_matrix_data(gd_test, model_feature, model_output)

initial_weight <- c(0.0,0.0,0.0)
step_size <- 1e-12
max_iteration = 1000


multiple_weight_0_penalty <- ridge_regression_GD(mmat$fmatrix,mmat$omatrix,initial_weight,step_size,0.0,max_iteration)
multiple_weight_0_penalty

#high penalty 1e7
multiple_weight_high_penalty <- ridge_regression_GD(mmat$fmatrix,mmat$omatrix,initial_weight,step_size,1e7,max_iteration)
multiple_weight_high_penalty

#now lets check which of the following performs the best
# 1. initial weights(0)
# 2. GD without regularization
# 3. GD with reqularization

# question 1 - initial weight
initial_prediction <- predict_output(mmatT$fmatrix,initial_weight)
initial_residual <- mmatT$omatrix - initial_prediction
initial_RSS <- sum(initial_residual*initial_residual)
initial_RSS #9.279136e+14

# 2 - without regularization
GD_prediction <- predict_output(mmatT$fmatrix,multiple_weight_0_penalty[[1]])
GD_residual <- mmatT$omatrix - GD_prediction
GD_RSS <- sum(GD_residual*GD_residual)
GD_RSS #1.43087e+14

#3 - with regularization
Reg_prediction <- predict_output(mmatT$fmatrix, multiple_weight_high_penalty[[1]])
Reg_residual <- mmatT$omatrix - Reg_prediction
Reg_RSS <- sum(Reg_residual*Reg_residual)
Reg_RSS

#predict the house price for the first house in the test for both non regularized and regularised prediction
print(GD_prediction[1])
print(mmatT$omatrix[1]-GD_prediction[1])

print(Reg_prediction[1])
print(mmat$omatrix[1]-Reg_prediction[1])


#initially should use 1e11 as the high penalty, but afer few iteration the weight are so less that 
# they reached infinity and after 1000 iteration got NaN as output hence decreased to 1e7