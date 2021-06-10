### model fit and check
# set-up =======================================================================

library(ggplot2)
library(readr)

##3read file in from data wrangling

setosa_subset = read_csv('./output/setosa_subset_data.csv',
                         guess_max =16000)

# further subset data
setosa_subset = setosa_subset[which(setosa_subset$Petal.Width > 0.2), ]


# run a simple model
hist(setosa_subset$Sepal.Length)

setosa_model = glm(Sepal.Length ~ Petal.Width*Petal.Length, 
                   data = setosa_subset)

summary(setosa_model)
saveRDS(setosa_model, './output/setosa_model_object.rds') 
## save the model object so that you dont have to run them every time

# read in rds and plot results
setosa_model = readRDS('./output/setosa_model_object.rds')

##model prediction
new_data = data.frame(Petal.Length = seq(1.3, 1.9, 0.1),
                      Petal.Width = seq(0.3, 0.9, 0.1))
new_data$prediction = predict(setosa_model, new_data, type = "response")

write_csv(new_data, './output/new_data_for_prediction.csv')
