##Homework #2
##Amarilda Celhaka
##9/19/2019

#. Load the data frame "iris" and do the following:
###1. Produce a numerical summary for each of the variables in the data.
###2. Obtain a table that shows the total number of observations of each species.
###3. Add a new variable to the data frame that contains the ratio of petal length and petal width for each observation.
###4. Find the mean and median of the ratios obtained in the previous step for each species.
###5. Create subsets of the original dataset so that each subset contains all the observations of the same species.
###6. Divide the original dataset randomly into five subsets of equal size.

data('iris')
fix(iris)
str(iris)

##1
summary(iris)


##2
table(iris$Species)

##3
#adding petalRatio to the dataset
petalRatio <- (iris$Petal.Length/iris$Petal.Width)
iris$petalRatio <- petalRatio
fix(iris)

##4
##mean
species <-as.factor(iris$Species)
for(i in levels(species)){
  m <- mean(iris$petalRatio[species==i])
  cat("Species",i, " Mean = ",m,'\n')
}

##median
species <-as.factor(iris$Species)
for(i in levels(species)){
  m <- median(iris$petalRatio[species==i])
  cat("Species Median",i,m,'\n')
}

##5

subset(iris, Species == 'setosa', select = Sepal.Length:Petal.Width)
subset(iris, Species == 'versicolor', select = Sepal.Length:Petal.Width)
subset(iris, Species == 'virginica', select = Sepal.Length:Petal.Width)

##6
# the following function splits data frame 'givenDataSet' randomly 
# into K subsets of equal (nearly equal) size:
split_dataset <- function(givenDataSet,K){
  n <- nrow(givenDataSet)
  X <- givenDataSet[sample(1:n,n),]
  # the subsets of the data will be stored in a list
  datasets <- list()
  for(i in 1:K){
    idx <- which(1:n %% K == i)
    datasets[[i]] <- X[idx,]
  }
  return(datasets)
}

#using the function given by the professor, we input our dataset iris and 
#the number of samples = 5
split_dataset(iris,5)










#. Load DAAG data frame "carprice" and do the following:
##1. Use plot() or boxpot() to compare the prices among different types of cars. Write a paragraph reporting your finding from the chart.
##2. Identify pairs of variables that "seem" correlated.

##1
data('carprice')
fix(carprice)
plot(carprice$Type ~ carprice$Price)
plot(carprice$Price ~ carprice$Type)
#Looking at the plot of prices and types of cars we can see how prices change depending on the type of the car. 
#Prices on average are cheaper for a smaller car with a mean around 10 than it is followed by a compact car with
#with a mean of around 13-14. For a large car, in general prices are higher, with a mean of around 22
#The min price is scored for a amall car and the highest price is seen for a midsize car which is about 40
#The highest price for a small car seem to be lower than the lowest price for a large, midsize, sporty and van type of cars
#For a midsize car the highest price seem to have a very big gap from mean, and the same thing seem to apply for a large car.
#For a sporty car we have an outlier, which indicates the most extreme observation for the price in sports car

##2 
pairs(carprice)

##The pairs that seem correlated are a few. On Range Price row, Min.Price, Price and Max Price seem
#correlated to one another
#in MinPrice Row we see correleated Rough Range and Range Price
#Also [Range Price,Rough Price] is corelated with [Rough Price, Range Price]
#[Mpg highway, RAnge Price] is correlated with [Mpg highway, Rough Price]














#. Load DAAG data frame "rainforest" and
##1. For each variable in the data frame, identify the rows with missing values.
##2. Remove any possible missing values for each variable.
##3. For the variables without missing values, find the values of the six summary statistics. Compare your results with the results from summary() function for the data frame.

data('rainforest')
fix(rainforest)      
which(is.na(rainforest),arr.ind = T)

##2
idx <- which(is.na(rainforest),arr.ind = T)
idx
removed_missed_values <- rainforest[-idx[,1],]
removed_missed_values

##3

## 6 summanry statistics for dbh
cat("Mean of dbh = ", mean(removed_missed_values$dbh))
cat("Median of dbh = ", median(removed_missed_values$dbh))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$dbh,probs=c(0,0.25,0.5,0.75,1))

cat("Mean of bark = ", mean(removed_missed_values$bark))
cat("Median of bark = ", median(removed_missed_values$bark))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$bark,probs=c(0,0.25,0.5,0.75,1))

cat("Mean of wood = ", mean(removed_missed_values$wood))
cat("Median of wood = ", median(removed_missed_values$wood))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$wood,probs=c(0,0.25,0.5,0.75,1))

cat("Mean of root = ", mean(removed_missed_values$root))
cat("Median of root = ", median(removed_missed_values$root))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$root,probs=c(0,0.25,0.5,0.75,1))

cat("Mean of rootsk = ", mean(removed_missed_values$rootsk))
cat("Median of rootsk = ", median(removed_missed_values$rootsk))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$rootsk,probs=c(0,0.25,0.5,0.75,1))

cat("Mean of branch = ", mean(removed_missed_values$branch))
cat("Median of branch = ", median(removed_missed_values$branch))
#Min, first quartile, 3rd quantile and max 
quantile(removed_missed_values$branch,probs=c(0,0.25,0.5,0.75,1))

summary(removed_missed_values)

##After looking at the 6 summary statistics of each of the non missing values(NA) varaibles
#we can notice that they are exactly the same as displayed by the summary function. 