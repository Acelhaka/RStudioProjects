#Homework #3 
#Amarilda Celhaka
#10/17/2018



#1. Use iris data set in R to obtain 95% and 99% confidence interval
#estimates of the mean sepal length for each of the three iris species 
#(two intervals of different confidence levels for each species).
data('iris')
str(iris)
fix(iris)



x <- iris$Sepal.Length
species <-as.factor(iris$Species)
for(i in levels(species)){
 
  ci <-  t.test(x[species==i],conf.level = 0.95)
  cat("*************Species: ",i )
  print(ci)
 
}

###obtaining the 99% confidence interval
x <- iris$Sepal.Length
species <-as.factor(iris$Species)
for(i in levels(species)){
  
  ci <-  t.test(x[species==i],  conf.level = 0.99)
  cat("******Species",i )
  print(ci)
  
}












##2. Again use the iris data. Find 95% and 99% confidence intervals
#for the difference of the means of the petal width between each pair
#of species (setosa vs versicolor, setosa vs virginica, and versicolor
#vs virginica). Do this first assume the variables have the same
#variance, and then do it without the assumption (and therefore allow 
#the variances can be different).


#setosa vs vesrsicolor


#95% confidence interval with assumption that the variables have the same variance
ci1 <-  t.test(x[species=='setosa'],x[species=='versicolor'],var.equal = TRUE, conf.level = 0.95)

#to display left side of Confidence interval
ci1$conf.int[1]

#to display left side of Confidence interval
ci1$conf.int[2]



#95% confidence interval with diffent variances
ci2 <-  t.test(x[species=='setosa'],x[species=='versicolor'],var.equal = FALSE, conf.level = 0.95)
ci2



#99% confidence interval with assumption that the variables have the same variance
ci3 <-  t.test(x[species=='setosa'],x[species=='versicolor'],var.equal = TRUE, conf.level = 0.99)
ci3



#99% confidence interval with diffent variances
ci4 <-  t.test(x[species=='setosa'],x[species=='versicolor'],var.equal = FALSE, conf.level = 0.99)
ci4






#setosa vs virginica



#95%confidence interval with assumption that the variables have the same variance
ci5 <-  t.test(x[species=='setosa'],x[species=='virginica'],var.equal = TRUE, conf.level = 0.95)
ci5


#95%confidence interval with diffent variances
ci6 <-  t.test(x[species=='setosa'],x[species=='virginica'], var.equal = FALSE, conf.level = 0.95)
ci6



#99% confidence interval with assumption that the variables have the same variance
ci7 <-  t.test(x[species=='setosa'],x[species=='virginica'],var.equal = TRUE, conf.level = 0.99)
ci7


#99% confidence interval with diffent variances
ci8 <-  t.test(x[species=='setosa'],x[species=='virginica'],var.equal = FALSE, conf.level = 0.99)
ci8





#versicolor vs virginica


#95% confidence interval with assumption that the variables have the same variance
ci9 <-  t.test(x[species=='versicolor'],x[species=='virginica'],var.equal = TRUE, conf.level = 0.95)
ci9


#95%confidence interval with diffent variances
ci10 <-  t.test(x[species=='versicolor'],x[species=='virginica'],var.equal = FALSE, conf.level = 0.95)
ci10


#99% confidence interval with assumption that the variables have the same variance
ci11 <-  t.test(x[species=='versicolor'],x[species=='virginica'], var.equal = TRUE, conf.level = 0.99)
ci11


#99%confidence interval with diffent variances
ci12 <-  t.test(x[species=='versicolor'],x[species=='virginica'], var.equal = FALSE, conf.level = 0.99)
ci12








#3 Using simulation data from a normal distribution N(???2,3) to
#obtain N = 10000 95% confdence intervals for the population
#mean and then find percentage of these intervals that contains
#the mean µ = ???2

#size = 25




##confidence interval of population mean


N <- 10000
count <- 0
for(i in 1:N){
  x <- rnorm(25,-2,3)
  
  #calculating left and right side of confidence interval
  c1 <- (mean(x) - qt(0.975, 24)*sd(x)/sqrt(length(x)))
 
  c2 <- (mean(x) + qt(0.975, 24)*sd(x)/sqrt(length(x)))
 
  #checking if the mean = -2 is part of the sample
  if(c1<-2 & c2>-2)
  {
  count <- count + 1    #increasing count by 1 if a sample with mean=-2 is found
  
  }
 
  
}


#diving count by the sample Size and multiplying with 100 to find the percentage
percentage <- (count/N)*100
percentage















#4This problem is related to two sample test problem.
#1 Write a function that takes two samples with dierent lengths
#as input and outputs the p-value of the  t test. Assume both
#samples have the same variance
 


calculatePvalue <- function(sample1, sample2){
          x1 <- mean(sample1)
          x2 <- mean(sample2)
          
          sd1 <- sd(sample1)
          sd2 <- sd(sample2)
          
          #since variances are equal mu2-mu1 = 0
          numerator <- (x1-x2)
        
          denominator <- sqrt(((sd1^2)/length(sample1))+((sd2^2)/length(sample2)))
          
          t <- numerator/denominator
          
        
          df_numerator <- (((sd1^2)/length(sample1)) + ((sd2^2)/length(sample2)))^2
          
          df_denominator <- ((((sd1^2)/length(sample1))^2)/length(sample1 -1)) + 
                            ((((sd2^2)/length(sample2))^2)/length(sample2 -1))
          
          
          degree_of_freedom = df_numerator/df_denominator
          
          p_value <- 2*pt(-abs(t), degree_of_freedom)
          
          print("P value")
          print (p_value)

}



#b

##Taking two samples to test our function
sample1 <- rnorm(50, 1,2)
sample2 <- rnorm(60,1.5,2)


#calling the function for two normal distribution
calculatePvalue(sample1, sample2)

#checking if the value of our p-value is the same as our function output by using t.test 
#that display p-value
t.test(sample1,sample2)


