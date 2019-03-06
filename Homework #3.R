#                                 Amarilda Celhaka
#                                   Homework #3
#                                    9/26/2018







##1. Create and save an external .txt file containing 20 values for each of two variables
##with the names "Population1" and "Population2" respectively. The values for Population1 
##are generated from B(1,0.4) and the values for Population2 are generated from B(1,0.6). 
##Then do the following

rbinom(20,1,0.4)
#1 0 0 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 0 1
rbinom(20,1,0.6)
#0 1 1 0 0 1 1 0 1 0 1 1 0 1 1 1 1 1 1 1




#(a) Read the file into R as a data frame.

population_data <- read.table("population.txt",  header = TRUE)
population_data




 
 
#(b) Draw a random sample of size 100 from the set of the values of Population1 in the data
#frame and calculate the sample mean and sample standard deviation.
 
 

 # 1:20 from row 1 to 20, size of sample = 100, with replacement, extracting only first column(pop1)
 
 pop1_sample <- population_data[sample(1:20,100,replace=T),1]
 pop1_sample
 
 
 #mean of population 1 random sample
 
 mean_pop1 <- mean(pop1_sample)
 mean_pop1
 
 
 #standard deviation of population 1 random sample
 
 sd_pop1 <- sd(pop1_sample)
 sd_pop1
 

 
 
 
#(c) Repeat step (b) 1000 times to generate 1000 sample means for both Population1 and Population2.
 
 
 
 #sample means for Population 1

 sample_means_pop1 <- array()   #creating this array to hold 1000 sample means
 
 for ( i in 1:1000){
  
    mean_pop1 <- mean(population_data[sample(1:20,100,replace=T),1])
    
    sample_means_pop1[i] <- mean_pop1 
    
    print(mean_pop1)            #printing sample means after each iteration
   
 }

 
 
 
 #population2 sample means
 
 sample_means_pop2 <- array()
 
for ( i in 1:1000){
  
   mean_pop2 <- mean(population_data[sample(1:20,100,replace=T),1])
   
   sample_means_pop2[i] <- mean_pop2
   
   print(mean_pop2)
   
 }

 
 

 
 
#(d) Display histograms for two sets of the samples means.
 
 
  #histogram for population 1 sample means
 
  hist(sample_means_pop1, breaks = 200)
  
 
  #histogram for population 2 sample means
  
  hist(sample_means_pop2, breaks = 200)

  
  
  

    
#(e) Make a side-by-side boxplot for two sets of the sample means.
  
  
 #boxplot for population 1 sample means
  
 boxplot(sample_means_pop1) 
 
 
 #boxplot for population 2 sample means
 
 boxplot(sample_means_pop2)
 
 plot(sample_means_pop1 ~ sample_means_pop2)
 plot(sample_means_pop2 ~ sample_means_pop1)
 
 
 
 
 

#(f) Write a function that takes a set of sample means (from the sample population) and a percentage
#number as inputs and returns the sample percentile as the output.
 
 
  percentile <- function(pop_sample, percentage){
    
    return (quantile(pop_sample,percentage));
    
  }

  
  
  
  
  
#(g) For each set of the sample means, plot the function defined in (f) as the percentage number 
#varies from 0 to 1.
  
  
  
  percentile(sample_means_pop1,.25)
  
  percentile(sample_means_pop2, .6)
  
  

  
  
#(h) Save two sets of the samples generated above to an external excel spreadsheet and an external R dataset.

  
  
   #saving population 1 sample means as a R file
  
   save(sample_means_pop1,file='SampleMeansOfPopulation1.RDATA')
 
   #saving population 1 sample means as a excel speadsheet
   
   write.csv(sample_means_pop1,file='SampleMeansOfPopulation1.csv')
   
   
   #saving population 2 sample means as a R file
   
   save(sample_means_pop2,file='SampleMeansOfPopulation2.RDATA')
   
   #saving population 2 sample means as an excel spreadsheet
   
   save(sample_means_pop2,file='SampleMeansOfPopulation2.RDATA')
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
 
 ##2. Create and save an external excel spreadsheet file containing 20 values for each of two variables with
 #the names "Measurement1" and "Measurement2" respectively. The values for Measurement1 are generated from 
 #N(1.0,2) and the values for Measurement2 are generated from N(2.5,1). Repeat (a) - (h) in Problem 1.

 rnorm(20, mean = 1.0, sd = 2)
 
 rnorm(20, mean = 2.5, sd = 1)
 
 
 
 #(a)
 

 #sep = "," indicates that colums are separated by a comma
 
 measurement_data <- read.table("Measurements.csv",  header = TRUE, sep=",")
 measurement_data

 

 
 
 
  
 #(b) 
 
 
 #generating a random sample of size 100 from the set of values of Measurement 1
 
 measure1_sample <- measurement_data[sample(1:20,100,replace=T),1]
 
 #mean of measurement 1 random sample
 
 mean_measure1 <- mean(measure1_sample)
 mean_measure1

 #standard deviation of measurement 1 random sample
 
 sd_measure1 <- sd(measure1_sample)
 sd_measure1
 
 
 
 
 
 
 
 
 #(c)
 
 
 
 #sample means for Measurement 1
 
 sample_means_measure1 <- array()   #creating this array to hold 1000 sample means
 
 for ( i in 1:1000){
   
   mean_measure1 <- mean(measurement_data[sample(1:20,100,replace=T),1])
   
   sample_means_measure1[i] <- mean_measure1 
   
   print(mean_measure1)            #printing sample means after each iteration
   
 }
 
 
 
 
 #Measurerement 2 sample means
 
 sample_means_measure2 <- array()
 
 for ( i in 1:1000){
   
   mean_measure2 <- mean(measurement_data[sample(1:20,100,replace=T),1])
   
   sample_means_measure2[i] <- mean_measure2
   
   print(mean_measure2)
   
 }

 
 
 
 
 
 
 #(d) 
 
 
 #histogram for measurement 1 sample means
 
 hist(sample_means_measure1, breaks = 200)
 
 
 #histogram for measurement 2 sample means
 
 hist(sample_means_measure2, breaks = 200)

 
 
 
 
 
 
 #(e) 

 
 
 #boxplot for measurement 1 sample means
 
 boxplot(sample_means_measure1) 
 
 
 #boxplot for measurement 2 sample means
 
 boxplot(sample_means_measure2)
 
 
 
 
 
 #(f)
 
 
 percentile <- function(pop_measure, percentage){
   
   return (quantile(pop_measure,percentage));
   
 }
 
 
 
 
 
 #(g)
 
 
 
 percentile(sample_means_measure1,.25)
 
 percentile(sample_means_measure2, .6)

 
 
 
 
 
 #(h) 
 
 
 
 #saving measurement 1 sample means as a R file
 
 save(sample_means_measure1,file='SampleMeansOfMeasurement1.RDATA')
 
 #saving measurement 1 sample means as a excel speadsheet
 
 write.csv(sample_means_measure1,file='SampleMeansOfMeasurement1.csv')
 
 
 #saving measurement 2 sample means as a R file
 
 save(sample_means_measure2,file='SampleMeansOfMeasurement2.RDATA')
 
 #saving measurement 2 sample means as an excel spreadsheet
 
 write.csv(sample_means_measure2,file='SampleMeansOfMeasurement2.csv')
 
