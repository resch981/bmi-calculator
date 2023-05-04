
# BMI calculator

# Libraries
library(tidyverse)

# Example data
htdata <- c(154.7, 89.3, 160.2, 156.0)
wtdata <- c(42.2, 12, 97.1, 75.3)
# You could import a csv of height and weight values instead

# Input height (cm) and weight (kg), output BMI  
bmiCalc <- function(height, weight){
  bmi <- (weight / height^2) * 10000
  return(bmi)
}

# Check BMI calculator func with example
bmidata <- bmiCalc(htdata, wtdata)
bmidata

# Input for bmiCat must be a tibble
bmidata <- bind_cols(bmi = bmidata)

# Separate BMI values into categories
bmiCat <- function(bmi) {
  catd <- bmi %>% 
    mutate(cat = case_when(bmi <= 0 ~ "NA", 
           bmi > 0 & bmi < 18.5 ~ "A",
           bmi >= 18.5 & bmi < 25 ~ "B",
           bmi >= 25 & bmi < 30 ~ "C",
           bmi >= 30 ~ "D"))
  return(catd)
}

# Check BMI categorize func with example
bmidata_cat <- bmiCat(bmidata)

# Now time to design tests for these functions!

calcTest <- function(){
  # Arbitrary values must return the correct, expected output for the BMI Calculator
  test <- bmiCalc(2, 40) == 100000
  return(test)
}

calcTest()

catTest <- function(){
  # Create arbitrary values to test every possible category.
  Test_Per_Cat <- c(-1, 0, 1, 18.5, 25, 30)
  Test_Per_Cat_Table <- bind_cols(cat_test = Test_Per_Cat)
  Exp_Result <- c("NA", "NA", "A", "B", "C", "D")
  Exp_Result_Table <- bind_cols(cat_test = Test_Per_Cat, exp_cat = Exp_Result)
  test2 <- bmiCat(Test_Per_Cat_Table) == Exp_Result_Table
  return(test2)
}

catTest()

Test_Per_Cat <- c(-1, 0, 1, 18.5, 25, 30)
Test_Per_Cat_Table <- bind_cols(cat_test = Test_Per_Cat)
demo <- bmiCat(Test_Per_Cat_Table)
demo

# This test actually revealed a bug in my conditionals!
