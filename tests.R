
####################### Tests ##########################


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
