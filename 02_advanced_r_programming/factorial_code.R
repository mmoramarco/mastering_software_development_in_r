# Part 1: Factorial Function
#
# The objective of Part 1 is to write a function that computes the factorial of 
# an integer greater than or equal to 0. Recall that the factorial of a number n
# is n * (n-1) * (n - 2) * … * 1. The factorial of 0 is defined to be 1. Before 
# taking on this part of the assignment, you may want to review the section on 
# Functional Programming in this course (you can also read that section here).
# 
# For this Part you will need to write four different versions of the Factorial 
# function:
#
# 1. Factorial_loop: a version that computes the factorial of an integer using 
#    looping (such as a for loop)
# 2. Factorial_reduce: a version that computes the factorial using the reduce() 
#    function in the purrr package. Alternatively, you can use the Reduce() 
#    function in the base package.
# 3. Factorial_func: a version that uses recursion to compute the factorial.
# 4. Factorial_mem: a version that uses memoization to compute the factorial.
#
# After writing your four versions of the Factorial function, use the 
# microbenchmark package to time the operation of these functions and provide a
# summary of their performance. In addition to timing your functions for 
# specific inputs, make sure to show a range of inputs in order to demonstrate 
# the timing of each function for larger inputs.
#
# In order to submit this assignment, please prepare two files:
#  
# 1. factorial_code.R: an R script file that contains the code implementing your
#    classes, methods, and generics for the longitudinal dataset.
# 2. factorial_output.txt: a text file that contains the results of your 
#    comparison of the four different implementations.

factorial_loop <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    results <- 1
    for (i in 1:n) {
      results <- results * i
    }
    return(results)
  }
}

library(purrr)
factorial_reduce <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    reduce(as.double(1:n), function(x,y) {
      x * y
    })
  }
}

factorial_func <- function(n) {
  if(n == 0) {
    return(1)
  } else {
    return(n * factorial_func(n-1))
  }
}

factorial_lookup <- cbind(1:50,sapply(c(1:50),factorial_loop))

factorial_mem <- function(n) {
  if(n == 0) {
    return(1)
  } else if (n %in% factorial_lookup[,1]) {
    return(factorial_lookup[factorial_lookup[,1] == n,2])
  } else {
    result <- factorial_loop(n)
    factorial_lookup <<- rbind(factorial_lookup,c(n,result))
    return(result)
  }
}

library(microbenchmark)
microbenchmark(factorial_loop(10),
               factorial_reduce(10),
               factorial_func(10),
               factorial_mem(10),
               factorial_loop(50),
               factorial_reduce(50),
               factorial_func(50),
               factorial_mem(50),
               factorial_loop(100),
               factorial_reduce(100),
               factorial_func(100),
               factorial_mem(100))
