#1st step: Define 4 functions
#First function:
fibonacci <- function(user_number)
{#if input number is equal to 1, then return 0
  if (user_number == 1) {x <- c(0)}
  #if input number is equal to 2, then return (0,1)
  else if (user_number == 2) {x <- c(0,1)} 
  #if input number is greater than 2, then return (0,1,(n-2+n-1)...)
  else 
  {x <- c(0,1)
  for (i in 3:user_number) 
  {
    y <- i-2
    z <- i-1
    x[i] <- x[y]+x[z]
  }
  }
  #Print out the result
  cat(x)
}

#Second function:
fizzbuzz <- function(user_number)
{x <- c()
for (i in 1:user_number)
{#if the number can be divided by 15, then return "FizzBuzz"
  if (i %% 15 == 0)
  {x[i] = "FizzBuzz"}
  #if the number can be divided by 3, then return "Fizz"
  else if (i %% 3 == 0)
  {x[i] = "Fizz"}
  #if the number can be divided by 5, then return "Buzz"
  else if (i %% 5 == 0)
  {x[i] = "Buzz"}
  #if the number cannot be divided by (3,5,15), then return the number
  else
  {x[i] = i}
}
#Print out the result
cat(x)
}

#Third function:
calprime <-function(user_number)
{#if the number is equal to 1, then there is no prime number
  if (user_number == 1)
  {cat("No prime number")}
  #if the number is equal to 2, then 2 is the only one prime number
  else if (user_number == 2)
  {prime <- 2
  cat(prime)}
  #if the number is greater than 2, then as long as the number cannot be 
  #divided by all the numbers up to the given number, then the number is a 
  #prime number
  else
  {prime <- c(2)
  for (i in 3:user_number)
  {if (any(i %% 2:(i-1) == 0)) 
  {next}
    else
    {prime[length(prime)+1] <- i}
  }
  #Print out the list of the prime numbers
  cat(prime)
  }
}

#Fourth function:
nvector <- function(user_number)
{x <- sample(-50:50,user_number,replace = TRUE)
#Print out the result
cat(x)}


#2nd step: Repeat the game depending on the answer of the user whether to quit or calculate again
repeat
{
  #Print out the menu of options
  print("Welcomne to R calculation", quote = FALSE)
  cat("Below is the menu of the 4 calculations:
      1.Fibonacci number
      2.FizzBuzz
      3.Calculate primes number
      4.A vector with n numbers\n")
  
  #User input a choice and check whether the number is in (1,2,3,4)
  user_select <- 0
  while (!(user_select %in% c(1,2,3,4))) 
  {user_select <- as.integer(readline(prompt = "Please choose 1 from the menu: "))}
  
  #User input a number and check whether the number is greater than 0
  user_number <- -1
  while (user_number < 0 || is.na(as.numeric(user_number)) == TRUE)
  {user_number <- as.integer(readline(prompt = "Then please input an integer: "))}
  
  
  #Print out a menu for user to choose
  switch(user_select,
         fibonacci(user_number),
         fizzbuzz(user_number),
         calprime(user_number),
         nvector(user_number))
  
  #Ask user whether to quit or play again
  user_quit_check <- readline(prompt = "Would you like to calculate again? Enter y or n ")
  if (tolower(user_quit_check) == "n") {break}
}


