# ============================================================
# STAT 206 – Functions Lab 1
# ============================================================


# Task 1 -------------------------------------------------------
# say_hello(): prints a greeting message

say_hello <- function() {
  print("Hello, R programmer!")
}

say_hello()


# Task 2 -------------------------------------------------------
# square(x): returns the square of x

square <- function(x) {
  return(x^2)
}

square(5)   # 25


# Task 3 -------------------------------------------------------
# rectangle_area(length, width): returns area of a rectangle

rectangle_area <- function(length, width) {
  return(length * width)
}

rectangle_area(4, 5)   # 20


# Task 4 -------------------------------------------------------
# power(base, exponent = 2): raises base to exponent (default = 2)

power <- function(base, exponent = 2) {
  return(base^exponent)
}

power(3)      # 9  (3^2)
power(3, 4)   # 81 (3^4)


# Task 5 -------------------------------------------------------
# is_even(n): returns TRUE if n is even, FALSE if odd
# warns if n is not an integer

is_even <- function(n) {
  if (n != as.integer(n)) {
    warning("Input is not an integer.")
    return(NA)
  }
  return(n %% 2 == 0)
}

is_even(4)    # TRUE
is_even(7)    # FALSE
is_even(3.5)  # Warning + NA


# Task 6 -------------------------------------------------------
# grade_score(score): returns a letter grade (A–F) or "Invalid score"

grade_score <- function(score) {
  if (score < 0 || score > 100) {
    return("Invalid score")
  } else if (score >= 90) {
    return("A")
  } else if (score >= 80) {
    return("B")
  } else if (score >= 70) {
    return("C")
  } else if (score >= 60) {
    return("D")
  } else {
    return("F")
  }
}

grade_score(95)   # A
grade_score(82)   # B
grade_score(55)   # F
grade_score(105)  # Invalid score


# Task 7 -------------------------------------------------------
# my_sum(vec): computes the sum of a numeric vector using a for loop

my_sum <- function(vec) {
  total <- 0
  for (val in vec) {
    total <- total + val
  }
  return(total)
}

my_sum(c(1, 2, 3, 4, 5))   # 15


# Task 8 -------------------------------------------------------
# factorial_loop(n): computes n! using a loop (n is a non-negative integer)

factorial_loop <- function(n) {
  result <- 1
  if (n == 0) return(1)
  for (i in 1:n) {
    result <- result * i
  }
  return(result)
}

factorial_loop(0)   # 1
factorial_loop(5)   # 120
factorial_loop(7)   # 5040


# Task 9 -------------------------------------------------------
# mean_sd(vec): returns mean and standard deviation as a named list

mean_sd <- function(vec) {
  return(list(mean = mean(vec), sd = sd(vec)))
}

mean_sd(c(2, 4, 6, 8, 10))


# Task 10 ------------------------------------------------------
# min_max_range(vec): returns min, max, and range as a named list

min_max_range <- function(vec) {
  return(list(
    min   = min(vec),
    max   = max(vec),
    range = max(vec) - min(vec)
  ))
}

min_max_range(c(3, 7, 1, 9, 4))


# Task 11 ------------------------------------------------------
# Temperature conversion functions

fahr_to_celsius <- function(f) {
  return((f - 32) * 5 / 9)
}

celsius_to_fahr <- function(c) {
  return((c * 9 / 5) + 32)
}

# convert_temp(temp, unit = "C"):
#   unit = "C" → input is Celsius, returns Fahrenheit
#   unit = "F" → input is Fahrenheit, returns Celsius

convert_temp <- function(temp, unit = "C") {
  if (unit == "C") {
    return(celsius_to_fahr(temp))
  } else if (unit == "F") {
    return(fahr_to_celsius(temp))
  } else {
    stop("unit must be 'C' or 'F'")
  }
}

convert_temp(100, "C")   # 212 (Fahrenheit)
convert_temp(32,  "F")   # 0   (Celsius)


# Task 12 ------------------------------------------------------
# standardize(vec): z-score standardization
# if all values are identical (zero variance), returns a vector of zeros

standardize <- function(vec) {
  if (sd(vec) == 0) {
    return(rep(0, length(vec)))
  }
  return((vec - mean(vec)) / sd(vec))
}

standardize(c(2, 4, 6, 8, 10))
standardize(c(5, 5, 5, 5))   # all zeros


# Task 13 ------------------------------------------------------
# is_prime(n): returns TRUE if n is prime (n is a positive integer > 1)

is_prime <- function(n) {
  if (n < 2) return(FALSE)
  for (i in 2:floor(sqrt(n))) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}

is_prime(2)    # TRUE
is_prime(13)   # TRUE
is_prime(15)   # FALSE


# Task 14 ------------------------------------------------------
# roll_dice(n, sides = 6): simulates rolling n dice
# Returns: individual rolls, sum of rolls, proportion >= target (default = sides)

roll_dice <- function(n, sides = 6, target = sides) {
  rolls <- sample(1:sides, size = n, replace = TRUE)
  return(list(
    rolls      = rolls,
    total      = sum(rolls),
    proportion = mean(rolls >= target)
  ))
}

set.seed(42)
roll_dice(10)           # 10 standard dice
roll_dice(5, sides = 8) # 5 eight-sided dice


# Task 15 ------------------------------------------------------
# repeat_operation(f, x, n): applies function f to x, n times repeatedly

repeat_operation <- function(f, x, n) {
  result <- x
  for (i in 1:n) {
    result <- f(result)
  }
  return(result)
}

# Using square() from Task 2:
# square(square(square(2))) = ((2^2)^2)^2 = 256
repeat_operation(square, 2, 3)   # 256
