##easy function to calculate roots of any quadratic

##function takes in the scaled coordinates (so make the coefficient of x^2 1)
##and returns the roots

solve_quadratic = function(b_prime, c_prime, a_prime = 1){
  #scale the coefficients
  if (a_prime != 1){
    b_prime = b_prime/a_prime
    c_prime = c_prime/a_prime}
  
  #expressing midpoint between the two zeroes and their distance
  midpoint = -(b_prime/2)
  distance_sq = midpoint^2 - c_prime
  
  #if complex (ie, distance_sq is negative)
  if (distance_sq < 0) {
    #print("the roots are complex")
    distance_sq = -distance_sq ##to calculate square root
    distance = sqrt(distance_sq)
    root_1 = complex(1, midpoint, distance)
    root_2 = complex(1, midpoint, -distance)
  }
  else {
    #print("the roots are real")
    distance = sqrt(distance_sq)
    root_1 = midpoint + distance
    root_2 = midpoint - distance
  }
  #return the roots
  return(
    list(root1 = root_1, root2 = root_2)
    )
  }
  
solve_quadratic(4, 5, 1)
