# This simulates the effects of random errors in marking ballots
# Goal is to see whether, given results of 2016 presidential election
# in WI, MI, & PA

clinton = rep.int(1, 2500000)
trump = rep.int(-1, 2500000)

wi = matrix(sample(c(clinton, trump)))

# Probability that a ballot gets assigned to wrong candidate
# 0 to 1 by .01
flip_probability = .10

count_votes = function(x) {
  if (sample(1:100, 1) < (flip_probability * 100 + 1)) { 
    return(x)
  } else {
    return(-x)
  }
}

simulations = function(x) {
  results = NULL
  results = apply(wi, 1, count_votes)
  return(length(results[results == -1])) # Trump votes
}

simulated_counts = replicate(100, simulations())


