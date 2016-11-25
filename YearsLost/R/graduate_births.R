
# using Silvia Rizzi's method
graduateBirths <- function(
		BxN, 
		BxAge = seq(10, 50, by = 5),
		Ex, 
		ExAge = 1:length(Ex) - 1, 
		lambda = 1, 
		alpha = 10, 
		beta = 50, 
		width = unique(diff(BxAge))){
  # pick out vectors

  # set Bx as small as possible for pclm function
  ind      <- BxN == 0
  BxN[ind] <- 0.001 

  # how many abridged age classes
  n        <- length(BxN)
  a        <- alpha:(beta-1)
  
  a2       <- alpha:(max(BxAge) + width - 1)
  # cut down Ex:
  Ex       <- Ex[ExAge %in% a2]
  
  # create identity matrix
  m        <- length(a2)
  B        <- diag(m)

  # create compostion matrix
  C        <-  kronecker(diag(n), matrix(1, 1, width))

  # examine this
 # C        <-  C[,1:41] #reduce it...-> same dimensions like B
  C2       <- t(t(C) * Ex)
  # repeat, splitting counts, but spitting back rates.
  fxhat    <- pclm(y = BxN, C2, B, lambda=10000, deg = 2, show = FALSE)$gamma
  
  # cut down to alpha beta range
  fxhat[!a2 %in% a] <- 0
 
  # spit back
  out      <- data.frame(Age = a2, Bx = fxhat * Ex, Ex = Ex, Fx = fxhat)
  out      <- out[a2 %in% a, ]
  return(out)
}
