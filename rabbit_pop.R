
rabbit_pop = function(fertility, survivability, initial, nstep) {
  
  nclasses = length(fertility)
  if ((nclasses!=length(survivability) ))
  { return("NA") }
  
  if ((nclasses!=length(initial) ))
  { return("NA") }
  leslie_matrix = matrix(nrow=nclasses, ncol=nclasses)
  leslie_matrix[,] = 0.0
  leslie_matrix[1,] = fertility
  for (i in 1:(nclasses-1)) {
    leslie_matrix[i+1,i] = survivability[i]
  }
  leslie_matrix[nclasses,nclasses] = survivability[nclasses]
  structure = matrix(nrow=nclasses, ncol=nstep)
  total_pop = rep(0, times=nstep)
  structure[,1] = initial
  
  for (i in 2:nstep) {
    
    total_pop[i]=sum(structure[,i-1])
   structure[,i] = leslie_matrix %*% structure[,i-1]
    
  }
  
  return(list(popbyage=structure, poptot=total_pop))
}

