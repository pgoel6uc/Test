library(ggplot2)

# Making a comment change to see if it commits to github
TotExp <- 2000
TotSteps <- 500
eps <- 0
q_star <- rnorm(10,0,1)
# Q stores the current estimate of the reward. So Q[2,5] is the estimated reward for selecting action 5 'before' 2 steps
Q <- matrix(nrow=TotSteps+1, ncol=10)
# We initialize Q values with 50 for step 0. A sufficiently large number so that all states are explored at least once
Q[1,] <- rep(5, times=10)

# N stores the number of times each state has been visited. This helps in updating the expected reward value
N <- rep(0,times=10)

# A would store the steps taken
A <- rep(0, times=TotSteps)

# R is the realized reward matrix. Each row represents the new experiment, where each column represents the step
R <- matrix(nrow=TotExp, ncol=TotSteps)

for (Exp in 1:TotExp) {

  for (step in 1:TotSteps) {
    # A[step] is the action that would be taken at this step
    Prob <- runif(1,0,1)
    if (Prob < eps) {
      A[step] <- floor(runif(1,1,11))
    } else {
      A[step] <- max(which(Q[step,]==max(Q[step,])))
    }
    R[Exp, step] <- q_star[A[step]] + rnorm(1,0,1)
    N[A[step]] <- N[A[step]]+1
    for (i in 1:10) {
      if (i == A[step]) {
        Q[step+1,A[step]] <- (Q[step,A[step]]*N[A[step]] + R[Exp, step])/(1+N[A[step]])
      } else {
        Q[step+1,i] <- Q[step,i]
      }
    }
  }
}
RSum <- apply(R,MARGIN=2,mean)
q_star

plot(RSum, type='n')
lines(RSum)
mean(RSum[200:500])
RSum0 <- RSum

ToPlot <- as.data.frame(cbind(RSum0, RSum001, RSum01, RSum03))
ToPlot$x <- as.numeric(rownames(ToPlot))
ggplot(data = ToPlot, aes(x=x, y=RSum0, color='black')) + geom_line()  + geom_line(data=ToPlot, aes(x=x, y=RSum001, color='red')) + geom_line(data=ToPlot, aes(x=x, y=RSum01, color='blue')) + geom_line(data=ToPlot, aes(x=x, y=RSum03, color='green'))
