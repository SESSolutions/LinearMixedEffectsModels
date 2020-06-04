AsymUnbalancedData<- function(DATA_GROUP,PRE_SET_MAX,STEP_SIZE,FROM )
{

##Header File
library(nlme)
 
## Inputs
#DATA_GROUP  <<- c(100) # Give as input to the grouping factor - number of datasets 
# PRE_SET_MAX <- c(9)
# STEP_SIZE <-c(0.5)
# FROM <-c(0)
  
## Creation and Initialization of Matrix for Calculation of Samples. 
UNBALANCED_CD4<<- matrix(nrow=DATA_GROUP,ncol=PRE_SET_MAX, byrow=TRUE)
WEEK <- matrix(nrow=DATA_GROUP,ncol=PRE_SET_MAX, byrow=TRUE)
GROUPINGS <- matrix(nrow=DATA_GROUP,ncol=PRE_SET_MAX, byrow=TRUE)
TRTART <- matrix(nrow=DATA_GROUP,ncol=PRE_SET_MAX, byrow=TRUE)
j <-1
i<-1
TimePoints <- 0

B.Intercept <- 27
B.Time <- -0.2
B.Xi <- -2
B.X.time <- -0.15

##Assuming Correlated Random Effects and sampling from multivariate normal distribution 
S.Intercept <- 3*3
S.Slope<- 0.1*0.1
Corr.01 <- -0.75
Cov.01 <- Corr.01*3*0.1
Sigma <- matrix(c(S.Intercept,Cov.01,Cov.01,S.Slope),ncol=2,nrow=2) 
#S.Normal <- rmvnorm(N_Data_groups,c(0,0),Sigma)
#R.Intercept <-S.Normal(,1)
#R.Slope <- S.Normal(,2)
U<-t(chol(Sigma))

## Random Generation of Samples for Discrete Variable, which is later converted to factors.
Trt <<- matrix(sort(sample(rep(0:1,each=(PRE_SET_MAX*DATA_GROUP/2)))),nrow=DATA_GROUP, ncol=PRE_SET_MAX,byrow=TRUE) #matrix(runif(PRE_SET_MAX*DATA_GROUP,min=0, max=1),nrow=DATA_GROUP, ncol=PRE_SET_MAX,byrow=TRUE)  
## Calculation of CD4 using for loop to ensure that each instance is calculated with it corresponding units of variable values.

for(i in 1:DATA_GROUP){
n <- runif(1, min=0, max=1)
n_i1 <- 3*(0 <= n & n <0.2)
n_i2 <- 4*(0.2 <= n & n < 0.4)
n_i3 <- 5*(0.4 <= n & n < 0.6)
n_i4 <- 6*(0.6 <= n & n < 0.8)
n_i5 <- 8*(0.8 <= n & n < 1)
n_i  <- (n_i1 + n_i2 + n_i3 + n_i4 +n_i5) # the number of measurement
TO <- STEP_SIZE*(n_i-1)
TimePoints <- seq(from=FROM,to=TO, by=STEP_SIZE)
b.rand <- U%*%c(rnorm(2,0,1)) # Multivariate Sampling - Just one sample for each random effect
for(j in 1:PRE_SET_MAX) {
    if (j <= n_i)
       { 
            
            Errors <- (0.3*rnorm(1, mean=1.3, sd=1.9)+ 0.7*rnorm(1, mean=-0.6, sd=3.1)) 
            UNBALANCED_CD4[i,j]<- ((B.Intercept + b.rand[1] ) + ((B.Time + b.rand[2])*TimePoints[j]) + (B.Xi*Trt[i,j]) + (B.X.time*Trt[i,j]*TimePoints[j])+ Errors)
            TRTART[i,j] <-Trt[i,j]
            WEEK[i,j] <- TimePoints[j]
            GROUPINGS[i,j] <- i 
            }      
         }           
   }



## Building the Data Frame to contain all dataset in a one frame

Id <- as.vector(t(GROUPINGS)) # converting each matrix to vector, so that they can fit into dataframe structure.

Type <- as.factor(sort(Trt))

Time <- as.vector(t(WEEK))

Response <- as.vector(t(UNBALANCED_CD4))

Dataframe <- data.frame(Id,Type,Time,Response)

NA.in.Dataframe.Removed <-na.omit(Dataframe)

NA.in.Dataframe.Removed[,'Type'] <- as.factor(NA.in.Dataframe.Removed[,'Type'])

return(NA.in.Dataframe.Removed)


}

