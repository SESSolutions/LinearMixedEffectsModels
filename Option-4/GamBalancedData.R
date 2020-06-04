GamBalancedData<-function(N_Data_groups,FollowUp_Times){

##Header Files
library(nlme) 

##Inputs to the function are as follows: 
# N_Data_Group: Single-Length-vector number such as: N_Data_Group = 400  
# FollowUp_Times = c(0,10,20,30,40)

## Initializations 

N_Measures <-length(FollowUp_Times)
N_Dataset  <-N_Data_groups*N_Measures
LikeForLike.Response <-matrix(nrow=N_Data_groups,ncol=N_Measures)
Week   <- matrix(nrow=N_Data_groups,ncol=N_Measures)
discrete.covariate <-matrix(nrow=N_Data_groups,ncol=N_Measures)
PatId  <- matrix(nrow=N_Data_groups,ncol=N_Measures)
Samples <- matrix(nrow=N_Data_groups,ncol=N_Measures)

##Generating samples for discrete covariate: categorical variable X
    
Discrete.sample <- sample(rep(0:1,each=(N_Dataset/2)))#runif(N_Measures*N_Data_groups, min =0, max=1)
discrete.covariate <- matrix(sort(Discrete.sample), nrow=N_Data_groups, ncol=N_Measures, byrow=TRUE)
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
i<-1
j <-1
for(i in 1:N_Data_groups){
  FollowUp_Times 
  b.rand <- U%*%c(rnorm(2,0,1))                                
  for(j in 1:N_Measures){
     Errors <- (rgamma(1,shape=2,rate=sqrt(1.92/2),)-((2*sqrt(2))/sqrt(1.9))) #Distorted Error Distribution   
     Week[i,j] <- FollowUp_Times[j]
     PatId[i,j] <- i
     LikeForLike.Response[i,j] <- ((B.Intercept + b.rand[1]) + ((B.Time  + b.rand[2])*FollowUp_Times[j])+ (B.Xi*discrete.covariate[i,j])+ (B.X.time*FollowUp_Times[j]*discrete.covariate[i,j])+Errors)# equation 1       
        }                       
 }


discrete.covariate <- as.vector(t(discrete.covariate))

## Arranging the different Data in a Data frame

Response <- as.vector(t(LikeForLike.Response))
Type <- sort(as.factor(discrete.covariate ))   #as.vector(t(Samples))
Time<- as.vector(t(Week))
Id <- as.vector(t(PatId))       
Dataset <- data.frame(Id,Type,Time,Response )
Dataset[,'Type'] <- as.factor(Dataset[,'Type'])
return(Dataset)
}