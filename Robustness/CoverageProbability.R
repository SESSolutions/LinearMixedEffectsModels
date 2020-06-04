CoverageProbability <- function(option,iteration, N_Groups, TimeVector, MAX, Step_Size,Start){

#Creation of Vector Variables 
Bool.Vector1 <- vector(mode = "integer", length = iteration)
Bool.Vector2 <- vector(mode = "integer", length = iteration)
Bool.Vector3 <- vector(mode = "integer", length = iteration)
Bool.Vector4 <- vector(mode = "integer", length = iteration)
Bool.Vector5 <- vector(mode = "integer", length = iteration)
Bool.Vector6 <- vector(mode = "integer", length = iteration)
Bool.Vector7 <- vector(mode = "integer", length = iteration)
Bool.Vector8 <- vector(mode = "integer", length = iteration)
Bool.Vector9 <- vector(mode = "integer", length = iteration)
Bool.Vector10 <-vector(mode = "integer", length = iteration)
Bool.Vector11 <-vector(mode = "integer", length = iteration)
Bool.Vector12 <-vector(mode = "integer", length = iteration)
Bool.Vector13 <-vector(mode = "integer", length = iteration)
Bool.Vector14<-vector(mode = "integer", length = iteration)
Bool.Vector15<-vector(mode = "integer", length = iteration)
Bool.Vector16<- vector(mode = "integer", length = iteration)

i <- 1
B0 <- 27
B1 <- -0.2
B2 <- -2
B3 <- - 0.15

  if(option==1){

     #For Balanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- BalancedData(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector1[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector2[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector3[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector4[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( data.lme)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
 
     Bool.Vector1[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector2[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector3[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector4[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
       }

     #For Balanced Without Random Slope

     for(i in 1:iteration){
     data <- BalancedData_RI(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE)
 
     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector5[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector6[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector7[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector8[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector5[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector6[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector7[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector8[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }

       }


 #For Unbalanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- UnbalancedData(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector9[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector10[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector11[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector12[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector9[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector10[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector11[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector12[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
        }

     #For Unbalanced without Random Slope

     for(i in 1:iteration){
     data <- UnbalancedData_RI(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector13[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector14[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector15[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector16[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector13[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector14[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector15[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector16[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
  }

}







if(option==2){

#For Balanced With Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- SymBalancedData(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector1[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector2[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector3[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector4[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector1[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector2[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector3[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector4[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
       }

     #For Balanced Without Random Slope

     for(i in 1:iteration){
     data <- SymBalancedData_RI(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE)
 
     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector5[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector6[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector7[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector8[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector5[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector6[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector7[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector8[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }

       }


     #For Unbalanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- SymUnbalancedData(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector9[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector10[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector11[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector12[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector9[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector10[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector11[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector12[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
        }

     #For Unbalanced without Random Slope

     for(i in 1:iteration){
     data <- SymUnbalancedData_RI(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector13[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector14[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector15[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector16[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector13[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector14[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector15[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector16[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
  }
          }



if(option==3){
#For Balanced With Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- AsymBalancedData(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector1[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector2[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector3[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector4[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector1[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector2[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector3[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector4[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
       }

     #For Balanced Without Random Slope

     for(i in 1:iteration){
     data <- AsymBalancedData_RI(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE)
 
     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector5[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector6[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector7[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector8[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector5[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector6[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector7[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector8[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }

       }


     #For Unbalanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- AsymUnbalancedData(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector9[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector10[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector11[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector12[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector9[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector10[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector11[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector12[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
        }

     #For Unbalanced without Random Slope

     for(i in 1:iteration){
     data <- AsymUnbalancedData_RI(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector13[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector14[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector15[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector16[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector13[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector14[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector15[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector16[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
  }
          }






if(option==4){
#For Balanced With Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- GamBalancedData(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector1[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector2[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector3[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector4[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector1[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector2[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector3[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector4[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
       }

     #For Balanced Without Random Slope

     for(i in 1:iteration){
     data <- GamBalancedData_RI(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE)
 
     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector5[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector6[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector7[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector8[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector5[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector6[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector7[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector8[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }

       }


     #For Unbalanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- GamUnbalancedData(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector9[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector10[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector11[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector12[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector9[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector10[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector11[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector12[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
        }

     #For Unbalanced without Random Slope

     for(i in 1:iteration){
     data <- GamUnbalancedData_RI(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector13[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector14[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector15[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector16[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector13[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector14[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector15[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector16[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
  }
          }




if(option==5){
#For Balanced With Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- ExpoBalancedData(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector1[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector2[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector3[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector4[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector1[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector2[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector3[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector4[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
       }

     #For Balanced Without Random Slope

     for(i in 1:iteration){
     data <- ExpoBalancedData_RI(N_Groups,TimeVector)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE)
 
     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector5[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector6[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector7[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector8[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector5[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector6[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector7[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector8[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }

       }


     #For Unbalanced with Random Intercept and Random Slope

     for(i in 1:iteration){
     data <- ExpoUnbalancedData(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~Time|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector9[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector10[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector11[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector12[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector9[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector10[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector11[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector12[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
        }

     #For Unbalanced without Random Slope

     for(i in 1:iteration){
     data <- ExpoUnbalancedData_RI(N_Groups,MAX,Step_Size,Start)
     data.lme <- try(lme(Response~Time*Type, data=data,random=~1|Id), silent=TRUE)
     interval.BalancedData <-try(intervals(data.lme), silent=TRUE) 

     if (class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'){
     Bool.Vector13[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector14[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector15[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     Bool.Vector16[i] <- (3*(class(data.lme)== 'try-error' | class( interval.BalancedData)== 'try-error'))
     }
     
     else if(class( interval.BalancedData)!= 'try-error' & class( interval.BalancedData)!= 'try-error'){
     Bool.Vector13[i] <- 1*(interval.BalancedData$fix[1,1] < B0 & interval.BalancedData$fix[1,3] > B0)
     Bool.Vector14[i] <- 1*(interval.BalancedData$fix[2,1] < B1 & interval.BalancedData$fix[2,3] > B1)
     Bool.Vector15[i] <- 1*(interval.BalancedData$fix[3,1] < B2 & interval.BalancedData$fix[3,3] > B2)
     Bool.Vector16[i] <- 1*(interval.BalancedData$fix[4,1] < B3 & interval.BalancedData$fix[4,3] > B3)  
     }
  }
          }

B_RIRS<-c(mean(Bool.Vector1[Bool.Vector1!=3]),mean(Bool.Vector2[Bool.Vector2!=3]),mean(Bool.Vector3[Bool.Vector3!=3]),mean(Bool.Vector4[Bool.Vector4!=3]))
U_RIRS <-c(mean(Bool.Vector5[Bool.Vector5!=3]),mean(Bool.Vector6[Bool.Vector6!=3]),mean(Bool.Vector7[Bool.Vector7!=3]),mean(Bool.Vector8[Bool.Vector8!=3]))
B_RI<-c(mean(Bool.Vector9[Bool.Vector9!=3]),mean(Bool.Vector10[Bool.Vector10!=3]),mean(Bool.Vector11[Bool.Vector11!=3]),mean(Bool.Vector12[Bool.Vector12!=3]))
U_RI<-c(mean(Bool.Vector13[Bool.Vector13!=3]),mean(Bool.Vector14[Bool.Vector14!=3]),mean(Bool.Vector15[Bool.Vector15!=3]),mean(Bool.Vector16[Bool.Vector16!=3]))
Coverage.Prob.Matrix <- cbind(B_RIRS,B_RI,U_RIRS,U_RI)
return(signif(Coverage.Prob.Matrix,digits=3)) 
}