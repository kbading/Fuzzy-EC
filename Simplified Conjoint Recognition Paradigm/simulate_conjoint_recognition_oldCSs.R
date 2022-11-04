
#Load (or install and load) packages we will use
library(pacman) 
p_load('tidyverse', 'jsonlite', 'data.table', 'stringr', 'psych', 'MPTinR') 
library(papaja)
library(afex)


id <- rep(1:100, each = 30)
probe <- rep(c(rep("same US",10),rep("same US valence",10),rep("new pair",10)),100)
verbatim <- rep(NA,3000)
gist <- rep(NA,3000)
guessing_old <- rep(NA,3000)
guessing_OP  <- rep(NA,3000)
response     <- rep(NA,3000)

simulated_responses <- data.frame(id
                                  ,probe
                                  ,verbatim
                                  ,gist
                                  ,guessing_old
                                  ,guessing_OP
                                  ,response)

V_p <- .5
V_r <- .5
G_p <- 0
G_r <- 0
b <- .5
a <- .5



V_u <- .5
G_u <- 0

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$probe[i]== "same US"){
    simulated_responses$verbatim[i] <- rbinom(1,1,V_p)
    simulated_responses$gist[i] <- rbinom(1,1,G_p)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_OP[i] <- rbinom(1,1,a)
  }
  if(simulated_responses$probe[i]== "same US valence"){
    simulated_responses$verbatim[i] <- rbinom(1,1,V_r)
    simulated_responses$gist[i] <- rbinom(1,1,G_r)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_OP[i] <- rbinom(1,1,a)
  }
  if(simulated_responses$probe[i]== "new pair"){
    simulated_responses$verbatim[i] <- rbinom(1,1,V_u)
    simulated_responses$gist[i] <- rbinom(1,1,G_u)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_OP[i] <- rbinom(1,1,a)
  }
  
}

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$probe[i] == "same US"){
    if(simulated_responses$verbatim[i] == 1){
      simulated_responses$response[i] <- "same US"
    }
    if(simulated_responses$verbatim[i] == 0){
      if(simulated_responses$gist[i] == 1){
        if(simulated_responses$guessing_OP[i] == 1){
          simulated_responses$response[i] <- "same US"
        }
        if(simulated_responses$guessing_OP[i] == 0){
          simulated_responses$response[i] <- "same US valence"
        }
      }
      if(simulated_responses$gist[i] == 0){
        if(simulated_responses$guessing_old[i] == 1){
          if(simulated_responses$guessing_OP[i] == 1){
            simulated_responses$response[i] <- "same US"
          }
          if(simulated_responses$guessing_OP[i] == 0){
            simulated_responses$response[i] <- "same US valence"
          }
        }
        if(simulated_responses$guessing_old[i] == 0){
          simulated_responses$response[i] <- "new pair"
        }
      }
    }
  }
  if(simulated_responses$probe[i] == "same US valence"){
    if(simulated_responses$verbatim[i] == 1){
      simulated_responses$response[i] <- "same US valence"
    }
    if(simulated_responses$verbatim[i] == 0){
      if(simulated_responses$gist[i] == 1){
        if(simulated_responses$guessing_OP[i] == 1){
          simulated_responses$response[i] <- "same US"
        }
        if(simulated_responses$guessing_OP[i] == 0){
          simulated_responses$response[i] <- "same US valence"
        }
      }
      if(simulated_responses$gist[i] == 0){
        if(simulated_responses$guessing_old[i] == 1){
          if(simulated_responses$guessing_OP[i] == 1){
            simulated_responses$response[i] <- "same US"
          }
          if(simulated_responses$guessing_OP[i] == 0){
            simulated_responses$response[i] <- "same US valence"
          }
        }
        if(simulated_responses$guessing_old[i] == 0){
          simulated_responses$response[i] <- "new pair"
        }
      }
    }
  }
  if(simulated_responses$probe[i] == "new pair"){
    if(simulated_responses$verbatim[i] == 1){
      simulated_responses$response[i] <- "new pair"
    }
    if(simulated_responses$verbatim[i] == 0){
      if(simulated_responses$gist[i] == 1){
        simulated_responses$response[i] <- "new pair"
      }
      if(simulated_responses$gist[i] == 0){
        if(simulated_responses$guessing_old[i] == 1){
          if(simulated_responses$guessing_OP[i] == 1){
            simulated_responses$response[i] <- "same US"
          }
          if(simulated_responses$guessing_OP[i] == 0){
            simulated_responses$response[i] <- "same US valence"
          }
        }
        if(simulated_responses$guessing_old[i] == 0){
          simulated_responses$response[i] <- "new pair"
        }
      }
    }
  }
}

simulated_responses$probe <- factor(simulated_responses$probe, levels = c("same US","same US valence","new pair"))
simulated_responses$response <- factor(simulated_responses$response, levels = c("same US","same US valence","new pair"))

freq_choice= simulated_responses %>% 
  group_by(probe, response) %>% 
  summarise(freq_eval = n()) 

#check if the .eqn file is correct
check.mpt("conjoint_recognition.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main= fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "conjoint_recognition.eqn")
model_mpt_main

