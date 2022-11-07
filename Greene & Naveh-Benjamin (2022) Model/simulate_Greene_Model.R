
#Load (or install and load) packages we will use
library(pacman) 
p_load('tidyverse', 'jsonlite', 'data.table', 'stringr', 'psych', 'MPTinR') 
library(papaja)
library(afex)


id <- rep(1:100, each = 40)
probe <- rep(c(rep("I",10),rep("R",10),rep("UW",10),rep("UO",10)),100)
verbatim <- rep(NA,4000)
gist <- rep(NA,4000)
fuzzy <- rep(NA,4000)
guessing_intact <- rep(NA,4000)
guessing_old <- rep(NA,4000)
guessing_intact_old  <- rep(NA,4000)
response     <- rep(NA,4000)

simulated_responses <- data.frame(id
                                  ,probe
                                  ,verbatim
                                  ,gist
                                  ,fuzzy
                                  ,guessing_intact
                                  ,guessing_old
                                  ,guessing_intact_old
                                  ,response)

V_i <- .5
V_r <- .5
G_i <- .5
G_r <- .5
f <- .5
a <- .5
b <- .5
a_b <- .5

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$probe[i]== "I"){
    simulated_responses$verbatim[i] <- rbinom(1,1,V_i)
    simulated_responses$gist[i] <- rbinom(1,1,G_i)
    simulated_responses$fuzzy[i] <- 0
    simulated_responses$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses$probe[i]== "R"){
    simulated_responses$verbatim[i] <- rbinom(1,1,V_r)
    simulated_responses$gist[i] <- rbinom(1,1,G_r)
    simulated_responses$fuzzy[i] <- 0
    simulated_responses$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses$probe[i]== "UW"){
    simulated_responses$verbatim[i] <- 0
    simulated_responses$gist[i] <- 0
    simulated_responses$fuzzy[i] <- rbinom(1,1,f)
    simulated_responses$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses$probe[i]== "UO"){
    simulated_responses$verbatim[i] <- 0
    simulated_responses$gist[i] <- 0
    simulated_responses$fuzzy[i] <- 0
    simulated_responses$guessing_intact[i] <- 0
    simulated_responses$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  
}

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$probe[i] == "I"){
    if(simulated_responses$verbatim[i] == 1){
      simulated_responses$response[i] <- "intact"
    }
    if(simulated_responses$verbatim[i] == 0){
      if(simulated_responses$gist[i] == 1){
        if(simulated_responses$guessing_intact[i] == 1){
          simulated_responses$response[i] <- "intact"
        }
        if(simulated_responses$guessing_intact[i] == 0){
          simulated_responses$response[i] <- "related"
        }
      }
      if(simulated_responses$gist[i] == 0){
        if(simulated_responses$guessing_old[i] == 1){
          if(simulated_responses$guessing_intact_old[i] == 1){
            simulated_responses$response[i] <- "intact"
          }
          if(simulated_responses$guessing_intact_old[i] == 0){
            simulated_responses$response[i] <- "related"
          }
        }
        if(simulated_responses$guessing_old[i] == 0){
          simulated_responses$response[i] <- "unrelated"
        }
      }
    }
  }
  if(simulated_responses$probe[i] == "R"){
    if(simulated_responses$verbatim[i] == 1){
      simulated_responses$response[i] <- "related"
    }
    if(simulated_responses$verbatim[i] == 0){
      if(simulated_responses$gist[i] == 1){
        if(simulated_responses$guessing_intact[i] == 1){
          simulated_responses$response[i] <- "intact"
        }
        if(simulated_responses$guessing_intact[i] == 0){
          simulated_responses$response[i] <- "related"
        }
      }
      if(simulated_responses$gist[i] == 0){
        if(simulated_responses$guessing_old[i] == 1){
          if(simulated_responses$guessing_intact_old[i] == 1){
            simulated_responses$response[i] <- "intact"
          }
          if(simulated_responses$guessing_intact_old[i] == 0){
            simulated_responses$response[i] <- "related"
          }
        }
        if(simulated_responses$guessing_old[i] == 0){
          simulated_responses$response[i] <- "unrelated"
        }
      }
    }
  }
  if(simulated_responses$probe[i] == "UW"){
    if(simulated_responses$fuzzy[i] == 1){
      if(simulated_responses$guessing_intact[i] == 1){
        simulated_responses$response[i] <- "intact"
      }
      if(simulated_responses$guessing_intact[i] == 0){
        simulated_responses$response[i] <- "related"
      }
    }
    if(simulated_responses$fuzzy[i] == 0){
      if(simulated_responses$guessing_old[i] == 1){
        if(simulated_responses$guessing_intact_old[i] == 1){
          simulated_responses$response[i] <- "intact"
        }
        if(simulated_responses$guessing_intact_old[i] == 0){
          simulated_responses$response[i] <- "related"
        }
      }
      if(simulated_responses$guessing_old[i] == 0){
        simulated_responses$response[i] <- "unrelated"
      }
    }
  }
  if(simulated_responses$probe[i] == "UO"){
    if(simulated_responses$guessing_old[i] == 1){
      if(simulated_responses$guessing_intact_old[i] == 1){
        simulated_responses$response[i] <- "intact"
      }
      if(simulated_responses$guessing_intact_old[i] == 0){
        simulated_responses$response[i] <- "related"
      }
    }
    if(simulated_responses$guessing_old[i] == 0){
      simulated_responses$response[i] <- "unrelated"
    }
  }
}

simulated_responses$probe <- factor(simulated_responses$probe, levels = c("I","R","UW","UO"))
simulated_responses$response <- factor(simulated_responses$response, levels = c("intact","related","unrelated"))

freq_choice= simulated_responses %>% 
  group_by(probe, response) %>% 
  summarise(freq_eval = n()) 

#check if the .eqn file is correct
check.mpt("Greene_Model.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main= fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "Greene_Model.eqn")
model_mpt_main

































































####### Simulate data based on alternative model ########

# no fuzzy trace, only verbatim and gist for all four probe types



id <- rep(1:100, each = 40)
probe <- rep(c(rep("I",10),rep("R",10),rep("UW",10),rep("UO",10)),100)
verbatim <- rep(NA,4000)
gist <- rep(NA,4000)
fuzzy <- rep(NA,4000)
guessing_intact <- rep(NA,4000)
guessing_old <- rep(NA,4000)
guessing_intact_old  <- rep(NA,4000)
response     <- rep(NA,4000)

simulated_responses_AM <- data.frame(id
                                  ,probe
                                  ,verbatim
                                  ,gist
                                  ,fuzzy
                                  ,guessing_intact
                                  ,guessing_old
                                  ,guessing_intact_old
                                  ,response)

V_i <- .5
V_r <- .5
G_i <- .5
G_r <- .5
f <- .5
a <- .5
b <- .5
a_b <- .5

V_uw <- .5
V_uo <- .5
G_uw <- .5
G_uo <- .5

for(i in 1:nrow(simulated_responses_AM)){
  if(simulated_responses_AM$probe[i]== "I"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_i)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_i)
    simulated_responses_AM$fuzzy[i] <- 0
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "R"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_r)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_r)
    simulated_responses_AM$fuzzy[i] <- 0
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "UW"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_uw)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_uw)
    simulated_responses_AM$fuzzy[i] <- 0
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "UO"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_uo)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_uo)
    simulated_responses_AM$fuzzy[i] <- 0
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  
}

for(i in 1:nrow(simulated_responses_AM)){
  if(simulated_responses_AM$probe[i] == "I"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "intact"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        if(simulated_responses_AM$guessing_intact[i] == 1){
          simulated_responses_AM$response[i] <- "intact"
        }
        if(simulated_responses_AM$guessing_intact[i] == 0){
          simulated_responses_AM$response[i] <- "related"
        }
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$guessing_old[i] == 1){
          if(simulated_responses_AM$guessing_intact_old[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact_old[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$guessing_old[i] == 0){
          simulated_responses_AM$response[i] <- "unrelated"
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "R"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "related"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        if(simulated_responses_AM$guessing_intact[i] == 1){
          simulated_responses_AM$response[i] <- "intact"
        }
        if(simulated_responses_AM$guessing_intact[i] == 0){
          simulated_responses_AM$response[i] <- "related"
        }
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$guessing_old[i] == 1){
          if(simulated_responses_AM$guessing_intact_old[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact_old[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$guessing_old[i] == 0){
          simulated_responses_AM$response[i] <- "unrelated"
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "UW"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "unrelated"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        simulated_responses_AM$response[i] <- "unrelated"
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$guessing_old[i] == 1){
          if(simulated_responses_AM$guessing_intact_old[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact_old[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$guessing_old[i] == 0){
          simulated_responses_AM$response[i] <- "unrelated"
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "UO"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "unrelated"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        simulated_responses_AM$response[i] <- "unrelated"
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$guessing_old[i] == 1){
          if(simulated_responses_AM$guessing_intact_old[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact_old[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$guessing_old[i] == 0){
          simulated_responses_AM$response[i] <- "unrelated"
        }
      }
    }
  }
}

simulated_responses_AM$probe <- factor(simulated_responses_AM$probe, levels = c("I","R","UW","UO"))
simulated_responses_AM$response <- factor(simulated_responses_AM$response, levels = c("intact","related","unrelated"))

freq_choice= simulated_responses_AM %>% 
  group_by(probe, response) %>% 
  summarise(freq_eval = n()) 

#check if the .eqn file is correct
check.mpt("Greene_Model.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main= fit.mpt(data = freq_choice$freq_eval
                        ,model.filename = "Greene_Model.eqn")
model_mpt_main




####### Simulate data based on alternative model ########

# verbatim, gist and fuzzy traces for all four probe types



id <- rep(1:100, each = 40)
probe <- rep(c(rep("I",10),rep("R",10),rep("UW",10),rep("UO",10)),100)
verbatim <- rep(NA,4000)
gist <- rep(NA,4000)
fuzzy <- rep(NA,4000)
guessing_intact <- rep(NA,4000)
guessing_old <- rep(NA,4000)
guessing_intact_old  <- rep(NA,4000)
response     <- rep(NA,4000)

simulated_responses_AM <- data.frame(id
                                     ,probe
                                     ,verbatim
                                     ,gist
                                     ,fuzzy
                                     ,guessing_intact
                                     ,guessing_old
                                     ,guessing_intact_old
                                     ,response)

V_i <- .5
V_r <- .5
G_i <- .5
G_r <- .5
#f <- .5
a <- .5
b <- .5
a_b <- .5

V_uw <- 0
V_uo <- 0
G_uw <- 0
G_uo <- 0

f_i <- 0
f_r <- 0
f_uw <- 0
f_uo <- 0

for(i in 1:nrow(simulated_responses_AM)){
  if(simulated_responses_AM$probe[i]== "I"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_i)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_i)
    simulated_responses_AM$fuzzy[i] <- rbinom(1,1,f_i)
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "R"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_r)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_r)
    simulated_responses_AM$fuzzy[i] <- rbinom(1,1,f_r)
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "UW"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_uw)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_uw)
    simulated_responses_AM$fuzzy[i] <- rbinom(1,1,f_uw)
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  if(simulated_responses_AM$probe[i]== "UO"){
    simulated_responses_AM$verbatim[i] <- rbinom(1,1,V_uo)
    simulated_responses_AM$gist[i] <- rbinom(1,1,G_uo)
    simulated_responses_AM$fuzzy[i] <- rbinom(1,1,f_uo)
    simulated_responses_AM$guessing_intact[i] <- rbinom(1,1,a)
    simulated_responses_AM$guessing_old[i] <- rbinom(1,1,b)
    simulated_responses_AM$guessing_intact_old[i] <- rbinom(1,1,a_b)
  }
  
}

for(i in 1:nrow(simulated_responses_AM)){
  if(simulated_responses_AM$probe[i] == "I"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "intact"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        if(simulated_responses_AM$guessing_intact[i] == 1){
          simulated_responses_AM$response[i] <- "intact"
        }
        if(simulated_responses_AM$guessing_intact[i] == 0){
          simulated_responses_AM$response[i] <- "related"
        }
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$fuzzy[i] == 1){
          if(simulated_responses_AM$guessing_intact[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$fuzzy[i] == 0){
          if(simulated_responses_AM$guessing_old[i] == 1){
            if(simulated_responses_AM$guessing_intact_old[i] == 1){
              simulated_responses_AM$response[i] <- "intact"
            }
            if(simulated_responses_AM$guessing_intact_old[i] == 0){
              simulated_responses_AM$response[i] <- "related"
            }
          }
          if(simulated_responses_AM$guessing_old[i] == 0){
            simulated_responses_AM$response[i] <- "unrelated"

          }
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "R"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "related"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        if(simulated_responses_AM$guessing_intact[i] == 1){
          simulated_responses_AM$response[i] <- "intact"
        }
        if(simulated_responses_AM$guessing_intact[i] == 0){
          simulated_responses_AM$response[i] <- "related"
        }
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$fuzzy[i] == 1){
          if(simulated_responses_AM$guessing_intact[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$fuzzy[i] == 0){
          if(simulated_responses_AM$guessing_old[i] == 1){
            if(simulated_responses_AM$guessing_intact_old[i] == 1){
              simulated_responses_AM$response[i] <- "intact"
            }
            if(simulated_responses_AM$guessing_intact_old[i] == 0){
              simulated_responses_AM$response[i] <- "related"
            }
          }
          if(simulated_responses_AM$guessing_old[i] == 0){
            simulated_responses_AM$response[i] <- "unrelated"
            
          }
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "UW"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "unrelated"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        simulated_responses_AM$response[i] <- "unrelated"
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$fuzzy[i] == 1){
          if(simulated_responses_AM$guessing_intact[i] == 1){
            simulated_responses_AM$response[i] <- "intact"
          }
          if(simulated_responses_AM$guessing_intact[i] == 0){
            simulated_responses_AM$response[i] <- "related"
          }
        }
        if(simulated_responses_AM$fuzzy[i] == 0){
          if(simulated_responses_AM$guessing_old[i] == 1){
            if(simulated_responses_AM$guessing_intact_old[i] == 1){
              simulated_responses_AM$response[i] <- "intact"
            }
            if(simulated_responses_AM$guessing_intact_old[i] == 0){
              simulated_responses_AM$response[i] <- "related"
            }
          }
          if(simulated_responses_AM$guessing_old[i] == 0){
            simulated_responses_AM$response[i] <- "unrelated"
            
          }
        }
      }
    }
  }
  if(simulated_responses_AM$probe[i] == "UO"){
    if(simulated_responses_AM$verbatim[i] == 1){
      simulated_responses_AM$response[i] <- "unrelated"
    }
    if(simulated_responses_AM$verbatim[i] == 0){
      if(simulated_responses_AM$gist[i] == 1){
        simulated_responses_AM$response[i] <- "unrelated"
      }
      if(simulated_responses_AM$gist[i] == 0){
        if(simulated_responses_AM$fuzzy[i] == 1){
          simulated_responses_AM$response[i] <- "unrelated"
        }
        if(simulated_responses_AM$fuzzy[i] == 0){
          if(simulated_responses_AM$guessing_old[i] == 1){
            if(simulated_responses_AM$guessing_intact_old[i] == 1){
              simulated_responses_AM$response[i] <- "intact"
            }
            if(simulated_responses_AM$guessing_intact_old[i] == 0){
              simulated_responses_AM$response[i] <- "related"
            }
          }
          if(simulated_responses_AM$guessing_old[i] == 0){
            simulated_responses_AM$response[i] <- "unrelated"
            
          }
        }
      }
    }
  }
}

simulated_responses_AM$probe <- factor(simulated_responses_AM$probe, levels = c("I","R","UW","UO"))
simulated_responses_AM$response <- factor(simulated_responses_AM$response, levels = c("intact","related","unrelated"))

freq_choice= simulated_responses_AM %>% 
  group_by(probe, response) %>% 
  summarise(freq_eval = n()) 

#check if the .eqn file is correct
check.mpt("Greene_Model.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main= fit.mpt(data = freq_choice$freq_eval
                        ,model.filename = "Greene_Model.eqn")
model_mpt_main
























