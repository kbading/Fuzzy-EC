
#Load (or install and load) packages we will use
library(pacman) 
p_load('tidyverse', 'jsonlite', 'data.table', 'stringr', 'psych', 'MPTinR') 
library(papaja)
library(afex)

n_p <- 1000       # number of participants
n_i <- 24      # number of items per category
n_n <- 48      # number of item for "new" probes
n_c <- 2         # number of categories
n_s <- 4         # number of members per category


id <- rep(1:n_p, each = 2*n_i + n_n)
category <- rep(c(rep("A",n_i),rep("B",n_i),rep("new",n_n)),n_p)
D_x <- rep(NA,(2*n_i+n_n)*n_p)
d_x <- rep(NA,(2*n_i+n_n)*n_p)
c_x <- rep(NA,(2*n_i+n_n)*n_p)
guess_old <- rep(NA,(2*n_i+n_n)*n_p)
guess_a <- rep(NA,(2*n_i+n_n)*n_p)
guess_correctS <- rep(NA,(2*n_i+n_n)*n_p)
response <- rep(NA,(2*n_i+n_n)*n_p)

simulated_responses <- data.frame(id
                                  ,category
                                  ,D_x
                                  ,d_x
                                  ,c_x
                                  ,guess_old
                                  ,guess_a
                                  ,guess_correctS
                                  ,response)



# Simulation 1: D_n > 0
D_a <- .5
D_b <- .5
D_n <- .5

c_a <- .5
c_b <- .5

d_a <- .5
d_b <- .5

b <- .5
a <- .5

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$category[i]== "A"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_a)
    simulated_responses$c_x[i] <- rbinom(1,1,c_a)
    simulated_responses$d_x[i] <- rbinom(1,1,d_a)
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- rbinom(1,1,1/n_s)
  }
  if(simulated_responses$category[i]== "B"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_a)
    simulated_responses$c_x[i] <- rbinom(1,1,c_a)
    simulated_responses$d_x[i] <- rbinom(1,1,d_a)
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- rbinom(1,1,1/n_s)
  }
  if(simulated_responses$category[i]== "new"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_n)
    simulated_responses$c_x[i] <- 0
    simulated_responses$d_x[i] <- 0
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- 0
  }
}

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$category[i] == "new"){
    
    if(simulated_responses$D_x[i] == 1){
      simulated_responses$response[i] <- "new"
    }
    
    if(simulated_responses$D_x[i] == 0){
      
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          simulated_responses$response[i] <- "A,wrong speaker"
        }
        if(simulated_responses$guess_a[i] == 0){
          simulated_responses$response[i] <- "B,wrong speaker"
        }
      }
      
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
      
    }
  }
  if(simulated_responses$category[i] == "A"){
    if(simulated_responses$D_x[i] == 1){
      if(simulated_responses$c_x[i] == 1){
        simulated_responses$response[i] <- "A,correct speaker"
      }
      if(simulated_responses$c_x[i] == 0){
        if(simulated_responses$d_x[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "A,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
        }
        if(simulated_responses$d_x[i] == 0){
          if(simulated_responses$guess_a[i] == 1){
            if(simulated_responses$guess_correctS[i] == 1){
              simulated_responses$response[i] <- "A,correct speaker"
            }
            if(simulated_responses$guess_correctS[i] == 0){
              simulated_responses$response[i] <- "A,wrong speaker"
            }
          }
          if(simulated_responses$guess_a[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }
        }
      }
    }
    if(simulated_responses$D_x[i] == 0){
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "A,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
        }
        if(simulated_responses$guess_a[i] == 0){
          simulated_responses$response[i] <- "B,wrong speaker"
        }
      }
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
    }
  }
  
  if(simulated_responses$category[i] == "B"){
    if(simulated_responses$D_x[i] == 1){
      if(simulated_responses$c_x[i] == 1){
        simulated_responses$response[i] <- "B,correct speaker"
      }
      if(simulated_responses$c_x[i] == 0){
        if(simulated_responses$d_x[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "B,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }
        }
        if(simulated_responses$d_x[i] == 0){
          if(simulated_responses$guess_a[i] == 1){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
          if(simulated_responses$guess_a[i] == 0){
            if(simulated_responses$guess_correctS[i] == 1){
              simulated_responses$response[i] <- "B,correct speaker"
            }
            if(simulated_responses$guess_correctS[i] == 0){
              simulated_responses$response[i] <- "B,wrong speaker"
            }
          }
        }
      }
    }
    if(simulated_responses$D_x[i] == 0){
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          simulated_responses$response[i] <- "A,wrong speaker"
        }
        if(simulated_responses$guess_a[i] == 0){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "B,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }

        }
      }
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
    }
  }
}

simulated_responses$category <- factor(simulated_responses$category, levels = c("A","B","new"))
simulated_responses$response <- factor(simulated_responses$response
                                       , levels = c("A,correct speaker"
                                                    ,"B,correct speaker"
                                                    ,"A,wrong speaker"
                                                    ,"B,wrong speaker"
                                                    ,"new")
                                       )

freq_choice= simulated_responses %>% 
  group_by(category, response) %>% 
  summarise(freq_eval = n()) 


#check if the .eqn file is correct
check.mpt("KW_model.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main = fit.mpt(data = freq_choice$freq_eval
                             ,model.filename = "KW_model.eqn"
                             ,restrictions.filename = list("D_a=D_b=D_n"))
model_mpt_main

model_mpt_D_n_0 = fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "KW_model.eqn"
                         ,restrictions.filename = list("D_a=D_b","D_n=0"))
model_mpt_D_n_0

pchisq(q = model_mpt_D_n_0$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_D_n_0$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#### recap: true D_n > 0
#### result: restriction "D_n = 0" does not produce misfit, but biased parameter estimates
#### --> accurate: a
#### --> overestimation: D_a, D_b
#### --> underestimation: c_a, c_b, d_a, d_b, b





# Simulation 2: D_n = 0
D_a <- .2
D_b <- .2
D_n <- 0

c_a <- .5
c_b <- .5

d_a <- .5
d_b <- .5

b <- .5
a <- .5

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$category[i]== "A"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_a)
    simulated_responses$c_x[i] <- rbinom(1,1,c_a)
    simulated_responses$d_x[i] <- rbinom(1,1,d_a)
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- rbinom(1,1,1/n_s)
  }
  if(simulated_responses$category[i]== "B"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_a)
    simulated_responses$c_x[i] <- rbinom(1,1,c_a)
    simulated_responses$d_x[i] <- rbinom(1,1,d_a)
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- rbinom(1,1,1/n_s)
  }
  if(simulated_responses$category[i]== "new"){
    simulated_responses$D_x[i] <- rbinom(1,1,D_n)
    simulated_responses$c_x[i] <- 0
    simulated_responses$d_x[i] <- 0
    simulated_responses$guess_old[i] <- rbinom(1,1,b)
    simulated_responses$guess_a[i] <- rbinom(1,1,a)
    simulated_responses$guess_correctS[i] <- 0
  }
}

for(i in 1:nrow(simulated_responses)){
  if(simulated_responses$category[i] == "new"){
    
    if(simulated_responses$D_x[i] == 1){
      simulated_responses$response[i] <- "new"
    }
    
    if(simulated_responses$D_x[i] == 0){
      
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          simulated_responses$response[i] <- "A,wrong speaker"
        }
        if(simulated_responses$guess_a[i] == 0){
          simulated_responses$response[i] <- "B,wrong speaker"
        }
      }
      
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
      
    }
  }
  if(simulated_responses$category[i] == "A"){
    if(simulated_responses$D_x[i] == 1){
      if(simulated_responses$c_x[i] == 1){
        simulated_responses$response[i] <- "A,correct speaker"
      }
      if(simulated_responses$c_x[i] == 0){
        if(simulated_responses$d_x[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "A,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
        }
        if(simulated_responses$d_x[i] == 0){
          if(simulated_responses$guess_a[i] == 1){
            if(simulated_responses$guess_correctS[i] == 1){
              simulated_responses$response[i] <- "A,correct speaker"
            }
            if(simulated_responses$guess_correctS[i] == 0){
              simulated_responses$response[i] <- "A,wrong speaker"
            }
          }
          if(simulated_responses$guess_a[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }
        }
      }
    }
    if(simulated_responses$D_x[i] == 0){
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "A,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
        }
        if(simulated_responses$guess_a[i] == 0){
          simulated_responses$response[i] <- "B,wrong speaker"
        }
      }
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
    }
  }
  
  if(simulated_responses$category[i] == "B"){
    if(simulated_responses$D_x[i] == 1){
      if(simulated_responses$c_x[i] == 1){
        simulated_responses$response[i] <- "B,correct speaker"
      }
      if(simulated_responses$c_x[i] == 0){
        if(simulated_responses$d_x[i] == 1){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "B,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }
        }
        if(simulated_responses$d_x[i] == 0){
          if(simulated_responses$guess_a[i] == 1){
            simulated_responses$response[i] <- "A,wrong speaker"
          }
          if(simulated_responses$guess_a[i] == 0){
            if(simulated_responses$guess_correctS[i] == 1){
              simulated_responses$response[i] <- "B,correct speaker"
            }
            if(simulated_responses$guess_correctS[i] == 0){
              simulated_responses$response[i] <- "B,wrong speaker"
            }
          }
        }
      }
    }
    if(simulated_responses$D_x[i] == 0){
      if(simulated_responses$guess_old[i] == 1){
        if(simulated_responses$guess_a[i] == 1){
          simulated_responses$response[i] <- "A,wrong speaker"
        }
        if(simulated_responses$guess_a[i] == 0){
          if(simulated_responses$guess_correctS[i] == 1){
            simulated_responses$response[i] <- "B,correct speaker"
          }
          if(simulated_responses$guess_correctS[i] == 0){
            simulated_responses$response[i] <- "B,wrong speaker"
          }
          
        }
      }
      if(simulated_responses$guess_old[i] == 0){
        simulated_responses$response[i] <- "new"
      }
    }
  }
}

simulated_responses$category <- factor(simulated_responses$category, levels = c("A","B","new"))
simulated_responses$response <- factor(simulated_responses$response
                                       , levels = c("A,correct speaker"
                                                    ,"B,correct speaker"
                                                    ,"A,wrong speaker"
                                                    ,"B,wrong speaker"
                                                    ,"new")
)

freq_choice= simulated_responses %>% 
  group_by(category, response) %>% 
  summarise(freq_eval = n()) 


#check if the .eqn file is correct
check.mpt("KW_model.eqn")

#test the model fit
#all tasks' order - do not fit the data well
model_mpt_main = fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "KW_model.eqn"
                         ,restrictions.filename = list("D_a=D_b","D_n=0"))
model_mpt_main

model_mpt_D_n_fixed = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "KW_model.eqn"
                          ,restrictions.filename = list("D_a=D_b=D_n"))
model_mpt_D_n_fixed

pchisq(q = model_mpt_D_n_fixed$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_D_n_fixed$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#### recap: true D_n = 0
#### result: restriction "D_n = D_a = D_b" produces misfit, and biased parameter estimates
#### --> accurate: a
#### --> underestimation: D_a, D_b
#### --> overestimation: D_n, c_a, c_b, d_a, d_b, b

model_mpt_D_n_free = fit.mpt(data = freq_choice$freq_eval
                              ,model.filename = "KW_model.eqn"
                              ,restrictions.filename = list("D_a=D_b"))
model_mpt_D_n_free

#### result: D_n = free produces misfit, and biased parameter estimates, and very large CIs
#### --> accurate: a
#### --> (slight) underestimation: D_a, D_b
#### --> (slight) overestimation: D_n, c_a, c_b, d_a, d_b, b



