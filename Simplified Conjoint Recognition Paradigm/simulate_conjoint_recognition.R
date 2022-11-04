
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
G_p <- .5
G_r <- .5
b <- .5
a <- .5

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
    simulated_responses$verbatim[i] <- 0
    simulated_responses$gist[i] <- 0
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

##Estimate parameters 

#A
#fit six models (one restriction at a time)
#then, compare their fit with the fit of the main, unrestricted model

#main model with c = 0
model_mpt_no_c_low = fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "rcb_extended.eqn"
                         ,restrictions.filename = list("c_low=0"))

model_mpt_no_c_high = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "rcb_extended.eqn"
                          ,restrictions.filename = list("c_high=0"))
#main model with r = 0
model_mpt_no_r_low = fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "rcb_extended.eqn"
                         ,restrictions.filename = list("r_low=0"))

model_mpt_no_r_high = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "rcb_extended.eqn"
                          ,restrictions.filename = list("r_high=0"))

#main model with b = .5
model_mpt_b_low50 = fit.mpt(data = freq_choice$freq_eval
                        ,model.filename = "rcb_extended.eqn"
                        ,restrictions.filename = list("b_low=.5"))

model_mpt_b_high50 = fit.mpt(data = freq_choice$freq_eval
                         ,model.filename = "rcb_extended.eqn"
                         ,restrictions.filename = list("b_high=.5"))

#test if nested models increase/decrease model fit

##C parameter
#delta G^2 model_no_c - main_model
pchisq(q = model_mpt_no_c_low$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_no_c_low$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

pchisq(q = model_mpt_no_c_high$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_no_c_high$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#R parameter
#delta G^2 model_no_r - main_model
pchisq(q = model_mpt_no_r_low$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_no_r_low$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

pchisq(q = model_mpt_no_r_high$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_no_r_high$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#B parameter
#delta G^2 model_b=.5 - main_model
pchisq(q = model_mpt_b_low50$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_b_low50$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

pchisq(q = model_mpt_b_high50$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_b_high50$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#B
#fit models one restriction at a time through the two conditions of the Time at judgment factor
#then, compare their fit with the fit of the main, unrestricted model

#main model with cs = cl
model_mpt_clow_chigh = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "rcb_extended.eqn"
                          ,restrictions.filename = list("c_low=c_high"))
#main model with rs = rl
model_mpt_rlow_rhigh = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "rcb_extended.eqn"
                          ,restrictions.filename = list("r_low=r_high"))
#main model with bs = bl
model_mpt_blow_bhigh = fit.mpt(data = freq_choice$freq_eval
                          ,model.filename = "rcb_extended.eqn"
                          ,restrictions.filename = list("b_low=b_high"))

#C parameter
#delta G^2 model_cs=cl - main_model
pchisq(q = model_mpt_clow_chigh$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_clow_chigh$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#R parameter
#delta G^2 model_rs_rl - main_model
pchisq(q = model_mpt_rlow_rhigh$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_rlow_rhigh$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)

#B parameter
#delta G^2 model_bs_bl - main_model
pchisq(q = model_mpt_blow_bhigh$goodness.of.fit[[2]] - model_mpt_main$goodness.of.fit[[2]]
       ,df = model_mpt_blow_bhigh$goodness.of.fit[[3]] - model_mpt_main$goodness.of.fit[[3]]
       ,lower.tail = FALSE)
