########### load packages

# install.packages("TreeBUGS")
library("TreeBUGS")
library("ggplot2")

# help files:
#?TreeBUGS
#vignette("TreeBUGS_1_intro")
#vignette("TreeBUGS_2_extended")

# assess heterogeneity graphically
plotFreq("DataperParticipant_CNIModel.csv", eqn = "CNI_modelBase.eqn")
plotFreq("DataperParticipant_CNIModel.csv", eqn = "CNI_modelBase.eqn",
         freq = T, boxplot = FALSE)



########### fit hierarchical CNI model with median respopnse time predictor


### prior distribution for slope parameters beta (relevant for Bayes factors)


IVprec <- 2   # similar to the default rscale=sqrt(2)/2 in BayesFactor package
beta <- rnorm(5000, 0, 1/sqrt(IVprec))
xx <- seq(-3,3,.1)
yy <- sapply(beta, function(b) pnorm(0 + abs(b)*xx))
yy.q <- apply(yy, 1, quantile, probs = c(.5,  .1, .9)) # .025,.975))
# plot median and 80% prediction intervals
plot(xx, yy.q["50%",], type = "l", lwd = 2, ylim = 0:1, las = 1,
     ylab = "MPT parameter", xlab = "z-standardized predictor")
polygon(c(xx, rev(xx)), c(yy.q[2,], rev(yy.q[3,])), col = adjustcolor(1, .3), border = NA)
for (i in 1:100)
  lines(xx, yy[,i], col = adjustcolor(1, .05))


### hierarchical MPT model

# fit model (random seed ensures replicable results)
set.seed(123)
CNI_Time <- traitMPT(eqnfile="CNI_modelBase.eqn",
                            data="DataperParticipant_CNIModel.csv",
                            covData="TimeMedian.csv",
                            predStructure=list("C ; MedianReactionTime",
                                               "N ; MedianReactionTime",
                                               "I ; MedianReactionTime"),
                            n.iter=32000,
                            n.adapt=2000,
                            n.thin=1, # less thinning => more precise Bayes factors
                            n.burnin=2000,
                            n.chain=6,
                            IVprec = IVprec,
                            ppp = 2000,
                            modelfilename = "JAGS_CNI_TimeMedian.txt",
                            parEstFile = "summary_CNI_TimeMedian.txt")
#save(CNI_Time, file ="fit_CNI_Time.RData")
# load("results/fit_CNI_personality.RData")



######### parameter estimates

# show summary statistics:
summary(CNI_Time)
plot(CNI_Time)
plot(CNI_Time, par = "slope")
plotParam(CNI_Time)

# slope parameters beta:
CNI_Time$summary$groupParameters$slope



# Interpretation of regression slopes (+/- 1 SD)
Time_sd <- apply(CNI_Time$mptInfo$covData, 2, sd) # SD of covariates
# standardized slopes:
beta_standardized <- 
  CNI_Time$summary$groupParameters$slope[c("slope_N_MedianReactionTime", "slope_C_MedianReactionTime","slope_I_MedianReactionTime"),1] * Time_sd[c("MedianReactionTime")]
# latent (probit) group mean and SD:
mu <- CNI_Time$summary$groupParameters$mu[,"Mean"]
sig <- CNI_Time$summary$groupParameters$sigma[,"Mean"]

# bivariate probit transform (takes group SD into acount)
# (1 SD below/above group mean)
plusminus_sd <- list(
  N_Time = probitInverse(mu["latent_mu_N"] +
                         beta_standardized["slope_N_MedianReactionTime"] *c(-1,1),
                       sigma = sig["latent_sigma_N"]),
  C_Time = probitInverse(mu["latent_mu_C"] +
                         beta_standardized["slope_C_MedianReactionTime"] *c(-1,1),
                       sigma = sig["latent_sigma_C"]),
  I_Time = probitInverse(mu["latent_mu_I"] +
                         beta_standardized["slope_I_MedianReactionTime"] *c(-1,1),
                       sigma = sig["latent_sigma_I"]))
plusminus_sd



########### model fit &  Bayes factors

# assess model fit with posterior predictive p-values:
#    T1 = Mean structure
#    T2 = Covariance structure
CNI_Time$summary$fitStatistics$overall
plotFit(CNI_Time)
plotFit(CNI_Time, stat = "cov")


# Bayes factors for slope parameters
par(mfrow=c(3,2))
bfs <- list(
  BayesFactorSlope(CNI_Time, parameter = "slope_N_MedianReactionTime", direction = ">", approx = "logspline"),
  BayesFactorSlope(CNI_Time, parameter = "slope_C_MedianReactionTime", direction = ">", approx = "logspline"),
  BayesFactorSlope(CNI_Time, parameter = "slope_I_MedianReactionTime", direction = "<", approx = "logspline"))
print(bfs)



# store results to text file:
sink("BayesFactor_slope_parameters.txt")
print(bfs)
cat("\n\n######### Parameter estimates for beta\n")
round(CNI_Time$summary$groupParameters$slope, 3)
cat("\n\n######### +/-1 SD predictions\n")
print(plusminus_sd)
sink()




########### plot regression models: Time --> N, Time --> C, Time --> I

# Time variables:
TimeMedian <- read.csv("Median.csv")
TimeMedian_ms <- apply(TimeMedian, 2, function(t) c(m=mean(t), sd = sd(t)))

# group-level predictions
CNI_Time$mptInfo$thetaNames

samples_C_Time <-  do.call("rbind",
                         CNI_Time$runjags$mcmc[,c("mu[1]", "slope_C_MedianReactionTime", "sigma[1]")])
samples_N_Time <- do.call("rbind",
                          CNI_Time$runjags$mcmc[,c("mu[3]","slope_N_MedianReactionTime", "sigma[3]")])
samples_I_Time <- do.call("rbind",
                          CNI_Time$runjags$mcmc[,c("mu[2]","slope_I_MedianReactionTime", "sigma[2]")])

sel <- sample(nrow(samples_N_Time), 5000)
xx <- seq(10,50,10)

pred_C <- apply(samples_C_Time[sel,], 1, function(par)
  probitInverse(par[1] + par[2] * (xx - TimeMedian_ms["m", "Median"]), par[3])[,1])
pred_N <- apply(samples_N_Time[sel,], 1, function(par)
  probitInverse(par[1] + par[2] * (xx - TimeMedian_ms["m", "Median"]), par[3])[,1])
pred_I <- apply(samples_I_Time[sel,], 1, function(par)
  probitInverse(par[1] + par[2] * (xx - TimeMedian_ms["m", "Median"]), par[3])[,1])



pred_C.q <- apply(pred_C, 1, quantile, probs = c(0.025, .5, .975))
pred_N.q <- apply(pred_N, 1, quantile, probs = c(0.025, .5, .975))
pred_I.q <- apply(pred_I, 1, quantile, probs = c(0.025, .5, .975))

# individual-level predictions
N <- nrow(TimeMedian)
samples_C <- do.call("rbind", CNI_Time$runjags$mcmc[,paste0("theta[1,", 1:N, "]")])
samples_N <- do.call("rbind", CNI_Time$runjags$mcmc[,paste0("theta[3,", 1:N, "]")])
samples_I <- do.call("rbind", CNI_Time$runjags$mcmc[,paste0("theta[2,", 1:N, "]")])

ind_C <- colMeans(samples_C)
ind_N <- colMeans(samples_N)
ind_I <-colMeans(samples_I)



# plotting
png("CNI_TimeMedianCandN.png", width = 9, height = 4.5, unit = "in", res = 300)
par(mfrow = c(1,2))


plot(xx, pred_C.q["50%",], type = "l", lwd = 2, ylim = 0:1, las = 1,
     main = "(A) Sensitivity to consequences (utilitarian)",
     ylab = "Parameter C", xlab = "Median RT [s]")
abline(v = TimeMedian_ms["m", "Median"], lty = "dashed")
polygon(c(xx, rev(xx)), c(pred_C.q[1,], rev(pred_C.q[3,])),
        col = adjustcolor(1, .2), border = NA)
points(TimeMedian$Median, ind_C, pch = 16, cex = .7, col = adjustcolor(1, .4))

plot(xx, pred_N.q["50%",], type = "l", lwd = 2,  las = 1, ylim = 0:1,
     main = "(B) Sensitivity to norms (deontological)",
     ylab = "Parameter N", xlab = "Median RT [s]")
abline(v = TimeMedian_ms["m", "Median"], lty = "dashed")
polygon(c(xx, rev(xx)), c(pred_N.q[1,], rev(pred_N.q[3,])),
        col = adjustcolor(1, .2), border = NA)
points(TimeMedian$Median, ind_N, pch = 16, cex = .7, col = adjustcolor(1, .4))

dev.off()

png("CNI_TimeMedianI.png", width = 4, height = 4.5, unit = "in", res = 300)
plot(xx, pred_I.q["50%",], type = "l", lwd = 2, ylim = 0:1, las = 1,
     main = "(C) Preference for inaction",
    ylab = "Parameter I", xlab = "Median RT [s]")
abline(v = TimeMedian_ms["m", "Median"], lty = "dashed")
polygon(c(xx, rev(xx)), c(pred_I.q[1,], rev(pred_I.q[3,])),
        col = adjustcolor(1, .2), border = NA)
points(TimeMedian$Median, ind_I, pch = 16, cex = .7, col = adjustcolor(1, .4))

dev.off()
