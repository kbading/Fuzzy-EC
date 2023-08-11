#par(mfrow = c(1, 2))
#hist(data$p_memory_us_valence)
#hist(data$p_memory_relation)
#hist(data$p_memory_us_valence[data$relation == "starting"], breaks = seq(0, 1, .1))
#hist(data$p_memory_us_valence[data$relation == "ending"], breaks = seq(0, 1, .1))

#hist(data$p_memory_relation[data$relation == "starting"], breaks = seq(0, 1, .1))
#hist(data$p_memory_relation[data$relation == "ending"], breaks = seq(0, 1, .1))

mean(id_effects$memory_us_valence)
mean(id_effects$memory_relation)
mean(data$p_memory_us_valence)
mean(data$p_memory_relation)

library(papaja)
apa_barplot(
  data = data
  , id = "id"
  , dv = "eval_rating"
  , factors = c("relation", "us_valence")
)
library(afex)
anova_out <- aov_4(formula = eval_rating ~ (us_valence + relation|id), data = data)
library(emmeans)
contrast(
  emmeans(anova_out, specs = ~ us_valence + relation)
  , list("a" = c(1, -1, 0, 0)))