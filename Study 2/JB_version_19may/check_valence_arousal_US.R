#Check whether valence and arousal differ as a function of positive vs. negative valence

##note: Between Study 1 (pilot) and Study 2, two USs were replaced to avoid black and white pictures:
###"War 8" (negative scene) in OASIS was replaced with "War 2" (negative scene) in OASIS
###"Plane crash 4" (negative object) in OASIS was replaced with "Plane crash 1" (negative object) in OASIS

require(pacman)
p_load("psych", "effectsize")

pict = read.csv("selected_pictures_wsw2.csv")
pict$valence = as.factor(pict$valence)

#valence mean as a function of valence
t.test(pict$Valence_mean~pict$valence, paired=FALSE) #significantly different
describeBy(pict$Valence_mean, pict$valence)
effectsize::cohens_d(pict$Valence_mean, pict$valence, paired = FALSE)

#arousal mean as a function of valence
t.test(pict$Arousal_mean~pict$valence, paired=FALSE) #not significantly different
describeBy(pict$Arousal_mean, pict$valence)
effectsize::cohens_d(pict$Arousal_mean, pict$valence, paired = FALSE)
