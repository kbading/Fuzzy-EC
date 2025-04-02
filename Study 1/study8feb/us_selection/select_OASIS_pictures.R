#Select pictures from the OASIS base (Kurdi et al., 2017)
#This script assumes that:
#- You have a folder with the pictures from the OASIS under the directory ("Images/")
#- You have the OASIS ratings ("OASIS.csv")
rm(list=ls())
#load packages
require(pacman)
p_load(tidyverse, imager, psych)

#read data from Kurdi et al. 2017
pict = read.csv("OASIS.csv")

#use set.seed to make picture selection reproducible
set.seed(55)

#remove pictures we cannot display, e.g., pictures of nude couples, men, and women; pictures of feces; pictures of dead animals/body...
pict$theme = substr(pict$Theme, 1, 4)
pict$theme_long = substr(pict$Theme, 1, 10)
pict = pict %>% filter(theme != "Nude" & theme_long != "Animal car" & theme_long != "Ambulance " & theme != "Fece")

describeBy(pict$Valence_mean, pict$Category)
describeBy(pict$Valence_SD, pict$Category)
describeBy(pict$Arousal_mean, pict$Category)

table(pict$Category, pict$Valence_mean < 2.5 & pict$Valence_mean>1)
table(pict$Category, pict$Valence_mean > 5.5 &pict$Valence_mean<7)
table(pict$Category, pict$Valence_mean < 4)
table(pict$Category, pict$Valence_mean < 4)
table(pict$Category, pict$Valence_mean < 4)

pict = pict %>% filter(Category != "Person")

table(pict$Category, pict$Valence_mean < 2.5 & pict$Valence_mean>1)
table(pict$Category, pict$Valence_mean > 5.5 & pict$Valence_mean<7)

length(pict$Category)
length(unique(pict$theme))

#select all negative pictures with ratings between 2 and 3 and arousal between 3 and 5
pict_neg = pict %>% filter(Valence_mean >= 1 & Valence_mean <= 2.5 #before: it was 2 -- 3
                           & Arousal_mean >= 2 & Arousal_mean <= 6
                           & Valence_SD < 1.2)

#randomly select 12 different pictures on different themes
select_neg_pic = pict_neg %>%
  group_by(theme) %>%
  sample_n(size=1) %>%
  ungroup() %>%
  sample_n(., size = 12, replace = FALSE)

#make a dataframe with all selected negative pictures
select_neg_pic$valence = "negative"
select_neg_pic$n = 1:length(select_neg_pic$X)

#test whether the mean valence of the selected negative pictures
##is below 4 (the middle value of the 7-point Likert scale)
t.test(select_neg_pic$Valence_mean, mu=4) #it is

describe(select_neg_pic$Valence_mean)
describe(select_neg_pic$Arousal_mean)

#select all positive pictures with ratings between 5 and 6 and arousal between 3 and 5
pict_pos = pict %>% filter(Valence_mean >= 5.5 & Valence_mean <= 7 #before: it was 5--6
                           & Arousal_mean >= 2 & Arousal_mean <= 6
                           & pict$Valence_SD < 1.2)

#randomly select 12 different positive pictures on different themes
select_pos_pic = pict_pos %>%
  group_by(theme) %>%
  sample_n(size=1) %>%
  ungroup() %>%
  sample_n(., size = 12, replace = FALSE)

#make a dataframe with all selected positive pictures
select_pos_pic$valence = "positive"
select_pos_pic$n = 1:length(select_pos_pic$X)

#test whether the mean valence of the selected positive pictures
##is above 4 (the middle value of the 7-point Likert scale)
t.test(select_pos_pic$Valence_mean, mu=4) #it is

describe(select_pos_pic$Valence_mean)
describe(select_pos_pic$Arousal_mean)

#selected negative and positive pictures should differ in valence
t.test(select_neg_pic$Valence_mean, select_pos_pic$Valence_mean) #yes

#they should not differ in arousal
t.test(select_neg_pic$Arousal_mean, select_pos_pic$Arousal_mean) #ok

#now, write the selected picures in a csv file
selected_pictures = rbind(select_neg_pic, select_pos_pic)
write.csv(selected_pictures, "selected_pictures_wsw.csv")

psych::describeBy(selected_pictures$Valence_mean, selected_pictures$valence)
psych::describeBy(selected_pictures$Arousal_mean, selected_pictures$valence)

#Now, we want to copy (and resize) the selected pictures
#add ".jpg" to the "Theme" column of the pictures we selected to retrieve them
##in the OASIS base
#we also add ".jpg" to the "n" column to then write the pictures
##with a more useful name (e.g., "negative3.jpg")
#selected_pictures$Theme[2] = substr(selected_pictures$Theme[2], 1, 5)
selected_pictures$jpg = paste0(selected_pictures$Theme, ".jpg")
selected_pictures$n = paste0(substr(selected_pictures$valence, 0, 1), selected_pictures$n, ".jpg")

unlink("selected_pictures/*")

for(x in 1:length(selected_pictures$jpg)) {

  file <- load.image(paste0("Images/", selected_pictures$jpg[x]))

  resized <- imresize(file,
                    scale = 1)

 save.image(resized,
             file = gsub("JPG", "jpg", paste0("selected_pictures/",selected_pictures$n[x])))

}
