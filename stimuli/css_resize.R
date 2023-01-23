#resize stimuli

#assumes you have a "CSs" folder (with the stimuli you want to use as CSs)
##and a "cs_selection" folder (where you want to write the resized CSs)

require(pacman) #if not installed, "install.packages("pacman")
p_load(imager) #a useful R package to process images

css = load.dir("CSs") #load all images from the "CSs" folder

css_name = list.files("CSs") #read the name of the images 

wi = 200 # width we want
he = 270 # height we want

unlink("cs_selection/*") #empty the "cs_selection" folder

for(i in 1:length(css)){ #in a loop (size = n of css)
  
  file = css[[i]] # read the i image
  
  resized = resize(file, wi, he) # resize the i image with width and height defined above
  
  save.image(resized, #save the resized i image
             file = paste0("cs_selection/",css_name[i])) #in the "cs_selection" folder, with the name of the source image
  
}