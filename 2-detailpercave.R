library(tidyverse)

setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

capt_data = read.table("allcaptures_allcaves.txt", h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))

# Remove duplicated observations based on ID, season and year
# Meaning that fish captured more than once in one season-year are kept only once
# No work with cave. /!\/!\/!\/!\ VERY unlikely that an individual was caught in 2 
# different caves within a 2(max 3)-weeks interval /!\/!\/!\/!\ 
capt_data = capt_data[!duplicated(capt_data[3:5]),]


# HOW MANY INDIVIDUALS ARE BEING FOLLOWED TOTAL?

length(levels(capt_data$ID)) # n=3298 different IDs overall

######################################################################################
######################################################################################
#### HOW MANY INDIVIDUALS PER CAVE ? HOW MANY WITH A CERTAIN NUMBER OF CAPTURES ? ####
######################################################################################
######################################################################################

# By summing the count for the subset in each cave, we obtain
# n = 3401. It is more than the overall number of IDs (n=3298) because some fish moved between caves and 

################################################################
# HOW MANY HAVE BEEN CAPTURED 1, 2, 3, ... IN TOTAL? PER CAVE? #
################################################################


n.caves=length(levels(capt_data$cave))

# Blank matrix to host the data
detail_per_cave = as.data.frame(matrix(data=NA,nrow=n.caves, ncol=2))
colnames(detail_per_cave) = c('cave', 'n.fish')
detail_per_cave[,1] = as.factor(levels(capt_data$cave))


# Initiate the number of most captures for 1 fish
i=1
a = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
detail_per_cave[i,2] = length(levels(droplevels(a$ID)))
b = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
per_cave_levels = as.data.frame(table(droplevels(a$ID)))
mostcaptures=max(per_cave_levels$Freq)

# This for loop to get the maximum number of captures for one fish
for (i in 1:n.caves){
  # counting the number of fish per cave
  a = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  detail_per_cave[i,2] = length(levels(droplevels(a$ID)))
  
  # counting the number of fish with a certain number of recaptures
  b = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  per_cave_levels = as.data.frame(table(droplevels(a$ID)))
  
  if (max(per_cave_levels$Freq) > mostcaptures){
    mostcaptures = max(per_cave_levels$Freq)
  }
}  

detail_per_cave=cbind(detail_per_cave, matrix(NA, nrow = n.caves, ncol = 2*mostcaptures))

for (i in 1:n.caves){
  # counting the number of fish per cave
  a = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  detail_per_cave[i,2] = length(levels(droplevels(a$ID)))
  
  # counting the number of fish with a certain number of recaptures
  b = subset(capt_data, capt_data$cave == as.factor(detail_per_cave[i,1]))
  per_cave_levels = as.data.frame(table(droplevels(a$ID)))
  
  for (j in 1:mostcaptures){
    detail_per_cave[i,2*j+1] = length(which(per_cave_levels$Freq == j))
    detail_per_cave[i,2*j+2] = round(100 * detail_per_cave[i,2*j+1] / detail_per_cave$n.fish[i] , 2)
  }
}



write.table(detail_per_cave, 'detail_per_cave.txt', sep = '\t')

# THINK OF A WAY TO COUNT FISH THAT HAVE POTENTIALLY LOST THEIR TAGS..