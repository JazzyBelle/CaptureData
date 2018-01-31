library(tidyverse)

setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

capt_data = read.table("allcaptures_allcaves.txt", h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))

# IDs with at least one recapture
a=as.data.frame(table(capt_data$ID))

# Contains only the fish that have been at least recaptured once
recapt_data = capt_data[which(capt_data$ID %in% droplevels(a[which(a$Freq > 1),1])),]
recapt_data$codedate=as.numeric(as.character(recapt_data$codedate))
recapt_data$timingcapture=NA

a=recapt_data$codedate[2]-recapt_data$codedate[1]

for (i in 1: (nrow(recapt_data)-1)){
  if (as.numeric(recapt_data$codedate[i+1]) > as.numeric(recapt_data$codedate[i])){
    recapt_data$timingcapture[i] = as.numeric(recapt_data$codedate[i+1]-recapt_data$codedate[i])
    #b = as.numeric(recapt_data$codedate[i+1]-recapt_data$codedate[i])
    #a <- c(a, b)
  }
}

table(recapt_data$timingcapture)

# 1    2    3    4    5    6    7    8 
# 1619  660  204   90   39   11    9    2 

# 1619 back to back recaptures - representing 869 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 1),3]))
# 660 2-seasons long recaptures - representing 506 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 2),3]))
# 204 3-seasons long recaptures - representing 195 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 3),3]))
# 90 4-seasons long recaptures - representing 89 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 4),3]))
# 39 5-seasons long recaptures - representing 39 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 5),3]))
# 11 6-seasons long recaptures - representing 11 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 6),3]))
# 9 7-seasons long recaptures - representing 9 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 7),3]))
# 2 8-seasons long recaptures - representing 2 IDs
levels(droplevels(recapt_data[which(recapt_data$timingcapture == 8),3]))
