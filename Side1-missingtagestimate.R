library(tidyverse)

setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

phenotype=read.table('phenotypicdata.txt', h=T)
capt_data = read.table("allcaptures_allcaves.txt", h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))

phenotype$uniqueID=paste(phenotype$capt.ID, phenotype$season, phenotype$year, sep='-')
a=as.data.frame(table(phenotype$uniqueID))

###############################################################
#### AVERAGING THE LENGTH BETWEEN TWO CAPTURES IN A SEASON ####
###############################################################

bla=match(droplevels(a[which(a$Freq == 2 ),1]), phenotype$uniqueID, nomatch = NA_integer_, incomparables = NULL)

temporary=phenotype[c(bla),]
temporary$length=as.numeric(temporary$length)
b=nrow(temporary)

## calculating length average between two visits ONLY IF difference between 2 visits is <5mm

for (i in 1:b){
  if (as.numeric(as.character(phenotype[bla[i],6]))-5 <= as.numeric(as.character(phenotype[bla[i]+1,6])) & as.numeric(as.character(phenotype[bla[i]+1,6])) <= as.numeric(as.character(phenotype[bla[i],6])) +5){
    temporary[i,6] = as.numeric((as.numeric(as.character(phenotype[bla[i],6])) + as.numeric(as.character(phenotype[bla[i]+1,6])))/2)
  }
}

## remove 2 visits and move average length back to phenotype
phenotype=rbind(phenotype[-c(bla, bla+1),], temporary)
phenotype=phenotype[order(as.numeric(row.names(phenotype))),]
rownames(phenotype)=c(1:nrow(phenotype))

## DEALING WITH 3 VISITS ----> No more fish with 3 captures in one season

## export the phenotype data
phenotype = phenotype[,1:6]
phenotype$ID = NA
phenotype$year = as.factor(phenotype$year)
phenotype=phenotype[-which(is.na(phenotype$capt.ID)),]
write.table(phenotype, "phenotype.txt")

#############################################################
#### COUNTING THE NUMBER OF POTENTIAL MISSING TAG EVENTS ####
#############################################################

## This count is based on fish that were first captured with a length > x mm but starting from 2014
## The minimal length can be modified and so is the year from which we account for missing tags




## Standardizing ID to be the same as in the capture-recapture data
n.phen.obs = nrow(phenotype)

for (i in 1:n.phen.obs){
  if (!(as.character(phenotype[i,1]) %in% as.character(capt_data$ID)) & as.character(phenotype[i,1]) %in% as.character(capt_data$nonatec)){
    phenotype[i,7] = as.character(capt_data[which(as.character(capt_data$nonatec) == as.character(phenotype[i,1])),3][1])
  }else{
    phenotype[i,7] = as.character(phenotype[i,1])
  }
}

## Merging phenotype with capture-recapture data ##

# merging only relevant information
data = capt_data[,c(2:9)]

combinedata=inner_join(data, phenotype, by = c("ID", "season","year"))

combinedata$suspicion = 0 # the column showing if a capture is suspicious

# remove missing label and length
combinedata=combinedata[-which(is.na(combinedata$label)),]
combinedata=combinedata[-which(is.na(combinedata$length)),]


# subset on year and keep captures only after 2014
# This feature can be modified

combinedata = combinedata[which(combinedata$year %in% c(2014:2017)),]

#
for (i in 1:nrow(combinedata)){
  if (substr(combinedata$label[i], 4,5) == substr(combinedata$year[i], 3, 4) && combinedata$length[i] > 80){
    combinedata$suspicion[i] = 1
  }
}

## dataframe with only potential missing tag capture
suspiciouscaptures = subset(combinedata, combinedata$suspicion == 1)