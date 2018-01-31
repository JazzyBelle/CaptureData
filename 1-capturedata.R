setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

library(tidyverse)


data=read.table('newfilenames_12-17.txt', h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))



# Verify that no ID were left unintentionally blank
# initially 4 errors
# edit : 4 corrections have been done on the  on the entire dataset
data[which(is.na(data$ID ==T)),]  

# Removing captures with no ID : initially 115 captures total
# edit : now 111 captures without ID and to be removed
capturedata=data[-which(is.na(data$ID ==T)),]
# leaving 6133 captures


#############################################################
#############################################################
#### Standardize label, nonatec, tag for all individuals ####
#############################################################
#############################################################

stdata_v1=as.data.frame(matrix(data=NA,nrow=0, ncol=9))
colnames(allcaves_stdata)=colnames(a)

for (k in 1:length(levels(capturedata$cave))){
  
# Subsetting on CAVES and storing in a
  a <- droplevels(subset(capturedata, capturedata$cave == levels(capturedata$cave)[k]))
  b <- as.data.frame(table(a$ID))
  b <- droplevels(b[which(b$Freq > 1),1]) # b contains the ID that appear more than once in cave 1

  e <- as.data.frame(table(a$ID))
  e <- droplevels(e[which(e$Freq == 1),1]) # e contains the ID that appear ONLY ONCE once in cave 1

  d=as.data.frame(matrix(data=NA,nrow=0, ncol=9)) # blank data.frame
  colnames(d)=colnames(a)


    for (i in 1:length(b)){
      c <- a[which(as.character(a$ID) == as.character(b[i])),] # c is a subset containing captures for IDs that appear more than once
  
      for (j in 1:nrow(c)){         
        if(is.na(c[j,7]) == FALSE){  # label for all these captures will be the same as one that is not NA
          c[,7] = c[j,7]
        }
        if(is.na(c[j,8]) == FALSE){  # nonatec for all these captures will be the same as one that is not NA
          c[,8] = c[j,8]
        }
        if(is.na(c[j,9]) == FALSE){  # tag for all these captures will be the same as one that is not NA
          c[,9] = c[j,9]
        }
      }
      d=rbind(d,c) # d now contains all the different iterations of c (captures for individuals captured more than once)
    }


    stdata = rbind(d,subset(a, a$ID %in% e)) # contains all captures for all individuals in 1 cave but with the same label, nonatec, tag for all

    stdata_v1 = rbind(stdata_v1, stdata) # contains all captures of all individuals for all caves

}


###################################################################################################################
###################################################################################################################
#### Get the same ID for unique individuals recorded under different IDs (can be either label, nonatec or tag) ####
###################################################################################################################
###################################################################################################################

    ## First thinking ###
    # an individual can have label as ID when the correspondence with nonatec or tag could be found somewhere else
    # need to do the same type of job than previously but using label, nonatec or tag instead of ID??? meaning that i subset on 
    # label for exemple and if one has a nonatec/tag != NA then they all get this nonatec/tag 
  
    # how to deal with fish that have a tag as ID but don't have a label or nonatec but that he info could be found ?? 
    # ISN'T that included in what's just above?? No, done anyway



allcaves_stdata=droplevels(stdata_v1)
allcaves_stdata$nonatec=as.factor(allcaves_stdata$nonatec)

######################
## Looping on label ##
######################

m=as.data.frame(matrix(data=NA,nrow=0, ncol=9))
colnames(m)=colnames(allcaves_stdata)


  for (k in 1:length(levels(allcaves_stdata$label))){
  
    # Subsetting on LABEL and storing in a
    a <- droplevels(subset(allcaves_stdata, allcaves_stdata$label == levels(allcaves_stdata$label)[k]))

      for (i in 1:nrow(a)){
        if(is.na(a[i,8]) == FALSE){  
          a[,8] = a[i,8]
        }
        if(is.na(a[i,9]) == FALSE){  
          a[,9] = a[i,9]
        }
      }
  
  m=rbind(m,a)  
  }

allcaves_stdata=rbind(m,allcaves_stdata[which(is.na(allcaves_stdata$label)),])  # obs for which label is NA are dropped, need to rbind them



########################
## Looping on nonatec ##
########################

m=as.data.frame(matrix(data=NA,nrow=0, ncol=9))
colnames(m)=colnames(allcaves_stdata)

  for (k in 1:length(levels(allcaves_stdata$nonatec))){
  
    # Subsetting on NONATEC and storing in a
    a <- droplevels(subset(allcaves_stdata, allcaves_stdata$nonatec == levels(allcaves_stdata$nonatec)[k]))
  
    for (i in 1:nrow(a)){
      if(is.na(a[i,7]) == FALSE){  
        a[,7] = a[i,7]
      }
      if(is.na(a[i,9]) == FALSE){  
        a[,9] = a[i,9]
      }
    }
  
    m=rbind(m,a)  
  } 

allcaves_stdata=rbind(m,allcaves_stdata[which(is.na(allcaves_stdata$nonatec)),])   


####################
## Looping on tag ## 
####################

# SUBSETTING ON TAG ALLOWS DETECTION OF ERRORS IN TAG INPUTED IN THE INITIAL DATA. BECAUSE LABEL AND NONATEC HAVE ALREADY BEEN USED, 
# A UNIQUE INDIVIDUAL (ONLY ONE LABEL OR NONATEC) THAT HAS 2 DIFFERENT TAG NUMBER (TYPO; EX: 9 INSTEAD OF 8, ETC..) CAN BE CORRECTED FOR.
# I CHECK WHAT CAN BE WRONG AND THEN GO BACK SEARCHING THE MASTERFILE TO SEE WHICH OF THE 2 POSSIBLE IDs IS THE RIGHT ONE.
# HAPPENS VERY OFTEN --> MEANING LOTS OF CORRECTED DATA! :)

m=as.data.frame(matrix(data=NA,nrow=0, ncol=9))
colnames(m)=colnames(allcaves_stdata)

  for (k in 1:length(levels(allcaves_stdata$tag))){
  
    # Subsetting on TAG and storing in a
    a <- droplevels(subset(allcaves_stdata, allcaves_stdata$tag == levels(allcaves_stdata$tag)[k]))
  
    if (nrow(a) > 1){
      for (i in 1:nrow(a)){
        if(is.na(a[i,7]) == FALSE){  
          a[,7] = a[i,7]
        }
        if(is.na(a[i,9]) == FALSE){  
          a[,9] = a[i,9]
        }
      }
    }
    m=rbind(m,a)  
  } 

allcaves_stdata=rbind(m,allcaves_stdata[which(is.na(allcaves_stdata$tag)),])   

# ALL CLEANED , allcaves_stdata HAS THE SAME LABEL, NONATEC, TAG (WHEN APPLICABLE) FOR ALL CAPTURES OF A PARTICULAR INDIVIDUAL
# NEED TO CORRECT FOR ID THAT ARE NO LONGER RELEVANT. EX: CAPTURE RECORDED UNDER LABEL WHEN TAG OR NONATEC HAS BEEN FOUND WITH ANOTHER CAPTURE



###################################################################################
###################################################################################
#### Correcting for ID, ie: giving the last known id as ID for all individuals ####
###################################################################################
###################################################################################

# Allowed last corrections of ID (issues with leading zero removed both in tag and nonatec) -> solved !

for (i in 1: nrow(allcaves_stdata)){
  
  if (is.na(allcaves_stdata[i,9]) == FALSE){
    allcaves_stdata[i,3] = as.factor(as.character(allcaves_stdata[i,9]))
  } else if (is.na(allcaves_stdata[i,9]) == TRUE && is.na(allcaves_stdata[i,8]) == FALSE){
    allcaves_stdata[i,3] = as.factor(as.character(allcaves_stdata[i,8]))
  }
  
}

############################################
############################################
#### Add a code for the date of capture ####
############################################
############################################

# Remove duplicated observations based on ID, season and year
# Meaning that fish captured more than once in one season-year are kept only once
# No work with cave. /!\/!\/!\/!\ VERY unlikely that an individual was caught in 2 
# different caves within a 2(max 3)-weeks interval /!\/!\/!\/!\ 
allcaves_stdata = allcaves_stdata[!duplicated(allcaves_stdata[3:5]),]

for (i in 1:nrow(allcaves_stdata)){
  if (as.character(allcaves_stdata[i,4]) == 'june'){
    allcaves_stdata$date[i] = paste(as.character(allcaves_stdata$year[i]), '1', as.character(allcaves_stdata$season[i]), sep='-')    
  } else if (as.character(allcaves_stdata[i,4]) == 'august'){
    allcaves_stdata$date[i] = paste(as.character(allcaves_stdata$year[i]), '2', as.character(allcaves_stdata$season[i]), sep='-')
  }
}

allcaves_stdata$codedate = as.numeric(as.factor(allcaves_stdata$date))

######################################################################################
######################################################################################

write.table(allcaves_stdata, "allcaptures_allcaves.txt", col.names = TRUE, sep = '\t')
write.csv(allcaves_stdata, "allcaptures_allcaves.csv")

######################################################################################
######################################################################################