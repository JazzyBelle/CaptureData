library(tidyverse)

setwd("C:/Users/lherb/Documents/PhD/Data/Capture Data/")

capt_data = read.table("allcaptures_allcaves.txt", h=T, colClasses = c('factor','factor','factor','factor','factor','factor','factor','character','factor'))
detail_per_cave=read.table('detail_per_cave.txt', h=T)

n.caves=length(levels(capt_data$cave))



# Splitting caves in 3 categories based on n.fish : small, medium, large

# small caves
smallcaves = detail_per_cave[which(detail_per_cave$n.fish < quantile(detail_per_cave$n.fish, c(0.33))),]
pctg.small = cbind(c(1:9), as.data.frame(t(smallcaves[,c(4,6,8,10,12,14,16,18,20)])))
colnames(pctg.small) = c('n.capt', as.character(smallcaves$cave))

cave=c(rep('C1', 9), rep('C12', 9), rep('C17', 9), rep('C17b', 9), rep('C19', 9), rep('C24', 9), rep('C6', 9))
pctg=c(pctg.small[,2], pctg.small[,3], pctg.small[,4], pctg.small[,5], pctg.small[,6], pctg.small[,7], pctg.small[,8])
for (i in 3:ncol(pctg.small)){
  a=rbind(a,pctg.small[,i])
}

graph.small = as.data.frame(cbind(cave, n.capt=rep(1:9, times=nrow(smallcaves)), pctg=as.numeric(as.character(pctg))))


ggplot(data=graph.small, aes(x = n.capt, y = as.numeric(as.character(pctg)), group = cave)) +
  geom_line(aes(colour=cave))+
  labs(x='Number of captures', y='Percentage', title = 'Percentage of fish with x captures' , subtitle='SMALL CAVES')+
  scale_y_continuous(limits=c(0,100))

ggsave('Percentage of fish with x captures in SMALL CAVES.jpg', height = 10, width = 10)

# medium caves
mediumcaves = detail_per_cave[which(detail_per_cave$n.fish > quantile(detail_per_cave$n.fish, c(0.33)) & detail_per_cave$n.fish < quantile(detail_per_cave$n.fish, c(0.66))),]
pctg.medium = cbind(c(1:9), as.data.frame(t(mediumcaves[,c(4,6,8,10,12,14,16,18,20)])))
colnames(pctg.medium) = c('n.capt', as.character(mediumcaves$cave))

cave=c(rep('C10', 9), rep('C2', 9), rep('C20', 9), rep('C21', 9), rep('C23', 9), rep('C27', 9))
pctg=c(pctg.medium[,2], pctg.medium[,3], pctg.medium[,4], pctg.medium[,5], pctg.medium[,6], pctg.medium[,7])
for (i in 3:ncol(pctg.medium)){
  a=rbind(a,pctg.medium[,i])
}

graph.medium = as.data.frame(cbind(cave, n.capt=rep(1:9, times=nrow(mediumcaves)), pctg=as.numeric(as.character(pctg))))


ggplot(data=graph.medium, aes(x = n.capt, y = as.numeric(as.character(pctg)), group = cave)) +
  geom_line(aes(colour=cave))+
  labs(x='Number of captures', y='Percentage', title = 'Percentage of fish with x captures' , subtitle='MEDIUM CAVES')+
  scale_y_continuous(limits=c(0,100))

ggsave('Percentage of fish with x captures in MEDIUM CAVES.jpg', height = 10, width = 10)

#large caves
largecaves = detail_per_cave[which(detail_per_cave$n.fish > quantile(detail_per_cave$n.fish, c(0.66))),]
pctg.large = cbind(c(1:9), as.data.frame(t(largecaves[,c(4,6,8,10,12,14,16,18,20)])))
colnames(pctg.large) = c('n.capt', as.character(largecaves$cave))

cave=c(rep('C11', 9), rep('C18', 9), rep('C22', 9), rep('C25', 9), rep('C26', 9), rep('C5', 9), rep('C7', 9))
pctg=c(pctg.large[,2], pctg.large[,3], pctg.large[,4], pctg.large[,5], pctg.large[,6], pctg.large[,7], pctg.large[,8])
for (i in 3:ncol(pctg.large)){
  a=rbind(a,pctg.large[,i])
}

graph.large = as.data.frame(cbind(cave, n.capt=rep(1:9, times=nrow(largecaves)), pctg=as.numeric(as.character(pctg))))


ggplot(data=graph.large, aes(x = n.capt, y = as.numeric(as.character(pctg)), group = cave)) +
  geom_line(aes(colour=cave))+
  labs(x='Number of captures', y='Percentage', title = 'Percentage of fish with x captures' , subtitle='LARGE CAVES')+
  scale_y_continuous(limits=c(0,100))

ggsave('Percentage of fish with x captures in LARGE CAVES.jpg', height = 10, width = 10)