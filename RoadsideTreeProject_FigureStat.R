install.packages("dplyr")
library(dplyr)
install.packages("vegan")
library(vegan)

# Figure 4

dat <- read.csv('https://raw.githubusercontent.com/ivanhykwong/RoadsideTrees-ShauKeiWan/main/TreeData_basic.csv')

## importance value
dat_speciessum <- dat %>% group_by(SpeciesEng) %>%
  summarize(stem_sum = n(), basal_sum = sum((DBH_cm**2)*pi))
dat_speciessum$RelDen <- dat_speciessum$stem_sum/sum(dat_speciessum$stem_sum)
dat_speciessum$RelDom <- dat_speciessum$basal_sum/sum(dat_speciessum$basal_sum)
dat_speciessum$IV <- dat_speciessum$RelDen + dat_speciessum$RelDom
dat_speciessum <- dat_speciessum[order(-dat_speciessum$IV),]
print(dat_speciessum)

## DBH group
dat_dbhgroup <- dat %>% mutate(DBH_group = cut(DBH_cm, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,Inf)))
dat_dbhgroup <- dat_dbhgroup %>% group_by(DBH_group) %>% 
  summarize(count = n(), n_species = n_distinct(SpeciesEng))
print(dat_dbhgroup)


# Table 2

dat <- read.csv('https://raw.githubusercontent.com/ivanhykwong/RoadsideTrees-ShauKeiWan/main/TreeData_SEMvariable.csv')
dat <- dat[,c("Zone","Eng")]

## Shannon & evenness
top3 <- function(Eng){
  t <- table(Eng) %>% as.data.frame() %>% arrange(desc(Freq))
  return(paste(t$Eng[1:3], collapse = "_"))
}
dat_zone <- dat %>% group_by(Zone) %>%
  summarize(count = n(), n_species = n_distinct(Eng), top3 = top3(Eng),
            Shannon = diversity(table(Eng)),
            evenness = diversity(table(Eng))/log(n_distinct(Eng)))
print(dat_zone)

dat_all <- dat %>% summarize(count = n(), n_species = n_distinct(Eng), 
                             Shannon = diversity(table(Eng)),
                             evenness = diversity(table(Eng))/log(n_distinct(Eng)))
print(dat_all)

## Jaccard similarity index
similarity <- function(Z1, Z2){
  s1 <- as.data.frame(table(dat[dat$Zone==Z1,]$Eng))
  colnames(s1) <- c("species","Freq_Z1")
  s2 <- as.data.frame(table(dat[dat$Zone==Z2,]$Eng))
  colnames(s2) <- c("species","Freq_Z2")
  df <- merge(s1,s2, by="species", all=TRUE)
  return(nrow(na.omit(df))/nrow(df))
}
print(paste0("similarity_Z1Z2: ",similarity(1,2)))
print(paste0("similarity_Z2Z3: ",similarity(2,3)))
print(paste0("similarity_Z1Z3: ",similarity(1,3)))
