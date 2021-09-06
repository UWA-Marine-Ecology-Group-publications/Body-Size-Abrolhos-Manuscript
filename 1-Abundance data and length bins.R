### Extract metadata and length data from a file and convert to abundance 
#Kye Adams June 2021

### OBJECTIVES ###
# 1. Import data 
# 2. Extract metadata
# 3. Extract abundance data based on length classes small (50-150 mm) medium (100-310 mm FL) large (375-1000 mm FL) 
# 4. Write data for CAP analysis in PERMANOVA 


# Clear memory ----
rm(list=ls())

# Libraries required ----
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)


## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study<-"bodysize"

## Folder Structure ----
# This script uses one main folder ('working directory')


## Set your working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # sets working directory to that of this script - or type your own
##working.dir<-setwd("C:/Users/00104541/OneDrive - University of Wollongong/Tim Body Length")
## Save these directory names to use later----
raw.dir<-paste(working.dir,"Raw data",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")


setwd(raw.dir)


# Import data ---
data<-read.csv("fish length.csv")


# Parse out metadata ---
metadata<-subset(data, select = c(1:18))%>%
  distinct(Opcode, .keep_all = TRUE)%>%
  group_by(Opcode) %>% 
  filter(row_number() == 1)%>%
  group_by(Opcode) %>% slice(1)
metadata

# Parse out Length and Species data ---
Lengthdf <-subset(data, select = c(15,16,17,18,6,4,7,10))
levels(Lengthdf$Status)<-c('Fished','Closure')
as.data.frame(Lengthdf)

Opcodelist<-unique(Lengthdf$Opcode)
Opcodelist<- as.data.frame(Opcodelist)
head(Opcodelist)


##SMALL CLASS##

#Define length classes and make into presence/absence data 

Small <-subset(Lengthdf, Length>=34 & Length<=67)%>%
       dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
 ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
 tally()


#check how many species in data
Small.Species <- unique(Small$Genus.Species[!is.na(Small$Genus.Species)])
SmallSpecies<- as.data.frame(Small.Species)
head(SmallSpeciesList)

factors<- metadata%>% select(Opcode, Status, Location, Site, Year)

Small.complete.data<-Small %>% 
  ungroup() %>%
  dplyr::select(Opcode, Genus.Species,n)%>%
  complete(Opcode)%>%
  complete(Genus.Species, nesting(Opcode), fill = list(n = 0))%>% #gives a complete list of species with zeros for absences
  left_join(.,factors)
  

#check all opcodes are present in data
SmallOpcodelist<-unique(Small.complete.data$Opcode)
SmallOpcodelist<- as.data.frame(SmallOpcodelist)
head(Opcodelist)

##convert to Wide Format
Small.data_wide <- spread(Small.complete.data, Genus.Species, n)

##Remove all species with all zero columns
sm.test<-Small.data_wide[, c(6:155)]
sm.df<-sm.test[, colSums(sm.test,na.rm = TRUE) != 0]
sm.fact<-Small.data_wide[, c(1:5)]

#reorder columns
Small.data_wide<-cbind(sm.df,sm.fact)


# Make the blank Column----
temp.column<-matrix(c(rep.int("",length(Small.data_wide))),nrow=length(Small.data_wide),ncol=1)
blank.column<-data.frame(temp.column)

# Make the final PRIMER data----
Small.data_wide[]<-lapply(Small.data_wide,as.character) #have to make whole data.frame as.character
Small.primer<-Small.data_wide%>%
  bind_rows(blank.column)
Small.primer<- Small.primer[, c(1:21, 27,22,23,24,25,26)]#adjust to suit number of columns in data
Small.primer <- plyr::rename(Small.primer, replace =c("temp.column"="") )
Small.primer[is.na(Small.primer)] <- ""
Small.primer$Code <- 1:nrow(Small.primer) 
Small.primer<- Small.primer[, c(28, 1:27)]#


# Write PRIMER data----
head(Small.primer)
write.csv(Small.primer,file=paste(study,"Small.PRIMER.fixed.csv",sep = "_"), row.names=FALSE)



#########################################MEDIUM CLASS#####################################################

Med <-subset(Lengthdf, Length>=105 & Length<=299)%>%
  dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
  ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
  tally()

#check how many species in data
MedSpecies<-unique(Med$Genus.Species)
MedSiteSpecies<- as.data.frame(MedSpecies)
head(MedSpeciesList)

factors<- metadata%>% select(Opcode, Status, Location, Site, Year)

Med.complete.data<-Med %>% 
  ungroup() %>%
  dplyr::select(Opcode, Genus.Species,n)%>%
  complete(Opcode)%>%
  complete(Genus.Species, nesting(Opcode), fill = list(n = 0))%>% #gives a complete list of species with zeros for absences
  left_join(.,factors)

#check all sites are present in data
MedSite<-unique(Med.complete.data$Site)
MedSiteList<- as.data.frame(MedSite)
head(MedSiteList)

##convert to Wide Format
Med.data_wide <- spread(Med.complete.data, Genus.Species, n)

##Remove all species with all zero columns
med.test<-Med.data_wide[, c(6:155)]
med.df<-med.test[, colSums(med.test,na.rm = TRUE) != 0]
med.fact<-Med.data_wide[, c(1:5)]

#reorder columns
Med.data_wide<-cbind(med.df,med.fact)

# Make the blank Column----
temp.column<-matrix(c(rep.int("",length(Med.data_wide))),nrow=length(Med.data_wide),ncol=1)
blank.column<-data.frame(temp.column)

# Make the final PRIMER data----
Med.data_wide[]<-lapply(Med.data_wide,as.character) #have to make whole data.frame as.character
Med.primer<-Med.data_wide%>%
  bind_rows(blank.column)
Med.primer<- Med.primer[, c(1:89, 95,90,91,92,93,94)]#change this to suit the number of columns
Med.primer <- plyr::rename(Med.primer, replace =c("temp.column"="") )
Med.primer[is.na(Med.primer)] <- ""
Med.primer$Code <- 1:nrow(Med.primer) 
Med.primer<- Med.primer[, c(96, 1:95)]#

# Write PRIMER data----
head(Med.primer)
write.csv(Med.primer,file=paste(study,"Med.PRIMER.fixed.csv",sep = "_"), row.names=FALSE)





########################################LARGE CLASS########################################################

Large <-subset(Lengthdf, Length>=360 & Length<=1000)%>%
  dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
  ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
  tally()

Large.complete.data<-Large %>% 
  ungroup() %>%
  dplyr::select(Opcode, Genus.Species,n)%>%
  complete(Opcode)%>%
  complete(Genus.Species, nesting(Opcode), fill = list(n = 0))%>% #gives a complete list of species with zeros for absences
  left_join(.,factors)

#check all sites are present in data
LargeSite<-unique(Large.complete.data$Site)
LargeSiteList<- as.data.frame(LargeSite)
head(LargeSiteList)

##convert to Wide Format
Large.data_wide <- spread(Large.complete.data, Genus.Species, n)

##Remove all species with all zero columns
lar.test<-Large.data_wide[, c(6:155)]
lar.df<-lar.test[, colSums(lar.test,na.rm = TRUE) != 0]
lar.fact<-Large.data_wide[, c(1:5)]

#reorder columns
Large.data_wide<-cbind(lar.fact,lar.df)

# Make the blank Column----
temp.column<-matrix(c(rep.int("",length(Large.data_wide))),nrow=length(Large.data_wide),ncol=1)
blank.column<-data.frame(temp.column)

# Make the final PRIMER data----
Large.data_wide[]<-lapply(Large.data_wide,as.character) #have to make whole data.frame as.character
Large.primer<-Large.data_wide%>%
  bind_rows(blank.column)
Large.primer<- Large.primer[, c(1:76, 82,77,78,79,80,81)]#change this to suit the number of columns
Large.primer <- plyr::rename(Large.primer, replace =c("temp.column"="") )
Large.primer[is.na(Large.primer)] <- ""
Large.primer$Code <- 1:nrow(Large.primer) 
Large.primer<- Large.primer[, c(83, 1:82)]#

# Write PRIMER data----
head(Large.primer)
write.csv(Large.primer,file=paste(study,"Large.PRIMER.fixed.csv",sep = "_"), row.names=FALSE)

library(BiodiversityR)

CAPdiscrim(lar.df, lar.fact, dist="gower", axes=4, m=0, mmax=10, add=FALSE, permutations=0)


#create species lists
Species<- unique(Lengthdf$Genus.Species)
SpeciesList<- as.data.frame(Species)
head(SpeciesList)

#create Small species list
SmallSpecies<- unique(Small$Genus.Species)
SmallSpeciesList<- as.data.frame(SmallSpecies)
head(SmallSpeciesList)
#create Medium species list
MediumSpecies<- unique(Med$Genus.Species)
MediumSpeciesList<- as.data.frame(MediumSpecies)
head(MediumSpeciesList)
#create Large species list
LargeSpecies<- unique(Large$Genus.Species)
LargeSpeciesList<- as.data.frame(LargeSpecies)
head(LargeSpeciesList)



 
#in Primer run a three Caps with Status as a factor





