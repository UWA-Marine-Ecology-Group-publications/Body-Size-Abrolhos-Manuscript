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
working.dir<-setwd("C:/Users/00104541/OneDrive - University of Wollongong/Tim Body Length")
## Save these directory names to use later----
raw.dir<-paste(working.dir,"Raw data",sep="/")
tidy.dir<-paste(working.dir,"Tidy data",sep="/")


setwd(raw.dir)


# Import data ---
data<-read.csv("fish length.csv")


# Parse out metadata ---
metadata<-subset(data, select = c(1:15))%>%
  distinct(Opcode, .keep_all = TRUE)%>%
  group_by(Opcode) %>% 
  filter(row_number() == 1)%>%
  group_by(Opcode) %>% slice(1)
metadata

# Parse out Length and Species data ---
Lengthdf <-subset(data, select = c(15,16,17,6,4,7,10))
as.data.frame(Lengthdf)



##SMALL CLASS##

#Define length classes and make into presence/absence data 

Small <-subset(Lengthdf, Length>=34 & Length<=67)%>%
       dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
 ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
 tally()

## Assuming all are currently represented somewhere in data set,
(SmallStatus <- unique(Small$Status[!is.na(Small$Status)]))
(SmallOpcode<- unique(Small$Opcode[!is.na(Small$Opcode)]))
(SmallGenus.Species <- unique(Small$Genus.Species[!is.na(Small$Genus.Species)]))
(SmallYear<- unique(Small$Year[!is.na(Small$Year)]))
(SmallLocation <- unique(Small$Location[!is.na(Small$Location)]))
(SmallSite <- unique(Small$Site[!is.na(Small$Site)]))

## The complete data with all species, for all locations, for all
## Status, present is
Small.complete <- expand.grid(SmallSt = SmallStatus, SmallOp = SmallOpcode,
                             Smallsp = SmallGenus.Species, Smallyr=SmallYear, 
                             SmallLoc=SmallLocation, SmallSite=SmallSite)

## Put the two together, with NA for unrecorded abundance
Small.complete.data <- merge(Small.complete, Small,
                            by.x = c("SmallSt", "SmallOp", "Smallsp","Smallyr", "SmallLoc", "SmallSite"),
                            by.y = c("Status", "Opcode", "Genus.Species", "Year", "Location", "Site"),
                            all.x = TRUE)

## Fill in the NAs
Small.complete.data$n[is.na(Small.complete.data$n)] <- 0



##convert to Wide Format
Small.data_wide <- spread(Small.complete.data, Smallsp, n)


#reorder columns
  Small.data_wide <- Small.data_wide[, c(6:26, 1,2,3,4,5)] #adjust to suit number of columns and factors in data


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

# Write PRIMER data----
head(Small.primer)
write.csv(Small.primer,file=paste(study,"Small.PRIMER.redone.csv",sep = "_"), row.names=FALSE)



#########################################MEDIUM CLASS#####################################################

Med <-subset(Lengthdf, Length>=105 & Length<=299)%>%
  dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
  ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
  tally()

## Assuming all are currently represented somewhere in data set,
(MedStatus <- unique(Med$Status[!is.na(Med$Status)]))
(MedOpcode<- unique(Med$Opcode[!is.na(Med$Opcode)]))
(MedGenus.Species <- unique(Med$Genus.Species[!is.na(Med$Genus.Species)]))
(MedYear<- unique(Med$Year[!is.na(Med$Year)]))
(MedLocation <- unique(Med$Location[!is.na(Med$Location)]))
(MedSite <- unique(Med$Site[!is.na(Med$Site)]))

## The complete data with all species, for all locations, for all
## Status, present is
Med.complete <- expand.grid(MedSt = MedStatus, MedOp = MedOpcode,
                              Medsp = MedGenus.Species, Medyr=MedYear, 
                              MedLoc=MedLocation, MedSite=MedSite)

## Put the two together, with NA for unrecorded abundance
Med.complete.data <- merge(Med.complete, Med,
                             by.x = c("MedSt", "MedOp", "Medsp","Medyr", "MedLoc", "MedSite"),
                             by.y = c("Status", "Opcode", "Genus.Species", "Year", "Location", "Site"),
                             all.x = TRUE)

## Fill in the NAs
Med.complete.data$n[is.na(Med.complete.data$n)] <- 0



##convert to Wide Format
Med.data_wide <- spread(Med.complete.data, Medsp, n)


#reorder columns
Med.data_wide <- Med.data_wide[, c(6:94, 1,2,3,4,5)]#change this to suit the number of columns and factors


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

# Write PRIMER data----
head(Med.primer)
write.csv(Med.primer,file=paste(study,"Med.PRIMER.Redone.csv",sep = "_"), row.names=FALSE)





########################################LARGE CLASS########################################################

Large <-subset(Lengthdf, Length>=360 & Length<=1000)%>%
  dplyr::mutate(Length=1)%>%
  dplyr::group_by(Opcode, Length, Status, Location, Site, Year)%>%
  ### Use this bit if you want abundance
  dplyr::group_by(Opcode, Genus.Species, Status, Location, Site, Year)%>%
  tally()

## Assuming all are currently represented somewhere in data set,
(LargeStatus <- unique(Large$Status[!is.na(Large$Status)]))
(LargeOpcode<- unique(Large$Opcode[!is.na(Large$Opcode)]))
(LargeGenus.Species <- unique(Large$Genus.Species[!is.na(Large$Genus.Species)]))
(LargeYear<- unique(Large$Year[!is.na(Large$Year)]))
(LargeLocation <- unique(Large$Location[!is.na(Large$Location)]))
(LargeSite <- unique(Large$Site[!is.na(Large$Site)]))

## The complete data with all species, for all locations, for all
## Status, present is
Large.complete <- expand.grid(LargeSt = LargeStatus, LargeOp = LargeOpcode,
                            Largesp = LargeGenus.Species, Largeyr=LargeYear, 
                            LargeLoc=LargeLocation, LargeSite=LargeSite)

## Put the two together, with NA for unrecorded abundance
Large.complete.data <- merge(Large.complete, Large,
                           by.x = c("LargeSt", "LargeOp", "Largesp","Largeyr", "LargeLoc", "LargeSite"),
                           by.y = c("Status", "Opcode", "Genus.Species", "Year", "Location", "Site"),
                           all.x = TRUE)

## Fill in the NAs
Large.complete.data$n[is.na(Large.complete.data$n)] <- 0



##convert to Wide Format
Large.data_wide <- spread(Large.complete.data, Largesp, n)


#reorder columns
Large.data_wide <- Large.data_wide[, c(6:81, 1,2,3,4,5)]#change this to suit the number of columns


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

# Write PRIMER data----
head(Large.primer)
write.csv(Large.primer,file=paste(study,"Large.PRIMER.redone.csv",sep = "_"), row.names=FALSE)




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





