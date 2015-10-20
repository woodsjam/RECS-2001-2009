#May have fixed the problem

recs2009_public <- read.csv("2009/recs2009_public.csv")


All2009<-recs2009_public

library(plyr)

library(reshape)

#AIA_ZONE issue with revalue producing results

All2009$AIA_ZONE<-revalue(as.factor(All2009$AIA_Zone),
                          c("1"="lessthan2000CDD_greaterthan_7000HDD","2"=
                            "lessthan2000CDD_between5500-7000HDD"
                            ,"3"="lessthan2000CDD_between4000-5499HDD","4"=
                              "lessthan2000CDD_lessthan4000DD",
                            "5"="greaterthan2000CDD_lessthan4000HDD"))



summary(All2009$AIA_Zone)

#METROMICRO (Description seem more complex than response code.
#Adjustment needed?)

#UR

All2009$UR<-revalue(as.factor(All2009$UR),c("U"="Urban","R"="Residential"))

summary(All2009$UR)

#KOWNRENT

All2009$KOWNRENT<-revalue(as.factor(All2009$KOWNRENT),
                          c("1"="Ownded","2"="Rented","3"="Occupied"))

summary(All2009$KOWNRENT)

#CONDCOOP (NA = -2) 

All2009$CONDCOOP[All2009$CONDCOOP==-2]<- NA

All2009$CONDCOOP<-revalue(as.factor(All2009$CONDCOOP),
                          c("1"="Condo","2"="Cooperative"))

summary(All2009$CONDCOOP)

#YEARMADE - Numeric years seem applicable, advise if change needed

#YEARMADERANGE

All2009$YEARMADERANGE<-revalue(as.factor(All2009$YEARMADERANGE),c("1"="PRE1950","2"="1950-59","3"="1960-69","4"="1970-79","5"="1980-89","6"="1990-99","7"="2000-04","8"="2005-09"))

summary(All2009$YEARMADERANGE)

#OCCUPYRANGE

All2009$OCCUPYYRANGE<-revalue(as.factor(All2009$OCCUPYYRANGE),c("1"="PRE1950","2"="1950-59","3"="1960-69","4"="1970-79","5"="1980-89","6"="1990-99","7"="2000-04","8"="2005-09"))

summary(All2009$OCCUPYYRANGE)

#Conversion - NA still

summary(All2009$CONVERSION)

sum(is.na(All2009$CONVERSION))

All2009$CONVERSION[All2009$CONVERSION==-2]<-NA


All2009$CONVERSION<-revalue(as.factor(All2009$CONVERSION),c("1"="BuiltAsApart","2"="ConverToApart"))


summary(All2009$CONVERSION)

#ORIG1FAM

All2009$ORIG1FAM[All2009$ORIG1FAM==-2]<-NA

All2009$ORIG1FAM<-revalue(as.factor(All2009$ORIG1FAM),c("1"="YES","0"="NO"))

summary(All2009$ORIG1FAM)

#LOOKLIKE

All2009$LOOKLIKE[All2009$LOOKLIKE==-2]<-NA

All2009$LOOKLIKE<-revalue(as.factor(All2009$LOOKLIKE),c("1"="MoreLikeHouse","2"="MoreLikeApart"))

summary(All2009$LOOKLIKE)

#NUMFLRS

All2009$NUMFLRS[All2009$NUMFLRS==-2] <- NA

All2009$NUMFLRS<-revalue(as.factor(All2009$NUMFLRS),c("1"="1Floor","2"="2Floors","3"="3Floors","4"="4Floors","5"="5Floors","6"="6Floors","7"="7Floors","8"="8Floors","9"="9Floors","10"="10Floor","11"="11Floors","12"="12Floors","13"="13Floors","14"="14Floors","15"="15Floors","20"="20Floors","21"="21Floors","35"="35Floors"))
                                                       
                                                       
summary(All2009$NUMFLRS)

#NUMAPTS

table(All2009$NUMAPTS)

All2009$NUMAPTS[All2009$NUMAPTS==-2]<-NA

All2009$NUMAPTS<-revalue(as.factor(All2009$NUMAPTS),c("5"="5units","6"="6unit","7"=""))