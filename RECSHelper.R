#Helper functions for RECS recoding

library(plyr)
library(stringr)


MarkCoded<-function(nameCol){
  Cols2009[Cols2009$V1==nameCol,]$Status<<-"Coded"
  
}


#Convert to TRUE and FALSE with options on NA treatment
#column is a column in the data frame
#na.action is either KEEP means to code as NA, TRUE means convert to TRUE, or FALSE means convert to FALSE
#na.key is the indicator used to indicate that an observation is missing. 
#output is a column for a dataframe


to.TF <-function(column, na.choice="KEEP", na.key=-2){
  if(na.choice=="KEEP") column[column==na.key]<-NA 
  if(na.choice=="TRUE") column[column==na.key]<-TRUE 
  if(na.choice=="FALSE") column[column==na.key]<-FALSE 
  
  theFullCol<-as.character(as.list( sys.call(sys.parent()) )[-1])
  theDollar<-str_locate(theFullCol,"\\$")
  ColName<-as.character(substr(theFullCol,theDollar+1,nchar(theFullCol) ))[1]
  MarkCoded(ColName)
  
  
  as.logical(revalue(as.factor(column),c("0"=FALSE, "1"=TRUE)))
}

# #Example Code
# summary(All2009$CENACHP)
# 
# summary(to.TF(All2009$CENACHP, na.choice="KEEP"))
# summary(to.TF(All2009$CENACHP, na.choice="FALSE"))
# summary(to.TF(All2009$CENACHP, na.choice="TRUE"))


# Change the real valued columns with an numeric na code to real NA
# Default NA indicator is -2
# output is a column for a dataframe
fix.na<-function(column, na.key=-2){
  column[column==na.key]<-NA
  column
}
  

# #Example Code
# summary(All2009$NUMBERAC)
# summary(fix.na(All2009$NUMBERAC))


# General Recode.  Keystroke saver
# column is a datafram
# key is a c in the format used for revalue
recode<-function(column, key, na.key="-2"){
  theFullCol<-as.character(as.list( sys.call(sys.parent()) )[-1])
  theDollar<-str_locate(theFullCol,"\\$")
  ColName<-as.character(substr(theFullCol,theDollar+1,nchar(theFullCol) ))[1]
  MarkCoded(ColName)
  revalue(as.factor(fix.na(column,na.key)),key)
}

recodeNumeric<-function(column, na.key="-2"){
  theFullCol<-as.character(as.list( sys.call(sys.parent()) )[-1])
  theDollar<-str_locate(theFullCol,"\\$")
  ColName<-as.character(substr(theFullCol,theDollar+1,nchar(theFullCol) ))[1]
  MarkCoded(ColName)
  as.numeric(fix.na(column,na.key))
}

summary(recodeNumeric(All2009$NUMTHERM))
# Example Code
# summary(revalue(as.factor(All2009$ESWWAC),c("0"=TRUE, "1"=FALSE, "-2"=FALSE, "-8"=FALSE, "-9"=FALSE)))
# summary(All2009$ESWWAC)
# 
summary(recode(All2009$ESWWAC,c("0"=TRUE, "1"=FALSE, "-2"=FALSE, "-8"=FALSE, "-9"=FALSE)))


#Common Recodes
# Best to define this at the top an then use them so you are less likely to make errors

FuelType<-c("1"="NG","2"="LPG", "3"="FuelOil","4"="Kerosene", "5"="Elec","8"="Solar","21"="Other", "-2"=NA)

# Example
# summary(recode(All2009$FUELPOOL,FuelType))


##
# Mark one colum as coded

#Common year vector

TYPEAGE<-c("1"="Le2Years","2"="2TO4Years","3"="5To9Years","41"="10To14Years","42"="15To19Years","5"="GT20Years")

#Common payment vector

PAYMENTS<-c("1"="Household","2"="RentIncluded","3"="OtherWay")

#Common Age Catergory 

AGECAT<-c("1"="Le5","2"="5To9","3"="10To14","4"="15To19","5"="20To24","6"="25To29","7"="30To34","8"="35To39","9"="40To44","10"="45To49","11"="50to54","12"="55To59","13"="60to64","14"="65To69","15"="70To74","16"="75To79","17"="80To84","18"="Gr84")

AGECAT05<-c("1"="Le2","2"="2To4","3"="5To9","4"="10To19","5"="Gr20","6"="OldAsHome")
