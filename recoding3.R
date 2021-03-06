recs2009_public <- read.csv("2009/recs2009_public.csv")

All2009<-recs2009_public

library(plyr)

library(reshape)

recode<-function(column, key){
  revalue(as.factor(column),key)
}

fix.na<-function(column, na.key=-2){
    column[column==na.key]<-NA
    column
  }


All2009$DOEID<-as.factor(All2009$DOEID)


#Region 


table(All2009$REGIONC)

All2009$REGIONC<-revalue(as.factor(All2009$REGIONC),c("1"="NE","2"="Midwest","3"="South","4"="West"))

summary(All2009$REGIONC)

#Division

summary(All2009$DIVISION)

All2009$DIVISION<-revalue(as.factor(All2009$DIVISION),c("1"="NewEnglandD","2"="MidAtlantic","3"="EastNorthCent","4"="WestNorthCent","5"="SouthAtlantic","6"="EastSouthCent","7"="WestSountCent","8"="MountNorthSub","9","MountSouthSub","10"="Pacific"))

#Reportable_Domain

summary(All2009$REPORTABLE_DOMAIN)

All2009$REPORTABLE_DOMAIN<-revalue(as.factor(All2009$REPORTABLE_DOMAIN),c("1"="CT.ME.NH.RI.VT","2"="MA","3"="NY","4"="NJ","5"="PA","6"="IL","7"="IN.OH","8"="MI","9"="WI","10"="IA.MN.ND.SD","11"="KS.NE","12"="MO","13"="VA","14"="DE.DC.MA.WV","15"="GA","16"="NC.SC","17"="FL","18"="AL.KY.MS","19"="TN","20"="AR.LA.OK","21"="TX","22"="CO","23"="ID.MT.UT.WY","24"="AZ","25"="NV.NM","26"="CA","27"="AK.HI.OR.WA"))

# TYPEHUQ

All2009$TYPEHUQ<-revalue(as.factor(All2009$TYPEHUQ),c("1"="Mobile","2"="SingleDetach","3"="SingleAttach","4"="Apartment2-4Units","5"="Apartment5PlusUnits"))

summary(All2009$TYPEHUQ)

#NWEIGHT (No NA values present)

summary(All2009$NWEIGHT)

summary(is.na(All2009$NWEIGHT))

#HDD65 (NO NA values present)

summary(All2009$HDD65)

summary(is.na(All2009$HDD65))

#CDD65 (No NA values present)

summary(All2009$CDD65)

summary(is.na(All2009$CDD65))

#HDD30YR (min value = 0 ; no '-2' values present)

summary(All2009$HDD30YR)

summary(is.na(All2009$HDD30YR))

#CDD30YR (No NA values present)

summary(All2009$CDD30YR)

summary(is.na(All2009$CDD30YR))

#Climate_Region_Pub

All2009$Climate_Region_Pub<-revalue(as.factor(All2009$Climate_Region_Pub),c("1"=" VeryCold/Cold","2"="Hot.Dry/Mixed.Dry","3"="Hot.Humid","4"="Mixed.Humid","5"="Marine"))

summary(All2009$Climate_Region_Pub)

summary



#AIA_ZONE issue with revalue producing results

All2009$AIA_ZONE<-revalue(as.factor(All2009$AIA_Zone),
                          c("1"="lessthan2000CDD_greaterthan_7000HDD","2"=
                              "lessthan2000CDD_between5500-7000HDD"
                            ,"3"="lessthan2000CDD_between4000-5499HDD","4"=
                              "lessthan2000CDD_lessthan4000DD",
                            "5"="greaterthan2000CDD_lessthan4000HDD"))



summary(All2009$AIA_Zone)

#METROMICRO (No NA values)

summary(is.na(All2009$METROMICRO))

#UR

All2009$UR<-revalue(as.factor(All2009$UR),c("U"="Urban","R"="Residential"))

summary(All2009$UR)


#KOWNRENT

All2009$KOWNRENT<-revalue(as.factor(All2009$KOWNRENT),
                          c("1"="Ownded","2"="Rented","3"="Occupied"))

summary(All2009$KOWNRENT)

#CONDCOOP  

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

#Conversion

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

#NUMAPTS  Every value is accounted for in the table however when I look at the
#summary there is 9 values labled as 'other'.  I went throught the entire 
#column and could not find any such values that do not match the code or table.
#NOTE: The NA values match the '-2' values

All2009$NUMAPTS<-revalue(as.factor(All2009$NUMAPTS),c("5"="5units","6"="6units","7"="7units","8"="8units","9"="9units","10"="10units","11"="11units","12"="12units","13"="13units","14"="14units","15"="15units","16"="16units","17"="17units","18"="18units","19"="19units","20"="20units","21"="21units","22"="22units","23"="23units","24"="24units","25"="25units","26"="26units","27"="27units","28"="28units","30"="30units","31"="31units","32"="32units","33"="33units","34"="34units","35"="35units","36"="36units","37"="37units","39"="39units","40"="40units","41"="41units","42"="42units","43"="43units","44"="44units","45"="45units","46"="46units","47"="47units","48"="48units","49"="49units","50"="50units","51"="51units","52"="52units","53"="53units","54"="54units","55"="55units","56"="56units","57"="57units","58"="58units","59"="59units","60"="60units","63"="63units","64"="64units","65"="65units","66"="66units","68"="68units","70"="70units","72"="72units","75"="75units","76"="76units","80"="80units","81"="81units","84"="84units","86"="86units","88"="88units","90"="90units","95"="95units","96"="96units","99"="99units","100"="100units","102"="102units","105"="105units","108"="108units","110"="110units","112"="112units","115"="115units","116"="116units","118"="118units","120"="120units","123"="123units","125"="125units","126"="126units","130"="130units","133"="133units","135"="135units","140"="140units","141"="141units","145"="145units","150"="150units","155"="155units","160"="160units","168"="168units","170"="170units","175"="175units","178"="178units","180"="180units","187"="187units","192"="192units","199"="199units","200"="200units","313"="313units","316"="316units","322"="322units","365"="365units"))

summary(All2009$NUMAPTS)

#WALLTYPE

All2009$WALLTYPE<-revalue(as.factor(All2009$WALLTYPE),c("1"="Brick","2"="Wood","3"="Siding","4"="Stucco","5"="Composition","6"="Stone","7"="Concrete","8"="Glass","9"="Other"))

summary(All2009$WALLTYPE)

#ROOFTYPE

All2009$ROOFTYPE[All2009$ROOFTYPE==-2]<-NA

All2009$ROOFTYPE<-revalue(as.factor(All2009$ROOFTYPE),c("1"="Ceramic.Clay","2"="WoodShingles.Shakes","3"="Metal","4"="Slate.SyntheticSlate","5"="CompositionShingles","6"="Asphalt","7"="ConcreteTiles","8"="Other"))

summary(All2009$ROOFTYPE)

#STUDIO

All2009$STUDIO[All2009$STUDIO==-2]<-NA

All2009$STUDIO<-revalue(as.factor(All2009$STUDIO),c("0"="NO","1"="YES"))

summary(All2009$STUDIO)

#NAPTFLRS

table(All2009$NAPTFLRS)

All2009$NAPTFLRS[All2009$NAPTFLRS==-2]<-NA

All2009$NAPTFLRS<-revalue(as.factor(All2009$NAPTFLRS),c("1"="1Floor","2"="2Floors","3"="3Floors"))

summary(All2009$NAPTFLRS)

#STORIES

summary(is.na(All2009$STORIES))

All2009$STORIES[All2009$STORIES==-2]<- NA

All2009$STORIES<-revalue(as.factor(All2009$STORIES),c("10"="OneStory","20"="TwoStories","31"="ThreeStories","32"="FourPlusStories","40"="SplitLevel","50"="OtherType"))

summary(All2009$STORIES)

#TYPEHUQ$ (addition to mobile home)

summary(is.na(All2009$TYPEHUQ4))

All2009$TYPEHUQ4[All2009$TYPEHUQ4==-2]<-NA

All2009$TYPEHUQ4<-revalue(as.factor(All2009$TYPEHUQ4),c("0"="NO","1"="YES"))

#BEDROOMS

table(All2009$BEDROOMS)

All2009$BEDROOMS[All2009$BEDROOMS==-2]<-NA

All2009$BEDROOMS<-revalue(as.factor(All2009$BEDROOMS),c("0"="NoBedrooms","1"="OneBedroom","2"="TwoBedrooms","3"="ThreeBedrooms","4"="FourBedrooms","5"="FiveBedrooms","6"="SixBedrooms","7"="SevenBedrooms","8"="EightBedrooms","9"="NineBedrooms","13"="ThirteenBedrooms"))

summary(All2009$BEDROOMS)

#NCOMBATH

table(All2009$NCOMBATH)

All2009$NCOMBATH[All2009$NCOMBATH==-2]<-NA

All2009$NCOMBATH<-revalue(as.factor(All2009$NCOMBATH),c("0"="NoFullBath","1"="OneFullBath","2"="TwoFullBath","3"="ThreeFullBath","4"="FourFullBath","5"="FiveFullBath","6"="SixFullBath","7"="SevenFullBath","8"="EightFullBath"))

summary(All2009$NCOMBATH)
#NHAFBATH

table(All2009$NHAFBATH)

All2009$NHAFBATH<-revalue(as.factor(All2009$NHAFBATH),c("0"="NoHalfBath","1"="OneHalfBath","2"="TwoHalfBath","3"="ThreeHalfBath","9"="NineHalfBath"))

summary(All2009$NHAFBATH)

#OTHROOMS

table(All2009$OTHROOMS)

All2009$OTHROOMS<-revalue(as.factor(All2009$OTHROOMS),c("1"="OneOtherRoom","2"="TwoOtherRooms","3"="ThreeOtherRooms","4"="FourOtherRooms","5"="FiveOtherRooms","6"="SixOtherRooms","7"="SevenOtherRooms","8"="EightOtherRooms","9"="NineOtherRomms","10"="TenOtherRooms","11"="ElevenOtherRooms","12"="TwelveOtherRooms","13"="ThriteenOtherRooms","14"="FourteenOtherRooms","17"="SeventeenOtherRooms"))

summary(All2009$OTHROOMS)

#TOTROOMS

table(All2009$TOTROOMS)
                         
All2009$TOTROOMS<-revalue(as.factor(All2009$TOTROOMS),c("1"="OneRoom","2"="TwoRooms","3"="ThreeRooms","4"="FourRooms","5"="FiveRooms","6"="SixRooms","7"="SevenRooms","8"="EightRooms","9"="NineRooms","10"="TenRooms","11"="ElevenRooms","12"="TwelveRooms","13"="ThirteenRooms","14"="FourteenRooms","15"="FifteenRooms","16"="SixteenRooms","18"="EighteenRooms","19"="NineteenRooms","21"="TwentyOneRooms","23"="TwentyThreeRooms"))

summary(All2009$TOTROOMS)

#STOVEN (Number of Stoves with oven)

table(All2009$STOVEN)

All2009$STOVEN<-revalue(as.factor(All2009$STOVEN),c("0"="NoStove","1"="1Stove","2"="TwoStoves","3"="ThreeStoves"))

summary(All2009$STOVEN)

#STOVENFUEL (Fuel used by most frequently used stove)

All2009$STOVENFUEL[All2009$STOVENFUEL==-2]<-NA

All2009$STOVENFUEL<-revalue(as.factor(All2009$STOVENFUEL),c("1"="NaturalGas","2"="Propane/LPG","5"="Electricity","21"="OtherFuel"))

summary(All2009$STOVENFUEL)

#STOVE Number of sepeate cooktops

table(All2009$STOVE)

All2009$STOVE<-revalue(as.factor(All2009$STOVE),c("0"="ZeroCookTops","1"="OneCookTop","2"="TwoCookTops"))

summary(All2009$STOVE)

#STOVEFUEL

All2009$STOVEFUEL[All2009$STOVEFUEL==-2]<-NA

All2009$STOVEFUEL<-revalue(as.factor(All2009$STOVEFUEL),c("1"="NaturalGas","2"="Propane/LPG","5"="Electricity","21"="OtherFuel"))

summary(All2009$STOVEFUEL)

#OVEN

table(All2009$OVEN)

All2009$OVEN<-revalue(as.factor(All2009$OVEN),c("0"="NoOven","1"="OneOven","2"="TwoOvens","3"="ThreeOvens"))

summary(All2009$OVEN)

#OVENFUEL 

All2009$OVENFUEL[All2009$OVENFUEL==-2]<-NA

All2009$OVENFUEL<-revalue(as.factor(All2009$OVENFUEL),c("1"="NaturalGas","2"="Propane/LPG","5"="Electricity","21"="OtherFuel"))

summary(All2009$OVENFUEL)

#OVENUSE

All2009$OVENUSE[All2009$OVENUSE==-2]<-NA

All2009$OVENUSE<-revalue(as.factor(All2009$OVENUSE),c("0"="NotUsed","1"="3Plus/Day","2"="Two/Day","3"="Once/Day","4"="Few/Week","5"="Once/Week","6"="LessThanOnce/Week"))

summary(All2009$OVENUSE)

#MICRO (Microwave Used)

All2009$MICRO<-revalue(as.factor(All2009$MICRO),c("0"="NO","1"="YES"))

summary(All2009$MICRO)

#AMTMICRO

All2009$AMTMICRO[All2009$AMTMICRO==-2]<-NA

All2009$AMTMICRO<-revalue(as.factor(All2009$AMTMICRO),c("1"="Cook/ReheatMostMeals","2"="Cook/ReheasHalfMeals","3"="Cook/ReheatFewMeals","4"="UsedVeryLittle"))

summary(All2009$AMTMICRO)

#OUTGRILL

All2009$OUTGRILL<-revalue(as.factor(All2009$OUTGRILL),c("0"="NO","1"="YES"))

summary(All2009$OUTGRILL)

#OUTGRILLFUEL

All2009$OUTGRILLFUEL[All2009$OUTGRILLFUEL==-2]<-NA

All2009$OUTGRILLFUEL<-revalue(as.factor(All2009$OUTGRILLFUEL),c("1"="NaturalGas","2"="Propane/LPG","21"="OtherFuel"))

summary(All2009$OUTGRILLFUEL)

#TOASTER

All2009$TOASTER<-revalue(as.factor(All2009$TOASTER),c("0"="NO","1"="YES"))

summary(All2009$TOASTER)

#NUMMEAL

All2009$NUMMEAL<-revalue(as.factor(All2009$NUMMEAL),c("0"="NeverCooks","1"="ThreePlusTimes/Day","2"="TwoTimes/Day","3"="Once/Day","4"="FewTimes/Week","5"="Once/Week","6"="LessThanOnce/Week"))

summary(All2009$NUMMEAL)

#FUELFOOD

All2009$FUELFOOD[All2009$FUELFOOD==-2]<-NA

All2009$FUELFOOD<-revalue(as.factor(All2009$FUELFOOD),c("1"="NaturalGas","2"="Propane/LPG","5"="Electricity","21"="OtherFuel"))

summary(All2009$FUELFOOD)

#NUMFRIG

table(All2009$NUMFRIG)

All2009$NUMFRIG<-revalue(as.factor(All2009$NUMFRIG),c("0"="NoFridege","1"="OneFridge","2"="TwoFridge","3"="ThreeFridge","4"="FourFidge","5"="FiveFridge","6"="SixFridge","7"="SevenFridge"))

summary(All2009$NUMFRIG)

#TYPERFR1

All2009$TYPERFR1[All2009$TYPERFR1==-2]<-NA

All2009$TYPERFR1<-revalue(as.factor(All2009$TYPERFR1),c("1"="Fullsize/OneDoor","3"="Halfsize/Compact","4"="Otherkind","5"="Fullsize/ThreePlusDoors","21"="FullsizeTwoDoors/FreezerNext","22"="FullsizeTwoDoors/FreezerAbove","23"="FullsizeTwodoors/Freezerbelow"))

summary(All2009$TYPERFR1)

#SIZRFRI1

All2009$SIZRFRI1[All2009$SIZRFRI1==-2]<-NA

All2009$SIZRFRI1<-revalue(as.factor(All2009$SIZRFRI1),c("1"="Halfsize/Compact","2"="Small","3"="Medium","4"="Large","5"="VeryLarge"))

summary(All2009$SIZRFRI1)

#REFRIFT1 (Defrosting Type of most commonly used fridge)

All2009$REFRIGT1[All2009$REFRIGT1==-2]<-NA

All2009$REFRIGT1<-revalue(as.factor(All2009$REFRIGT1),c("1"="Manual","2"="FrostFree","3"="NoWorkingFreezer"))

summary(All2009$REFRIGT1)

#ICE (Throught the door ice/water, most common fridge)

All2009$ICE[All2009$ICE==-2]<-NA

All2009$ICE<-revalue(as.factor(All2009$ICE),c("0"="NO","1"="YES"))

summary(All2009$ICE)

#AGERFRI1

All2009$AGERFRI1[All2009$AGERFRI1==-2]<-NA

All2009$AGERFRI1<-revalue(as.factor(All2009$AGERFRI1),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGERFRI1)

#ESFRIG

All2009$ESFRIG[All2009$ESFRIG==-2]<-NA
All2009$ESFRIG[All2009$ESFRIG==-8]<-NA
All2009$ESFRIG[All2009$ESFRIG==-9]<-NA

All2009$ESFRIG<-revalue(as.factor(All2009$ESFRIG),c("0"="NO","1"="YES"))

summary(All2009$ESFRIG)

#REPLCFRI

All2009$REPLCFRI[All2009$REPLCFRI==-2]<-NA
All2009$REPLCFRI[All2009$REPLCFRI==-8]<-NA
All2009$REPLCFRI[All2009$REPLCFRI==-9]<-NA

All2009$REPLCFRI<-revalue(as.factor(All2009$REPLCFRI),c("0"="NO","1"="YES"))

summary(All2009$REPLCFRI)

#TYPERFR2

All2009$TYPERFR2[All2009$TYPERFR2==-2]<-NA

All2009$TYPERFR2<-revalue(as.factor(All2009$TYPERFR2),c("1"="Fullsize/OneDoor","3"="Halfsize/Compact","4"="Otherkind","5"="Fullsize/ThreePlusDoors","21"="FullsizeTwoDoors/FreezerNext","22"="FullsizeTwoDoors/FreezerAbove","23"="FullsizeTwodoors/Freezerbelow"))

summary(All2009$TYPERFR2)

#SIZRFRI2

All2009$SIZRFRI2[All2009$SIZRFRI2==-2]<-NA

All2009$SIZRFRI2<-revalue(as.factor(All2009$SIZRFRI2),c("1"="Halfsize/Compact","2"="Small","3"="Medium","4"="Large","5"="VeryLarge"))

summary(All2009$SIZRFRI2)

#MONRFRI2

table(All2009$MONRFRI2)

All2009$MONRFRI2[All2009$MONRFRI2==-2]<-NA

All2009$MONRFRI2<-revalue(as.factor(All2009$MONRFRI2),c("0"="NotUsed","1"="OneMonth","2"="TwoMonths","3"="ThreeMonths","4"="FourMonths","5"="FiveMonths","6"="SixMonths","7"="SevenMonths","8"="EightMonths","9"="NineMonths","10"="TenMonths","11"="ElevenMonths","12"="TwelveMonths"))

summary(All2009$MONRFRI2)

#AGERFRI2

All2009$AGERFRI2[All2009$AGERFRI2==-2]<-NA

All2009$AGERFRI2<-revalue(as.factor(All2009$AGERFRI2),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGERFRI2)

#ESFRIG2

All2009$ESFRIG2[All2009$ESFRIG2==-2]<-NA
All2009$ESFRIG2[All2009$ESFRIG2==-8]<-NA
All2009$ESFRIG2[All2009$ESFRIG2==-9]<-NA

All2009$ESFRIG2<-revalue(as.factor(All2009$ESFRIG2),c("0"="NO","1"="YES"))

summary(All2009$ESFRIG2)

#SEPFREEZ

All2009$SEPFREEZ<-revalue(as.factor(All2009$SEPFREEZ),c("0"="NO","1"="YES"))

summary(All2009$SEPFREEZ)

#NUMFREEZ

All2009$NUMFREEZ[All2009$NUMFREEZ==-2]<-NA

All2009$NUMFREEZ<-revalue(as.factor(All2009$NUMFREEZ),c("1"="One","2"="Two","3"="Three"))

summary(All2009$NUMFREEZ)

#UPRTFRZR

All2009$UPRTFRZR[All2009$UPRTFRZR==-2]<-NA

All2009$UPRTFRZR<-revalue(as.factor(All2009$UPRTFRZR),c("1"="Upright","2"="Chest"))

summary(All2009$UPRTFRZR)

#SIZFREEZ

All2009$SIZFREEZ[All2009$SIZFREEZ==-2]<-NA

All2009$SIZFREEZ<-revalue(as.factor(All2009$SIZFREEZ),c("1"="Small","2"="Medium","3"="Large","4"="VeryLarge"))

summary(All2009$SIZFREEZ)

#REPLCFRZ

All2009$REPLCFRZ[All2009$REPLCFRZ==-2]<-NA
All2009$REPLCFRZ[All2009$REPLCFRZ==-8]<-NA
All2009$REPLCFRZ[All2009$REPLCFRZ==-9]<-NA

All2009$REPLCFRZ<-revalue(as.factor(All2009$REPLCFRZ),c("0"="NO","1"="YES"))

summary(All2009$REPLCFRZ)

#UPRTFRZR2

All2009$UPRTFRZR2[All2009$UPRTFRZR2==-2]<-NA

All2009$UPRTFRZR2<-revalue(as.factor(All2009$UPRTFRZR2),c("1"="Upright","2"="Chest"))

summary(All2009$UPRTFRZR2)

#SIZFREEZ2

All2009$SIZFREEZ2[All2009$SIZFREEZ2==-2]<-NA

All2009$SIZFREEZ2<-revalue(as.factor(All2009$SIZFREEZ2),c("1"="Small","2"="Medium","3"="Large","4"="VeryLarge"))

summary(All2009$SIZFREEZ2)

#AGEFRZR2

All2009$AGEFRZR2[All2009$AGEFRZR2==-2]<-NA

All2009$AGEFRZR2<-revalue(as.factor(All2009$AGEFRZR2),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGEFRZR2)

#DISHWASH

All2009$DISHWASH<-revalue(as.factor(All2009$DISHWASH),c("0"="NO","1"="YES"))

summary(All2009$DISHWASH)

#DWASHUSE

All2009$DWASHUSE[All2009$DWASHUSE==-2]<-NA

All2009$DWASHUSE<-revalue(as.factor(All2009$DWASHUSE),c("11"="<Once/Week","12"="Once/Week","13"="2-3/Week","20"="4-6/Week","30"=">=Once/Day"))

summary(All2009$DWASHUSE)

#AGEDW

All2009$AGEDW[All2009$AGEDW==-2]<-NA

All2009$AGEDW<-revalue(as.factor(All2009$AGEDW),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGEDW)

#ESDISHW

All2009$ESDISHW[All2009$ESDISHW==-2]<-NA
All2009$ESDISHW[All2009$ESDISHW==-8]<-NA
All2009$ESDISHW[All2009$ESDISHW==-9]<-NA

All2009$ESDISHW<-revalue(as.factor(All2009$ESDISHW),c("0"="NO","1"="YES"))

summary(All2009$ESDISHW)

#REPLCDW

All2009$REPLCDW[All2009$REPLCDW==-2]<-NA
All2009$REPLCDW[All2009$REPLCDW==-8]<-NA
All2009$REPLCDW[All2009$REPLCDW==-9]<-NA

All2009$REPLCDW<-revalue(as.factor(All2009$REPLCDW),c("0"="NO","1"="YES"))

summary(All2009$REPLCDW)

#CWASHER

All2009$CWASHER<-revalue(as.factor(All2009$CWASHER),c("0"="NO","1"="YES"))

summary(All2009$CWASHER)

#TOPFRONT

All2009$TOPFRONT[All2009$TOPFRONT==-2]<-NA

All2009$TOPFRONT<-revalue(as.factor(All2009$TOPFRONT),c("1"="TopLoad","2"="FrontLoad"))

summary(All2009$TOPFRONT)

#WASHLOAD

All2009$WASHLOAD[All2009$WASHLOAD==-2]<-NA

All2009$WASHLOAD<-revalue(as.factor(All2009$WASHLOAD),c("1"="<=One/Week","2"="2-4/Week","3"="5-9/Week","4"="10-14/Week","5"=">15/Week"))

summary(All2009$WASHLOAD)

#WASHTEMP

All2009$WASHTEMP[All2009$WASHTEMP==-2]<-NA

All2009$WASHTEMP<-revalue(as.factor(All2009$WASHTEMP),c("1"="Hot","2"="Warm","3"="Cold"))

summary(All2009$WASHTEMP)

#RNSETEMP

All2009$RNSETEMP[All2009$RNSETEMP==-2]<-NA

All2009$RNSETEMP<-revalue(as.factor(All2009$RNSETEMP),c("1"="Hot","2"="Warm","3"="Cold"))

summary(All2009$RNSETEMP)

#AGECWASH

All2009$AGECWASH[All2009$AGECWASH==-2]<-NA

All2009$AGECWASH<-revalue(as.factor(All2009$AGECWASH),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGECWASH)

#ESCWASH

All2009$ESCWASH[All2009$ESCWASH==-2]<-NA
All2009$ESCWASH[All2009$ESCWASH==-8]<-NA
All2009$ESCWASH[All2009$ESCWASH==-9]<-NA

All2009$ESCWASH<-revalue(as.factor(All2009$ESCWASH),c("0"="NO","1"="YES"))

summary(All2009$ESCWASH)

#DRYER

All2009$DRYER<-revalue(as.factor(All2009$DRYER),c("0"="NO","1"="YES"))

summary(All2009$DRYER)

#DRYERFUEL

All2009$DRYRFUEL[All2009$DRYRFUEL==-2]<-NA

All2009$DRYRFUEL<-revalue(as.factor(All2009$DRYRFUEL),c("1"="NaturalGas","2"="Propane/LPG","5"="Electrcity"))

summary(All2009$DRYRFUEL)

#DRYERUSE

All2009$DRYRUSE[All2009$DRYRUSE==-2]<-NA

All2009$DRYRUSE<-revalue(as.factor(All2009$DRYRUSE),c("1"="EveryWash","2"="Sometimes/Wash","3"="InfrequentUse"))

summary(All2009$DRYRUSE)

#AGECDRYER

All2009$AGECDRYER[All2009$AGECDRYER==-2]<-NA

All2009$AGECDRYER<-revalue(as.factor(All2009$AGECDRYER),c("1"="<TwoYears","2"="2-4Years","3"="5-9Years","41"="10-14Years","42"="15-19Years","5"=">20Years"))

summary(All2009$AGECDRYER)

#TVCOLOR

table(All2009$TVCOLOR)

All2009$TVCOLOR<-revalue(as.factor(All2009$TVCOLOR),c("0"="NoTV","1"="OneTV","2"="TwoTVs","3"="ThreeTVs","4"="FourTVs","5"="FiveTVs","6"="SixTvs","7"="SevenTvs","8"="EightTvs","9"="NineTvs","10"="TenTvs","12"="TwelveTvs","14"="FourteenTvs"))

summary(All2009$TVCOLOR)

#TVSIZE

All2009$TVSIZE1[All2009$TVSIZE1==-2]<-NA

All2009$TVSIZE1<-revalue(as.factor(All2009$TVSIZE1),c("1"="<=20inch","2"="21-26inch","3"=">=37inch"))

summary(All2009$TVSIZE1)

#TVTYPE

