#Starting from NUMAPTS

recs2009_public <- read.csv("2009/recs2009_public.csv")


All2009<-recs2009_public

library(plyr)

library(reshape)


#NUMAPTS  Every value is accounted for in the table however when I look at the
#summary there is 9 values labled as 'other'.  I went throught the entire 
#column and could not find any such values that do not match the code or table.
#NOTE: The NA values match the '-2' values

table(All2009$NUMAPTS)

All2009$NUMAPTS[All2009$NUMAPTS==-2]<-NA

All2009$NUMAPTS<-revalue(as.factor(All2009$NUMAPTS),c("5"="5units","6"="6units","7"="7units","8"="8units","9"="9units","10"="10units","11"="11units","12"="12units","13"="13units","14"="14units","15"="15units","16"="16units","17"="17units","18"="18units","19"="19units","20"="20units","21"="21units","22"="22units","23"="23units","24"="24units","25"="25units","26"="26units","27"="27units","28"="28units","30"="30units","31"="31units","32"="32units","33"="33units","34"="34units","35"="35units","36"="36units","37"="37units","39"="39units","40"="40units","41"="41units","42"="42units","43"="43units","44"="44units","45"="45units","46"="46units","47"="47units","48"="48units","49"="49units","50"="50units","51"="51units","52"="52units","53"="53units","54"="54units","55"="55units","56"="56units","57"="57units","58"="58units","59"="59units","60"="60units","63"="63units","64"="64units","65"="65units","66"="66units","68"="68units","70"="70units","72"="72units","75"="75units","76"="76units","80"="80units","81"="81units","84"="84units","86"="86units","88"="88units","90"="90units","95"="95units","96"="96units","99"="99units","100"="100units","102"="102units","105"="105units","108"="108units","110"="110units","112"="112units","115"="115units","116"="116units","118"="118units","120"="120units","123"="123units","125"="125units","126"="126units","130"="130units","133"="133units","135"="135units","140"="140units","141"="141units","145"="145units","150"="150units","155"="155units","160"="160units","168"="168units","170"="170units","175"="175units","178"="178units","180"="180units","187"="187units","192"="192units","199"="199units","200"="200units","313"="313units","316"="316units","322"="322units","365"="365units"))

summary(All2009$NUMAPTS)



