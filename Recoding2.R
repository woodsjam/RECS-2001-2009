#May have fixed the problem

All2009<-recs2009_public

library(plyr)

library(reshape)

#AIA_ZONE

All2009$AIA_ZONE<-revalue(as.factor(All2009$AIA_Zone),c("1"="<2000CDD.>7000HDD","2"="<2000CDD.5500-7000HDD","3"="<2000CDD.4000-5499HDD","4"="<2000CDD.<4000DD","5"=">2000CDD.<4000HDD"))

summary(All2009$AIA_Zone)


