#CELLAR

All2009$CELLAR[All2009$CELLAR==-2]<-NA

All2009$CELLAR<-revalue(as.factor(All2009$CELLAR),c("0"="NO","1"="YES"))

summary(All2009$CELLAR)

#CRAWL

All2009$CRAWL[All2009$CRAWL==-2]<-NA

All2009$CRAWL<-revalue(as.factor(All2009$CRAWL),c("0"="NO","1"="YES"))

summary(All2009$CRAWL)

#CONCRETE

All2009$CONCRETE[All2009$CONCRETE==-2]<-NA

All2009$CONCRETE<-revalue(as.factor(All2009$CONCRETE),c("0"="NO","1"="YES"))

summary(All2009$CONCRETE)

#BASEFIN (Finished Basement)

All2009$BASEFIN[All2009$BASEFIN==-2]<-NA

All2009$BASEFIN<-revalue(as.factor(All2009$BASEFIN),c("0"="NO","1"="YES"))

summary(All2009$BASEFIN)

#FINBASERMS (Rooms in finsihed basement)

All2009$FINBASERMS[All2009$FINBASERMS==-2]<-NA