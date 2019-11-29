
# Library Loading
library(RCurl)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(speedglm)
library(tibble)
library(ggplot2)
library(tidyverse)
library(geosphere)

# Climate Data Downloading, Conversion to binary files and create R objects for variables

date_seq <- seq(as.POSIXct("2010-01-01",tz="UTC"),as.POSIXct("2016-12-31",tz="UTC"),by="d")

download_folder <- c("U2M","U10M","U50M","V2M","V10M","V50M","DISPH","T2M","T10M")

bin_folder <- c(paste("",getwd(),"/U2M",sep=""),
                paste("",getwd(),"/U10M",sep=""),
                paste("",getwd(),"/U50M",sep=""),
                paste("",getwd(),"/V2M",sep=""),
                paste("",getwd(),"/V10M",sep=""),
                paste("",getwd(),"/V50M",sep=""),
                paste("",getwd(),"/DISPH",sep=""),
                paste("",getwd(),"/T2M",sep=""),
                paste("",getwd(),"/T10M",sep=""))

params<-c("U2M","U10M","U50M","V2M","V10M","V50M","DISPH","T2M","T10M") 

mapply(getMERRADataBox,
       download_folder,
       c(lon1),
       c(lat1),
       c(lon2),
       c(lat2),
       list(date_seq),
       download_folder,
       c("s_hoelt"),
       c("getMERRAData2017"),
       c(TRUE),
       avg="TRUE",
       dim="2d")

mapply(convMerraToBin,download_folder,bin_folder,list(date_seq),params,c(100))

u10m<-MERRABin(paste("",getwd(),"/U10M",sep=""))
u50m<-MERRABin(paste("",getwd(),"/U50M",sep=""))

v10m<-MERRABin(paste("",getwd(),"/V10M",sep=""))
v50m<-MERRABin(paste("",getwd(),"/V50M",sep=""))

t2m<-MERRABin(paste("",getwd(),"/T2M",sep=""))

# Climate Data subsetting

getts<-function(sr,ds){
  gpoints<-expand.grid(ds$lons,ds$lats)
  names(gpoints)<-c("lons","lats")
  srt<-data.frame(sr$lon,sr$lat)
  tr<-matrix(unlist(sapply(1:length(srt$sr.lat),function(i,srt)
  { print(i/length(srt$sr.lat))
    dista <<- distHaversine(srt[i,],gpoints)
    #distdat<-data.frame(gpoints,dista)
    nearest<- data.frame(gpoints[which.min(dista),1],gpoints[which.min(dista),2])
    #dista2<- distHaversine(nearest,gpoints)
    return (nearest)
  },srt=srt
    )
),ncol=2,byrow=TRUE)
}

getts3<-function(sr,ds){
  gpoints<-expand.grid(ds$lons,ds$lats)
  names(gpoints)<-c("lons","lats")
  srt<-data.frame(sr$lon,sr$lat)
  tr<-lapply(1:length(srt$sr.lat),function(i,srt)
  { print(i/length(srt$sr.lat))
    dista <- distHaversine(srt[i,],gpoints)
    distax <-sort(dista,decreasing=FALSE)
    #distdat<-data.frame(gpoints,dista)
    nearest<- data.frame(gpoints[which(dista%in%distax[1:4]),1],gpoints[which(dista%in%distax[1:4]),2])
    #print(srt[i,])
    #print(nearest)
    #dista2<- distHaversine(nearest,gpoints)
    return (nearest)
  },srt=srt
    )
}

getts2<-function(sr,ds){
  gpoints<-expand.grid(ds$lons,ds$lats)
  names(gpoints)<-c("lons","lats")
  srt<<-data.frame(sr$lon,sr$lat)
  tr<<-lapply(1:length(srt[,1]),function(i,srt)
  { print(i/length(srt[,1]))
    #dista <- distHaversine(srt[i,],gpoints)
    #distdat<-data.frame(gpoints,dista)
    #nearest<- data.frame(gpoints[which.min(dista),1],gpoints[which.min(dista),2])
    dista2<- data.frame(gpoints,distHaversine(srt[i,],gpoints))
    distat<<-dista2[order(dista2[,3]),]
    distat2<<-data.frame(distat[1:4,1:2])
    return (distat2)
  },srt=srt
    )
}

getts2<-function(sr,ds){
  gpoints<-expand.grid(ds$lons,ds$lats)
  names(gpoints)<-c("lons","lats")
  srt<<-data.frame(sr[,1],sr[,2])
  tr<-lapply(1:length(srt[,1]),function(i,srt)
  { print(i/length(srt[,1]))
    #dista <- distHaversine(srt[i,],gpoints)
    #distdat<-data.frame(gpoints,dista)
    #nearest<- data.frame(gpoints[which.min(dista),1],gpoints[which.min(dista),2])
    dista2<- data.frame(gpoints,distHaversine(srt[i,],gpoints))
    distat<-dista2[order(dista2[,3]),]
    distat2<<-data.frame(distat[1:4,1:2])
    return (distat2)
  },srt=srt
    )
}



print(1/length(srt[,1]))
    #dista <- distHaversine(srt[i,],gpoints)
    #distdat<-data.frame(gpoints,dista)
    #nearest<- data.frame(gpoints[which.min(dista),1],gpoints[which.min(dista),2])
    dista2<- data.frame(gpoints,distHaversine(srt[1,],gpoints))
    distat<-dista2[order(dista2[,3]),]
    distat2<<-data.frame(distat[1:4,1:2])
    return (distat2)

ds<-u10m
sr<-wcap

"C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/renewable_power_plants_DE_new.csv"

cap<-read_delim("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/time_series_installed capacities.csv",delim=",")
wcap<-cap %>% 
  dplyr::select(commissioning_date,decommissioning_date,energy_source_level_2,technology,electrical_capacity,lat,lon) 
wcap %>% mutate(start=commissioning_date,end=decommissioning_date,tech=energy_source_level_2,type=technology,capacity=as.numeric(electrical_capacity),latitude=as.numeric(lat),longitude=as.numeric(lon))
wcap<-wcap %>%   dplyr::filter(energy_source_level_2=="Wind")
#wcapOn<-wcap %>%   dplyr::filter(technology=="Onshore")
wcapOn<-wcap %>%   dplyr::filter(!is.na(commissioning_date))
#wcapOn<-wcapOn %>%   dplyr::filter(is.na(decommissioning_date))
#wcapOn<-wcapOn %>%   dplyr::filter(as.numeric(decommissioning_date)<as.numeric(enddate))
wcapOn<-wcapOn %>%   dplyr::filter(is.na(decommissioning_date) | as.numeric(decommissioning_date)>as.numeric(enddate))
wcapOn<-wcapOn %>%   dplyr::filter(as.numeric(commissioning_date)<as.numeric(enddate))
wcapOnt<-cumsum(wcapOn$electrical_capacity)
wcapOn<-data.frame(wcapOn,wcapOnt)
names(wcapOn$wcapOnt)<-c("cum_inst_cap")
wcapOn<-wcapOn[which(!is.na(wcapOn$lat)),]

cap<-read_delim("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/renewable_power_plants_DE_new.csv",delim=",")

c<-getts(wcapOn,u10m)
un<-unique(c)

c2<-getts2(un,u10m)
cc2<-ldply(c2,data.frame)
un2<-unique(cc2)

c3<-getts3(wcapOn,u10m)
cc3<-ldply(c3,data.frame)
un3<-unique(cc3)

srt<-data.frame(wcapOn$lon[1],wcapOn$lat[1])
dista <- distHaversine(srt[1,],gpoints)
distax <-sort(dista,decreasing=FALSE)
#distdat<-data.frame(gpoints,dista)
nearest<- data.frame(gpoints[which(dista%in%distax[1:4]),1],gpoints[which(dista%in%distax[1:4]),2])

c<-getts2(wcapOn,u10m)

c<-getts(wcap,u10m)

cx<-ldply(c)

un<-unique(c)
un2<-unique(cx)

tsh<-lapply(1:length(un2[,1]), function(i,un2,u10m,u50m,v10m,v50m,t2m){
    
   
   if(!is.numeric(u10m$getClosestTS(un2[i,1],un2[i,2]))){u10mw<-0} else{u10mw<-u10m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(u50m$getClosestTS(un2[i,1],un2[i,2]))){u50mw<-0} else{u50mw<-u50m$getClosestTS(un2[i,1],un2[i,2])}
   
   if(!is.numeric(v10m$getClosestTS(un2[i,1],un2[i,2]))){v10mw<-0} else{v10mw<-v10m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(v50m$getClosestTS(un2[i,1],un2[i,2]))){v50mw<-0} else{v50mw<-v50m$getClosestTS(un2[i,1],un2[i,2])}
   
   if(!is.numeric(t2m$getClosestTS(un2[i,1],un2[i,2]))){t2mw<-0} else{t2mw<-t2m$getClosestTS(un2[i,1],un2[i,2])}
   

    dt<-data.frame(u10mw,u50mw,v10mw,v50mw,t2mw)
   
   },un2=un2,u10m=u10m,u50m=u50m,v10m=v10m,v50m=v50m,t2m=t2m)

tstm<-matrix(unlist(tsh),ncol=length(tsh)*5,byrow=FALSE)


# Installed capacity proportions

ind<-sapply(1:length(c[,1]),function(j){
          ind<-which((un[,1]==c[j,1])&(un[,2]==c[j,2]))
    })

ind2<-sapply(1:length(cc2[,1]),function(j){
          ind2<-which((un2[,1]==cc2[j,1])&(un2[,2]==cc2[j,2]))
    })

ind3<-sapply(1:length(cc3[,1]),function(j){
          ind<-which((un3[,1]==cc3[j,1])&(un3[,2]==cc3[j,2]))
    })


rcap<-data.frame(tab_select1$utc_timestamp,sapply(tab_select1$utc_timestamp,recCap,wcapOn$wcapOnt,wcapOn$commissioning_date))
names(rcap)<-c("date","rcapacity")


##system.time(capm<-lapply(1:length(tab_select1$utc_timestamp),function(i,x,y,un,c,ind){
  #  print(i/length(tab_select1$utc_timestamp))
   # dateind<-which(y$commissioning_date<=x$utc_timestamp[i])
  #  cpprt<-y[1:max(dateind),5]
   # capm<-sapply(1:length(un[,1]),function(k,ind){
  #  capm<-sum(cpprt[which(ind[1:length(cpprt)]==k)])/rcap[i,2]
   # },ind=ind)
  #  },x=tab_select1,y=wcap,un=un2,c=c,ind=ind))

system.time(capmxl<-lapply(1:length(tab_select1$utc_timestamp),function(i,x,y,un,c,ind){
    print(i/length(tab_select1$utc_timestamp))
    dateind<-which(y$commissioning_date<=x$utc_timestamp[i])
    cpprt<-y[1:max(dateind),5]
    capm<-sapply(1:length(un[,1]),function(k,ind){
    capm<-sum(cpprt[which(ind[1:length(cpprt)]==k)])#/sum(cpprt)
    },ind=ind)
    },x=tab_select1,y=wcapOn,un=un,c=c,ind=ind))

system.time(capm2<-lapply(1:length(tab_select1$utc_timestamp),function(i,x,y,un2,c,ind){
    print(i/length(tab_select1$utc_timestamp))
    dateind<-which(y$commissioning_date<=x$utc_timestamp[i])
    cpprt<-y[1:max(dateind),5]
    capm<-sapply(1:length(un2[,1]),function(k,ind){
    capm<-sum(cpprt[which(ind[1:length(cpprt)]==k)])/sum(cpprt)
    },ind=ind)
    },x=tab_select1,y=wcapOn,un2=un2,c=c,ind=ind))

system.time(capm3<-lapply(1:length(tab_select1$utc_timestamp),function(i,x,y,un3,cc3,ind){
    print(i/length(tab_select1$utc_timestamp))
    dateind<-which(y$commissioning_date<=x$utc_timestamp[i])
    cpprt<-y[1:max(dateind),5]
    capm<-sapply(1:length(un3[,1]),function(k,ind){
    capm<-sum(cpprt[which(ind[1:length(cpprt)]==k)])/sum(cpprt)
    },ind=ind)
    },x=tab_select1,y=wcapOn,un3=un3,cc3=cc3,ind=ind))

dateind<-which(wcapOn$commissioning_date<=tab_select1$utc_timestamp[1])
cpprt<-wcapOn[1:max(dateind),5]
    capm<-sapply(1:length(un[,1]),function(k,ind){
    capm<-sum(cpprt[which(ind[1:length(cpprt)]==k)])/sum(cpprt)
    },ind=ind)
    

capmm<-matrix(unlist(capm),ncol=NROW(un),nrow=length(capm),byrow=TRUE)
capmxlm<-matrix(unlist(capmxl),ncol=NROW(un),nrow=length(capmxl),byrow=TRUE)
capmms<-matrix(unlist(capms),ncol=169,nrow=length(capm),byrow=TRUE)

capmmx<-matrix(unlist(capm2),ncol=NROW(un2),nrow=length(capm2),byrow=TRUE)
capmm3<-matrix(unlist(capm3),ncol=NROW(un3),nrow=length(capm3),byrow=TRUE)

capmm2<-sapply(1:NROW(capmm),function(i){
  print(i/NROW(capmm))
  minr<-min(capmm[i,])
  maxr<-max(capmm[i,])
  capmm2<-scale(capmm[i,], center = minr, scale = maxr - minr)
})

capmmx<-t(capmms)

minr<-apply(capmmx[,1:NCOL(capmmx)], 2, min)
maxr<-apply(capmmx[,1:NCOL(capmmx)], 2, max)

minr<-apply(capmmx3[,1:NCOL(capmmx3)], 2, min)
maxr<-apply(capmmx3[,1:NCOL(capmmx3)], 2, max)

capmmxscaled<-scale(capmmx, center = minr, scale = maxr - minr)
capmmxscaledt<-t(capmmxscaled)

# Climate data without subsetting

tsh<-lapply(1:length(u10m$grid$Lon),function(i,u2m,u10m,u50m,v2m,v10m,v50m){
   
   if(!is.numeric(u2m$getClosestTS(u2m$grid$Lon[i],u2m$grid$Lat[i]))){u2mw<-0} else{u2mw<-u2m$getClosestTS(u2m$grid$Lon[i],u2m$grid$Lat[i])}
   if(!is.numeric(u10m$getClosestTS(u10m$grid$Lon[i],u10m$grid$Lat[i]))){u10mw<-0} else{u10mw<-u10m$getClosestTS(u10m$grid$Lon[i],u10m$grid$Lat[i])}
   if(!is.numeric(u50m$getClosestTS(u50m$grid$Lon[i],u50m$grid$Lat[i]))){u50mw<-0} else{u50mw<-u50m$getClosestTS(u50m$grid$Lon[i],u50m$grid$Lat[i])}
   if(!is.numeric(v2m$getClosestTS(v2m$grid$Lon[i],v2m$grid$Lat[i]))){v2mw<-0} else{v2mw<-v2m$getClosestTS(v2m$grid$Lon[i],v2m$grid$Lat[i])}
   if(!is.numeric(v10m$getClosestTS(v10m$grid$Lon[i],v10m$grid$Lat[i]))){v10mw<-0} else{v10mw<-v10m$getClosestTS(v10m$grid$Lon[i],v10m$grid$Lat[i])}
   if(!is.numeric(v50m$getClosestTS(v50m$grid$Lon[i],v50m$grid$Lat[i]))){v50mw<-0} else{v50mw<-v50m$getClosestTS(v50m$grid$Lon[i],v50m$grid$Lat[i])}
   
    dt<-data.frame(u2mw,u10mw,u50mw,v2mw,v10mw,v50mw)
   
   },u2m=u2m,u10m=u10m,u50m=u50m,v2m=v2m,v10m=v10m,v50m=v50m)

tstm<-matrix(unlist(tsh),ncol=length(tsh)*6,byrow=FALSE)

########## test

tsh2<-lapply(1:length(un2[,1]), function(i,un2,u2m,u10m,u50m,v2m,v10m,v50m,t2m){
    
   if(!is.numeric(u2m$getClosestTS(un2[i,1],un2[i,2]))){u2mw<-0} else{u2mw<-u2m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(u10m$getClosestTS(un2[i,1],un2[i,2]))){u10mw<-0} else{u10mw<-u10m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(u50m$getClosestTS(un2[i,1],un2[i,2]))){u50mw<-0} else{u50mw<-u50m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(v2m$getClosestTS(un2[i,1],un2[i,2]))){v2mw<-0} else{v2mw<-v2m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(v10m$getClosestTS(un2[i,1],un2[i,2]))){v10mw<-0} else{v10mw<-v10m$getClosestTS(un2[i,1],un2[i,2])}
   if(!is.numeric(v50m$getClosestTS(un2[i,1],un2[i,2]))){v50mw<-0} else{v50mw<-v50m$getClosestTS(un2[i,1],un2[i,2])}
   #if(!is.numeric(disph$getClosestTS(un2[i,1],un2[i,2]))){disph<-0} else{disph<-disph$getClosestTS(un2[i,1],un2[i,2])}
   #if(!is.numeric(t2m$getClosestTS(un2[i,1],un2[i,2]))){t2mw<-0} else{t2mw<-t2m$getClosestTS(un2[i,1],un2[i,2])}
   #if(!is.numeric(t10m$getClosestTS(un2[i,1],un2[i,2]))){t10mw<-0} else{t10mw<-t10m$getClosestTS(un2[i,1],un2[i,2])}

    dt<-data.frame(u2mw,u10mw,u50mw,v2mw,v10mw,v50mw)
   

   #names(dt)<-c("u2w","v2w","u10w","v10w","u50w","v50w","ds")
   },un2=un2,u2m=u2m,u10m=u10m,u50m=u50m,v2m=v2m,v10m=v10m,v50m=v50m)

tstm2<-matrix(unlist(tsh2),ncol=length(tsh2)*6,byrow=FALSE)

tsh3<-lapply(1:length(un3[,1]), function(i,un3,u2m,u10m,u50m,v2m,v10m,v50m,t2m){
    
   if(!is.numeric(u2m$getClosestTS(un3[i,1],un3[i,2]))){u2mw<-0} else{u2mw<-u2m$getClosestTS(un3[i,1],un3[i,2])}
   if(!is.numeric(u10m$getClosestTS(un3[i,1],un3[i,2]))){u10mw<-0} else{u10mw<-u10m$getClosestTS(un3[i,1],un3[i,2])}
   if(!is.numeric(u50m$getClosestTS(un3[i,1],un3[i,2]))){u50mw<-0} else{u50mw<-u50m$getClosestTS(un3[i,1],un3[i,2])}
   if(!is.numeric(v2m$getClosestTS(un3[i,1],un3[i,2]))){v2mw<-0} else{v2mw<-v2m$getClosestTS(un3[i,1],un3[i,2])}
   if(!is.numeric(v10m$getClosestTS(un3[i,1],un3[i,2]))){v10mw<-0} else{v10mw<-v10m$getClosestTS(un3[i,1],un3[i,2])}
   if(!is.numeric(v50m$getClosestTS(un3[i,1],un3[i,2]))){v50mw<-0} else{v50mw<-v50m$getClosestTS(un3[i,1],un3[i,2])}
   #if(!is.numeric(disph$getClosestTS(un3[i,1],un3[i,2]))){disph<-0} else{disph<-disph$getClosestTS(un3[i,1],un3[i,2])}
   #if(!is.numeric(t2m$getClosestTS(un3[i,1],un3[i,2]))){t2mw<-0} else{t2mw<-t2m$getClosestTS(un3[i,1],un3[i,2])}
   #if(!is.numeric(t10m$getClosestTS(un3[i,1],un3[i,2]))){t10mw<-0} else{t10mw<-t10m$getClosestTS(un3[i,1],un3[i,2])}

    dt<-data.frame(u2mw,u10mw,u50mw,v2mw,v10mw,v50mw)
   

   #names(dt)<-c("u2w","v2w","u10w","v10w","u50w","v50w","ds")
   },un3=un3,u2m=u2m,u10m=u10m,u50m=u50m,v2m=v2m,v10m=v10m,v50m=v50m)

tstm3<-matrix(unlist(tsh3),ncol=length(tsh3)*6,byrow=FALSE)

tsh4<-lapply(1:length(redlocs[,1]), function(i,redlocs,u2m,u10m,u50m,v2m,v10m,v50m,t2m){
    
   if(!is.numeric(u2m$getClosestTS(redlocs[i,1],redlocs[i,2]))){u2mw<-0} else{u2mw<-u2m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   if(!is.numeric(u10m$getClosestTS(redlocs[i,1],redlocs[i,2]))){u10mw<-0} else{u10mw<-u10m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   if(!is.numeric(u50m$getClosestTS(redlocs[i,1],redlocs[i,2]))){u50mw<-0} else{u50mw<-u50m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   if(!is.numeric(v2m$getClosestTS(redlocs[i,1],redlocs[i,2]))){v2mw<-0} else{v2mw<-v2m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   if(!is.numeric(v10m$getClosestTS(redlocs[i,1],redlocs[i,2]))){v10mw<-0} else{v10mw<-v10m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   if(!is.numeric(v50m$getClosestTS(redlocs[i,1],redlocs[i,2]))){v50mw<-0} else{v50mw<-v50m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   #if(!is.numeric(disph$getClosestTS(redlocs[i,1],redlocs[i,2]))){disph<-0} else{disph<-disph$getClosestTS(redlocs[i,1],redlocs[i,2])}
   #if(!is.numeric(t2m$getClosestTS(redlocs[i,1],redlocs[i,2]))){t2mw<-0} else{t2mw<-t2m$getClosestTS(redlocs[i,1],redlocs[i,2])}
   #if(!is.numeric(t10m$getClosestTS(redlocs[i,1],redlocs[i,2]))){t10mw<-0} else{t10mw<-t10m$getClosestTS(redlocs[i,1],redlocs[i,2])}

    dt<-data.frame(u2mw,u10mw,u50mw,v2mw,v10mw,v50mw)
   

   #names(dt)<-c("u2w","v2w","u10w","v10w","u50w","v50w","ds")
   },redlocs=redlocs,u2m=u2m,u10m=u10m,u50m=u50m,v2m=v2m,v10m=v10m,v50m=v50m)

tstm4<-matrix(unlist(tsh4),ncol=length(tsh4)*6,byrow=FALSE)

##########


tab_<-read_delim("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/time_series_60min_singleindex_new.csv",delim=",")
tab_<-read_delim("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/time_series_60min_singleindex(1).csv",delim=",")
tab_select1<-tab_ %>%
  mutate(hour=hour(utc_timestamp),day=wday(utc_timestamp),month=month(utc_timestamp)) %>%
  dplyr::select(utc_timestamp,DE_wind_generation_actual,
                hour,day,month) %>%
  dplyr::filter(between(year(utc_timestamp),2010,2016)) 
tab_select1[is.na(tab_select1)]<-0

tab_select<-as_tibble(data.frame(tab_select1,tstm))
tab_select<-as_tibble(data.frame(tab_select1,tstm2))
tab_select<-as_tibble(data.frame(tab_select1,tstm3))
tab_select<-as_tibble(data.frame(tab_select1,tstm4))

names(tab_select)[1:5]<-c("date","tab_select.DE_wind_generation","hour","day","month")

tab_select<-as_tibble(data.frame(tab_select1,tstm,wdirm2))

names(tab_select)[1:5]<-c("date","tab_select.DE_wind_generation","hour","day","month")

rrcap<-read_delim("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/renewable_capacity_timeseries.csv",delim=",")
rrcap<-rrcap %>% 
  dplyr::select(day,DE_wind_capacity)%>%
  #dplyr::mutate(day=day,cap=as.numeric(DE_wind_capacity))%>%
  dplyr::filter(between(year(day),2010,2016))
rrcaps<-as.numeric(rrcap$DE_wind_capacity)

tab_select$tab_select.DE_wind_generation<-as.numeric(tab_select$tab_select.DE_wind_generation)/rep(rrcaps,each=24)

library(dummies)

hourss<-dummies::dummy(tab_select$hour)
dayss<-dummies::dummy(tab_select$day)
monthss<-dummies::dummy(tab_select$month)

datasf<-data.frame(tab_select$date,tab_select$tab_select.DE_wind_generation,hourss,dayss,monthss,tab_select[,6:NCOL(tab_select)])
names(datasf)[1:2]<-c("date","tab_select.DE_wind_generation")
#Helper function for Model Training and Prediction
mlpts<-function(data,tss,yrs,nets,loc){
  
library(lubridate)
library(tibble)  
library(tidyverse)  
library(caret)

library(parallel)
library(doParallel)  

maxs <- apply(datasf[,2:length(datasf)], 2, max)
mins <- apply(datasf[,2:length(datasf)], 2, min)

scaled <- as_tibble(scale(datasf[,2:length(datasf)], center = mins, scale = maxs - mins))
scaled[,which(apply(scaled,2, anyNA)==TRUE)]<-0  
  
ys<-unique(year(tss))

ys2<-split(ys,as.numeric(gl(length(ys),2,length(ys))))

'%ni%' <- Negate('%in%')  
 
if(length(ys2)>1){
  
  splits <- sapply(1:length(ys2),function(i){
    index <- which(year(datasf$date)%ni%ys2[[i]])
  })
  
}else{
 #index <- which(year(datasf[,1])%ni%yseq) 
 #splits<-list(index)
 }
  
mlptsd <- sapply(1:length(splits),function(i,splits){
  
  print(i/length(splits))
  
  index<-splits[[i]]

train <- datasf[index,]
test <- datasf[-index,]

set.seed(11)
index2 <- sample(index, size = nrow(train))

if(loc==TRUE){
train_ <- data.frame(scaled[index2,],capmmxscaledt[index2,])
test_ <- data.frame(scaled[-index2,],capmmxscaledt[-index2,])
}else{
train_ <- data.frame(scaled[index2,])
test_ <- data.frame(scaled[-index2,])
}

cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)

set.seed(11)
mlp_grid = expand.grid(layer1 = nets,
                       layer2 = nets,
                       layer3 = nets)

t3<<-caret::train(tab_select.DE_wind_generation ~ ., data=train_, method = "mlpML", preProcess = NULL,resample = NULL, allowParallel = TRUE,trControl = trainControl(number=1, verboseIter = TRUE, allowParallel = TRUE), tuneGrid = mlp_grid)

stopCluster(cluster)
registerDoSEQ()

t3pred<-pr.mlpcar<-predict(t3,test_[,2:NCOL(train_)])

pr.mlpcar_<- pr.mlpcar*(max(datasf$tab_select.DE_wind_generation)-min(datasf$tab_select.DE_wind_generation))+min(datasf$tab_select.DE_wind_generation)
  
return(pr.mlpcar_)
},splits=splits)


}

# Model training and Timeseries generation
# tss = Dates for whole timeseries
# yrs = number of years for prediction
# nets = node size per hidden network layer

tss<-seq(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2016-12-31 23:00:00"), by="hour")
mlptsd<-mlpts(data=datasf,tss=tss,yrs=2,nets=60,loc=FALSE)
############## Grid point subsetting 2  ##########################

mod_in <- readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/modelmlm2x206.rds")

inps<-mod_in$finalModel$snnsObject$getAllInputUnits()
hids<-mod_in$finalModel$snnsObject$getAllUnitsTType("UNIT_HIDDEN")

inpweights<-mod_in$finalModel$snnsObject$getWeightMatrix(inps,hids[1:as.numeric(mod_in$results[1])])

NROW(inpweights)

inpweightsw<-inpweights[44:NROW(inpweights),]
inpweightsu2<-inpweightsw[seq(1,1236,6),]

vars<-u2m,u10,u50m,v2m,v10m,v50m

inpw<-lapply(1:6,function(i){
  seqqs<<-seq(i,1236,6)
  inpw<-inpweightsw[seq(i,1236,6),]
})

subss<-as.numeric(which(rowSums(abs(inpweights))>summary(rowSums(abs(inpweights))[46:NROW(inpweights)])[5]))

names(datasf)[subss[which(subss>45)]]

names(un3)<-c("lon","lat")

locs<-lapply(1:NROW(un3),function(i){
  vars<-c("u2m","u10m","u50m","v2m","v10m","v50m")
  loc<-rep(un3[i,],each=6)
locs<-data.frame(vars,loc$lon,loc$lat)
names(locs)<-c("var","lon","lat")
return(locs)
})

library(plyr)

locations<-ldply(locs,data.frame)

locw<-data.frame(locations,rowSums(abs(inpweightsw)))

locw<-data.frame(locations,rowMeans(inpweightsw))
names(locw)[4]<-"absweight"

library(rgdal)
germany <- readOGR(dsn = 'data/shape_files', layer = "DEU_adm1")

DEbord<-fortify(DE_border)

locw2<-data.frame(locw,rep(seq(1,206,1),6))
names(locw2)[5]<-"id"

locw3<-locw2 %>% 
  group_by(id) %>% 
  summarise(Weights = sum(absweight))

ggplot() +
 geom_polygon(data = DEbord, aes(x=long, y=lat, group=group), fill = "transparent", color = "gray10", size = 1)+
 geom_point(data=locw3,aes(x=un3$lon,y=un3$lat,size = Weights))+
  #geom_density2d(data=locw3,aes(x=un3$lon,y=un3$lat,fill=Weights))+
  #stat_density_2d(geom = "polygon", aes(alpha = ..Weights..))+
  geom_raster(data=locw3,aes(x=un3$lon,y=un3$lat,fill = Weights), hjust=0.5,vjust=0.5, interpolate=TRUE)

ggplot() +
 geom_raster(data=locw[which(locw$var=="u50m"),],aes(x=lon,y=lat,fill = absweight), hjust=0.5,vjust=0.5, interpolate=FALSE)+
  geom_polygon(data = DEbord, aes(x=long, y=lat, group=group), fill = "transparent", color = "gray10", size = 1)+
 geom_point(data=locw,aes(x=lon,y=lat))+
  theme_classic(base_size = 30)
  #geom_density2d(data=locw3,aes(x=un3$lon,y=un3$lat,fill=Weights))+
  #stat_density_2d(geom = "polygon", aes(alpha = ..Weights..))+
  

geom_raster(aes(fill = Weights), hjust=0.5,
vjust=0.5, interpolate=FALSE)

  ggplot(locw, aes(x = lon, y = lat)) +
  geom_polygon(data = DEbord, aes(x=long, y=lat, group=group), fill = "transparent", color = "gray10", size = 1)+
  #stat_contour(geom="polygon",aes(fill=stat(level)))
  
  ggplot(locw, aes(x = lon, y = lat, z= absweight)) +
  geom_density2d(locw,aes(fill=..absweight..))#+stat_contour(geom="polygon",aes(fill=stat(absweight))) 
  
    geom_smooth(aes(size = absweight,colour = var))
  #geom_point(aes(size = absweight,colour = var))+
  #facet_grid(var~.)


locx<-locations[subss[which(subss>45)]-45,]
locx<-locations[subss,]

paste("U2m")

library(plotly)

plot_ly(z=abs(inpweightsu2),colors="Greys")
plot_ly(z=abs(inpweightsw)[1:45,],colors="Greys")
plot_ly(z=abs(inpweightsw)[46:48,],colors="Greys")

plot(rowSums(abs(inpweightsu2)))
plot(rowSums(abs(inpweights))[1:45])
plot(rowSums(abs(inpweights))[46:NROW(inpweights)])
plot(rowSums(abs(inpweights))[46:60])

capdataf<-data.frame(capmm)
plot_ly(z=capdataf)


implocs<-lapply(1:NROW(capmxlm),function(i){
  imploc<-which(capmxlm[i,]>summary(capmxlm[i,])[5])
})

redlocs<-un[unique(unlist(implocs)),]

############## Visualization ##########################

library(sp)
library(raster)

plot(expand.grid(u10m$lons,u10m$lats),type="n")
DE_border <- getData('GADM',country= "DE", level=0)
plot(DE_border,add=TRUE,col="orange")
points(wcapOn$lon,wcapOn$lat,col="salmon",lwd=2)
points(locx$lon,locx$lat,col="red",lwd=2)
points(un[,1],un[,2],col="orange",lwd=2)
points(redlocs,col="black",lwd=2)
points(c3[[3]],col="green",lwd=4)

sapply(1:length(wcapOn$lon),function(i){
plot(expand.grid(u10m$lons,u10m$lats))
points(wcapOn$lon[i],wcapOn$lat[i],col="salmon",lwd=4)
points(c3[[i]],col="green",lwd=4)
readline(prompt="Press [enter] to continue")  
})


points(expand.grid(u10m$lons,u10m$lats),col="black")

points(un2,col="red",lwd=2)
points(un3,col="black",lwd=2)
points(un,col="green",lwd=2)

cor(datasf$tab_select.DE_wind_generation,unlist(mlptsd))
(sqrt(mean((unlist(mlptsd)-datasf$tab_select.DE_wind_generation)^2))/mean(datasf$tab_select.DE_wind_generation))
(sqrt(mean((compnat$DE-datasf$tab_select.DE_wind_generation)^2))/mean(datasf$tab_select.DE_wind_generation))

cor(datasf$tab_select.DE_wind_generation,compnat$DE)
(mean(abs(unlist(mlptsd)-datasf$tab_select.DE_wind_generation))/mean(datasf$tab_select.DE_wind_generation))
(mean(abs(compnat$DE-datasf$tab_select.DE_wind_generation))/mean(datasf$tab_select.DE_wind_generation))


mlptsdx<-read_rds("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/mlptsd6x2_80.rds")

cor(datasf$tab_select.DE_wind_generation,unlist(mlptsdx))
(sqrt(mean((unlist(mlptsdx)-datasf$tab_select.DE_wind_generation)^2))/mean(datasf$tab_select.DE_wind_generation))
(sqrt(mean((compnat$DE-datasf$tab_select.DE_wind_generation)^2))/mean(datasf$tab_select.DE_wind_generation))

cor(datasf$tab_select.DE_wind_generation,compnat$DE)
(mean(abs(unlist(mlptsdx)-datasf$tab_select.DE_wind_generation))/mean(datasf$tab_select.DE_wind_generation))
(mean(abs(compnat$DE-datasf$tab_select.DE_wind_generation))/mean(datasf$tab_select.DE_wind_generation))
