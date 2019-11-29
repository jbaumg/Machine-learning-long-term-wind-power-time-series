
library(RCurl)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(speedglm)
library(tibble)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(openxlsx)
library(e1071)

setwd("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/figures3")


#setwd("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/mlm5_80")
#### rn
compnat<-readRDS("compnat.rds")

obseff<-readRDS("datasfmlm2final2.rds")$tab_select.DE_wind_generation

obs<-as_tibble(data.frame(date<-compnat$time,effekt<-obseff))
names(obs)<-c("date","effekt")

mod<-as_tibble(data.frame(date<-compnat$time,effekt<-compnat$DE))
names(mod)<-c("date","effekt")

###mlm1
#mlptsd6<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/mlptsd6_80.rds")

#mlptsd6<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/mlptsd680_2.rds")

obs2<-as_tibble(data.frame(date<-compnat$time,effekt<-obseff))
names(obs2)<-c("date","effekt")

mlm1ts<-unlist(readRDS("mlm1ts_60.rds"))
mod2<-as_tibble(data.frame(date<-compnat$time,effekt<-mlm1ts))
names(mod2)<-c("date","effekt")

#mod2<-as_tibble(data.frame(date<-compnat$time,effekt<-mlptsd6))
#names(mod2)<-c("date","effekt")

###mlm2
obs3<-as_tibble(data.frame(date<-compnat$time,effekt<-obseff))
names(obs3)<-c("date","effekt")

#mod3<-as_tibble(data.frame(date<-compnat$time,effekt<-mlptsd5))
#names(mod3)<-c("date","effekt")
mlm2ts<-unlist(readRDS("mlm2x206ts.rds"))
mod3<-as_tibble(data.frame(date<-compnat$time,effekt<-mlm2ts))
names(mod3)<-c("date","effekt")

#mod3<-as_tibble(data.frame(date<-compnat$time,effekt<-unlist(mlptsd)))
#names(mod3)<-c("date","effekt")
####

unit<-"CF"

cor(mod$effekt,obs$effekt)
cor(mod2$effekt,obs$effekt)
cor(mod3$effekt,obs$effekt)

sqrt(mean((mod$effekt-obs$effekt)^2))/mean(obs$effekt)
sqrt(mean((mod2$effekt-obs$effekt)^2))/mean(obs$effekt)
sqrt(mean((mod3$effekt-obs$effekt)^2))/mean(obs$effekt)

mean(abs(mod$effekt-obs$effekt))/mean(obs$effekt)
mean(abs(mod2$effekt-obs$effekt))/mean(obs$effekt)
mean(abs(mod3$effekt-obs$effekt))/mean(obs$effekt)

var(obs$effekt)
var(mod$effekt)
var(mod2$effekt)
var(mod3$effekt)

summary(obs$effekt)
summary(mod$effekt)
summary(mod2$effekt)
summary(mod3$effekt)

#### diurnal deviations

modho<-data.frame(obs$date,mod$effekt)
names(modho)<-c("date", "effekt")
modho<-as_tibble(modho)

modho24<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==00),])
modho1<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==01),])
modho2<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==02),])
modho3<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==03),])
modho4<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==04),])
modho5<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==05),])
modho6<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==06),])
modho7<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==07),])
modho8<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==08),])
modho9<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==09),])
modho10<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==10),])
modho11<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==11),])
modho12<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==12),])
modho13<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==13),])
modho14<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==14),])
modho15<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==15),])
modho16<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==16),])
modho17<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==17),])
modho18<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==18),])
modho19<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==19),])
modho20<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==20),])
modho21<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==21),])
modho22<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==22),])
modho23<-as_tibble(modho[which(as.numeric(format(modho$date,"%H"))==23),])

obsh24<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==00),])
obsh1<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==01),])
obsh2<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==02),])
obsh3<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==03),])
obsh4<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==04),])
obsh5<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==05),])
obsh6<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==06),])
obsh7<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==07),])
obsh8<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==08),])
obsh9<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==09),])
obsh10<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==10),])
obsh11<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==11),])
obsh12<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==12),])
obsh13<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==13),])
obsh14<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==14),])
obsh15<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==15),])
obsh16<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==16),])
obsh17<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==17),])
obsh18<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==18),])
obsh19<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==19),])
obsh20<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==20),])
obsh21<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==21),])
obsh22<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==22),])
obsh23<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==23),])

#error
herr24<-data.frame(modho24$effekt-obsh24$effekt,rep("24",length(modho24$effekt)))
names(herr24)<-c("deviation","hour")
herr1<-data.frame(modho1$effekt-obsh1$effekt,rep("01",length(modho1$effekt)))
names(herr1)<-c("deviation","hour")
herr2<-data.frame(modho2$effekt-obsh2$effekt,rep("02",length(modho2$effekt)))
names(herr2)<-c("deviation","hour")
herr3<-data.frame(modho3$effekt-obsh3$effekt,rep("03",length(modho3$effekt)))
names(herr3)<-c("deviation","hour")
herr4<-data.frame(modho4$effekt-obsh4$effekt,rep("04",length(modho4$effekt)))
names(herr4)<-c("deviation","hour")
herr5<-data.frame(modho5$effekt-obsh5$effekt,rep("05",length(modho5$effekt)))
names(herr5)<-c("deviation","hour")
herr6<-data.frame(modho6$effekt-obsh6$effekt,rep("06",length(modho6$effekt)))
names(herr6)<-c("deviation","hour")
herr7<-data.frame(modho7$effekt-obsh7$effekt,rep("07",length(modho7$effekt)))
names(herr7)<-c("deviation","hour")
herr8<-data.frame(modho8$effekt-obsh8$effekt,rep("08",length(modho8$effekt)))
names(herr8)<-c("deviation","hour")
herr9<-data.frame(modho9$effekt-obsh9$effekt,rep("09",length(modho9$effekt)))
names(herr9)<-c("deviation","hour")
herr10<-data.frame(modho10$effekt-obsh10$effekt,rep("10",length(modho10$effekt)))
names(herr10)<-c("deviation","hour")
herr11<-data.frame(modho11$effekt-obsh11$effekt,rep("11",length(modho11$effekt)))
names(herr11)<-c("deviation","hour")
herr12<-data.frame(modho12$effekt-obsh12$effekt,rep("12",length(modho12$effekt)))
names(herr12)<-c("deviation","hour")
herr13<-data.frame(modho13$effekt-obsh13$effekt,rep("13",length(modho13$effekt)))
names(herr13)<-c("deviation","hour")
herr14<-data.frame(modho14$effekt-obsh14$effekt,rep("14",length(modho14$effekt)))
names(herr14)<-c("deviation","hour")
herr15<-data.frame(modho15$effekt-obsh15$effekt,rep("15",length(modho15$effekt)))
names(herr15)<-c("deviation","hour")
herr16<-data.frame(modho16$effekt-obsh16$effekt,rep("16",length(modho16$effekt)))
names(herr16)<-c("deviation","hour")
herr17<-data.frame(modho17$effekt-obsh17$effekt,rep("17",length(modho17$effekt)))
names(herr17)<-c("deviation","hour")
herr18<-data.frame(modho18$effekt-obsh18$effekt,rep("18",length(modho18$effekt)))
names(herr18)<-c("deviation","hour")
herr19<-data.frame(modho19$effekt-obsh19$effekt,rep("19",length(modho19$effekt)))
names(herr19)<-c("deviation","hour")
herr20<-data.frame(modho20$effekt-obsh20$effekt,rep("20",length(modho20$effekt)))
names(herr20)<-c("deviation","hour")
herr21<-data.frame(modho21$effekt-obsh21$effekt,rep("21",length(modho21$effekt)))
names(herr21)<-c("deviation","hour")
herr22<-data.frame(modho22$effekt-obsh22$effekt,rep("22",length(modho22$effekt)))
names(herr22)<-c("deviation","hour")
herr23<-data.frame(modho23$effekt-obsh23$effekt,rep("23",length(modho23$effekt)))
names(herr23)<-c("deviation","hour")

errdt<-as_tibble(rbind(herr1, herr2, herr3,herr4, herr5, herr6,herr7, herr8, herr9,herr10, herr11, herr12,herr13, herr14, herr15,herr16, herr17, herr18,herr19, herr20, herr21,herr22, herr23, herr24))


modho2<-data.frame(obs2$date,mod2$effekt)
names(modho2)<-c("date", "effekt")
modho2<-as_tibble(modho2)

modho224<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==00),])
modho21<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==01),])
modho22<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==02),])
modho23<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==03),])
modho24<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==04),])
modho25<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==05),])
modho26<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==06),])
modho27<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==07),])
modho28<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==08),])
modho29<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==09),])
modho210<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==10),])
modho211<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==11),])
modho212<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==12),])
modho213<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==13),])
modho214<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==14),])
modho215<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==15),])
modho216<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==16),])
modho217<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==17),])
modho218<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==18),])
modho219<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==19),])
modho220<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==20),])
modho221<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==21),])
modho222<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==22),])
modho223<-as_tibble(modho2[which(as.numeric(format(modho2$date,"%H"))==23),])

obsh24<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==00),])
obsh1<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==01),])
obsh2<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==02),])
obsh3<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==03),])
obsh4<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==04),])
obsh5<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==05),])
obsh6<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==06),])
obsh7<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==07),])
obsh8<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==08),])
obsh9<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==09),])
obsh10<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==10),])
obsh11<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==11),])
obsh12<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==12),])
obsh13<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==13),])
obsh14<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==14),])
obsh15<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==15),])
obsh16<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==16),])
obsh17<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==17),])
obsh18<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==18),])
obsh19<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==19),])
obsh20<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==20),])
obsh21<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==21),])
obsh22<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==22),])
obsh23<-as_tibble(obs[which(as.numeric(format(obs$date,"%H"))==23),])


#error
herr224<-data.frame(modho224$effekt-obsh24$effekt,rep("24",length(modho24$effekt)))
names(herr224)<-c("deviation","hour")
herr21<-data.frame(modho21$effekt-obsh1$effekt,rep("01",length(modho21$effekt)))
names(herr21)<-c("deviation","hour")
herr22<-data.frame(modho22$effekt-obsh2$effekt,rep("02",length(modho22$effekt)))
names(herr22)<-c("deviation","hour")
herr23<-data.frame(modho23$effekt-obsh3$effekt,rep("03",length(modho23$effekt)))
names(herr23)<-c("deviation","hour")
herr24<-data.frame(modho24$effekt-obsh4$effekt,rep("04",length(modho24$effekt)))
names(herr24)<-c("deviation","hour")
herr25<-data.frame(modho25$effekt-obsh5$effekt,rep("05",length(modho25$effekt)))
names(herr25)<-c("deviation","hour")
herr26<-data.frame(modho26$effekt-obsh6$effekt,rep("06",length(modho26$effekt)))
names(herr26)<-c("deviation","hour")
herr27<-data.frame(modho27$effekt-obsh7$effekt,rep("07",length(modho27$effekt)))
names(herr27)<-c("deviation","hour")
herr28<-data.frame(modho28$effekt-obsh8$effekt,rep("08",length(modho28$effekt)))
names(herr28)<-c("deviation","hour")
herr29<-data.frame(modho29$effekt-obsh9$effekt,rep("09",length(modho29$effekt)))
names(herr29)<-c("deviation","hour")
herr210<-data.frame(modho210$effekt-obsh10$effekt,rep("10",length(modho210$effekt)))
names(herr210)<-c("deviation","hour")
herr211<-data.frame(modho211$effekt-obsh11$effekt,rep("11",length(modho211$effekt)))
names(herr211)<-c("deviation","hour")
herr212<-data.frame(modho212$effekt-obsh12$effekt,rep("12",length(modho212$effekt)))
names(herr212)<-c("deviation","hour")
herr213<-data.frame(modho213$effekt-obsh13$effekt,rep("13",length(modho213$effekt)))
names(herr213)<-c("deviation","hour")
herr214<-data.frame(modho214$effekt-obsh14$effekt,rep("14",length(modho214$effekt)))
names(herr214)<-c("deviation","hour")
herr215<-data.frame(modho215$effekt-obsh15$effekt,rep("15",length(modho215$effekt)))
names(herr215)<-c("deviation","hour")
herr216<-data.frame(modho216$effekt-obsh16$effekt,rep("16",length(modho216$effekt)))
names(herr216)<-c("deviation","hour")
herr217<-data.frame(modho217$effekt-obsh17$effekt,rep("17",length(modho217$effekt)))
names(herr217)<-c("deviation","hour")
herr218<-data.frame(modho218$effekt-obsh18$effekt,rep("18",length(modho218$effekt)))
names(herr218)<-c("deviation","hour")
herr219<-data.frame(modho219$effekt-obsh19$effekt,rep("19",length(modho219$effekt)))
names(herr219)<-c("deviation","hour")
herr220<-data.frame(modho220$effekt-obsh20$effekt,rep("20",length(modho220$effekt)))
names(herr220)<-c("deviation","hour")
herr221<-data.frame(modho221$effekt-obsh21$effekt,rep("21",length(modho221$effekt)))
names(herr221)<-c("deviation","hour")
herr222<-data.frame(modho222$effekt-obsh22$effekt,rep("22",length(modho222$effekt)))
names(herr222)<-c("deviation","hour")
herr223<-data.frame(modho223$effekt-obsh23$effekt,rep("23",length(modho223$effekt)))
names(herr223)<-c("deviation","hour")

errdt2<-as_tibble(rbind(herr21, herr22, herr23,herr24, herr25, herr26,herr27, herr28, herr29,herr210, herr211, herr212,herr213, herr214, herr215,herr216, herr217, herr218,herr219, herr220, herr221,herr222, herr223, herr224))

#mlptsd5<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/mlptsd5_80.rds")


modho3<-data.frame(obs3$date,mod3$effekt)
names(modho3)<-c("date", "effekt")
modho3<-as_tibble(modho3)

modho324<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==00),])
modho31<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==01),])
modho32<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==02),])
modho33<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==03),])
modho34<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==04),])
modho35<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==05),])
modho36<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==06),])
modho37<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==07),])
modho38<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==08),])
modho39<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==09),])
modho310<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==10),])
modho311<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==11),])
modho312<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==12),])
modho313<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==13),])
modho314<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==14),])
modho315<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==15),])
modho316<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==16),])
modho317<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==17),])
modho318<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==18),])
modho319<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==19),])
modho320<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==20),])
modho321<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==21),])
modho322<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==22),])
modho323<-as_tibble(modho3[which(as.numeric(format(modho3$date,"%H"))==23),])


#error
herr324<-data.frame(modho324$effekt-obsh24$effekt,rep("24",length(modho324$effekt)))
names(herr324)<-c("deviation","hour")
herr31<-data.frame(modho31$effekt-obsh1$effekt,rep("01",length(modho31$effekt)))
names(herr31)<-c("deviation","hour")
herr32<-data.frame(modho32$effekt-obsh2$effekt,rep("02",length(modho32$effekt)))
names(herr32)<-c("deviation","hour")
herr33<-data.frame(modho33$effekt-obsh3$effekt,rep("03",length(modho33$effekt)))
names(herr33)<-c("deviation","hour")
herr34<-data.frame(modho34$effekt-obsh4$effekt,rep("04",length(modho34$effekt)))
names(herr34)<-c("deviation","hour")
herr35<-data.frame(modho35$effekt-obsh5$effekt,rep("05",length(modho35$effekt)))
names(herr35)<-c("deviation","hour")
herr36<-data.frame(modho36$effekt-obsh6$effekt,rep("06",length(modho36$effekt)))
names(herr36)<-c("deviation","hour")
herr37<-data.frame(modho37$effekt-obsh7$effekt,rep("07",length(modho37$effekt)))
names(herr37)<-c("deviation","hour")
herr38<-data.frame(modho38$effekt-obsh8$effekt,rep("08",length(modho38$effekt)))
names(herr38)<-c("deviation","hour")
herr39<-data.frame(modho39$effekt-obsh9$effekt,rep("09",length(modho39$effekt)))
names(herr39)<-c("deviation","hour")
herr310<-data.frame(modho310$effekt-obsh10$effekt,rep("10",length(modho310$effekt)))
names(herr310)<-c("deviation","hour")
herr311<-data.frame(modho311$effekt-obsh11$effekt,rep("11",length(modho311$effekt)))
names(herr311)<-c("deviation","hour")
herr312<-data.frame(modho312$effekt-obsh12$effekt,rep("12",length(modho312$effekt)))
names(herr312)<-c("deviation","hour")
herr313<-data.frame(modho313$effekt-obsh13$effekt,rep("13",length(modho313$effekt)))
names(herr313)<-c("deviation","hour")
herr314<-data.frame(modho314$effekt-obsh14$effekt,rep("14",length(modho314$effekt)))
names(herr314)<-c("deviation","hour")
herr315<-data.frame(modho315$effekt-obsh15$effekt,rep("15",length(modho315$effekt)))
names(herr315)<-c("deviation","hour")
herr316<-data.frame(modho316$effekt-obsh16$effekt,rep("16",length(modho316$effekt)))
names(herr316)<-c("deviation","hour")
herr317<-data.frame(modho317$effekt-obsh17$effekt,rep("17",length(modho317$effekt)))
names(herr317)<-c("deviation","hour")
herr318<-data.frame(modho318$effekt-obsh18$effekt,rep("18",length(modho318$effekt)))
names(herr318)<-c("deviation","hour")
herr319<-data.frame(modho319$effekt-obsh19$effekt,rep("19",length(modho319$effekt)))
names(herr319)<-c("deviation","hour")
herr320<-data.frame(modho320$effekt-obsh20$effekt,rep("20",length(modho320$effekt)))
names(herr320)<-c("deviation","hour")
herr321<-data.frame(modho321$effekt-obsh21$effekt,rep("21",length(modho321$effekt)))
names(herr321)<-c("deviation","hour")
herr322<-data.frame(modho322$effekt-obsh22$effekt,rep("22",length(modho322$effekt)))
names(herr322)<-c("deviation","hour")
herr323<-data.frame(modho323$effekt-obsh23$effekt,rep("23",length(modho323$effekt)))
names(herr323)<-c("deviation","hour")

errdt3<-as_tibble(rbind(herr31, herr32, herr33,herr34, herr35, herr36,herr37, herr38, herr39,herr310, herr311, herr312,herr313, herr314, herr315,herr316, herr317, herr318,herr319, herr320, herr321,herr322, herr323, herr324))

# mean

errdtfr<-as_tibble(data.frame(unique(errdt$hour),with(errdt, tapply(deviation, hour, mean)),with(errdt, tapply(deviation, hour, min)),with(errdt, tapply(deviation, hour, max)),rep("RN",24)))
names(errdtfr)<-c("hour","mean","min","max", "type")

errdtfr2<-as_tibble(data.frame(unique(errdt2$hour),with(errdt2, tapply(deviation, hour, mean)),with(errdt2, tapply(deviation, hour, min)),with(errdt2, tapply(deviation, hour, max)),rep("MLM1",24)))
names(errdtfr2)<-c("hour","mean","min","max", "type")

errdtfr3<-as_tibble(data.frame(unique(errdt3$hour),with(errdt3, tapply(deviation, hour, mean)),with(errdt3, tapply(deviation, hour, min)),with(errdt3, tapply(deviation, hour, max)),rep("MLM2",24)))
names(errdtfr3)<-c("hour","mean","min","max", "type")

errdtfr<-as_tibble(data.frame(unique(errdt$hour),with(errdt, tapply(deviation, hour, mean)),with(errdt, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("RN",24)))
names(errdtfr)<-c("hour","mean","range", "type")

errdtfr2<-as_tibble(data.frame(unique(errdt2$hour),with(errdt2, tapply(deviation, hour, mean)),with(errdt2, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM1",24)))
names(errdtfr2)<-c("hour","mean","range", "type")

errdtfr3<-as_tibble(data.frame(unique(errdt3$hour),with(errdt3, tapply(deviation, hour, mean)),with(errdt3, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM2",24)))
names(errdtfr3)<-c("hour","mean","range", "type")

# abs. mean

#errdtfr<-as_tibble(data.frame(unique(errdt$hour),with(errdt, tapply(deviation, hour, function(x){mean(abs(x))})),with(errdt, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("RN",24)))
#names(errdtfr)<-c("hour","mean","range", "type")

#errdtfr2<-as_tibble(data.frame(unique(errdt2$hour),with(errdt2, tapply(deviation, hour, function(x){mean(abs(x))})),with(errdt2, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM1",24)))
#names(errdtfr2)<-c("hour","mean","range", "type")

#errdtfr3<-as_tibble(data.frame(unique(errdt3$hour),with(errdt3, tapply(deviation, hour, function(x){mean(abs(x))})),with(errdt3, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM2",24)))
#names(errdtfr3)<-c("hour","mean","range", "type")

errdtfr<-as_tibble(data.frame(unique(errdt$hour),with(errdt, tapply(deviation, hour, function(x){median(abs(x))})),with(errdt, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("RN",24)))
names(errdtfr)<-c("hour","median","range", "type")

errdtfr2<-as_tibble(data.frame(unique(errdt2$hour),with(errdt2, tapply(deviation, hour, function(x){median(abs(x))})),with(errdt2, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM1",24)))
names(errdtfr2)<-c("hour","median","range", "type")

errdtfr3<-as_tibble(data.frame(unique(errdt3$hour),with(errdt3, tapply(deviation, hour, function(x){median(abs(x))})),with(errdt3, tapply(deviation, hour, function(x){max(x)-min(x)})),rep("MLM2",24)))
names(errdtfr3)<-c("hour","median","range", "type")

errdtfrt<-rbind(errdtfr,errdtfr2,errdtfr3)
errdtfrtdf<-data.frame(errdtfr,errdtfr2,errdtfr3)

setwd("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/figures3")

write.xlsx(errdtfrtdf,"hdev.xlsx")

errdtfrd<-data.frame(errdt,rep("RN",length(errdt[,1])))
names(errdtfrd)<-c("Deviation","Hour","Model")

errdtfrd2<-data.frame(errdt2,rep("MLM1",length(errdt2[,1])))
names(errdtfrd2)<-c("Deviation","Hour","Model")

errdtfrd3<-data.frame(errdt3,rep("MLM2",length(errdt3[,1])))
names(errdtfrd3)<-c("Deviation","Hour","Model")

errdtfrdt3<-rbind(errdtfrd,errdtfrd2,errdtfrd3)

png(paste("",getwd(),"/errdtfrtdf.png",sep=""), height = 50*nrow(errdtfrtdf), width = 200*ncol(errdtfrtdf))
grid.table(errdtfrtdf)
dev.off()

#p66 <- ggplot(errdt, aes(hour,deviation))+
#geom_boxplot(fill="brown")

hdev<-ggplot(data = errdtfrdt3, aes(x=hour, y=deviation)) + geom_boxplot(aes(fill=type)) +facet_grid(.~type, scales = "free")
hdev
ggsave(paste("",getwd(),"/hdev.png",sep=""))

davg<-data.frame(hour(obs$date),obs$effekt)
names(davg)<-c("hour","effekt")
davg$hour[davg$hour==0]<-24
davgg<-as_tibble(data.frame(unique(davg$hour),with(davg, tapply(effekt, hour, mean))))
names(davgg)<-c("hour","avg")


hdev2<-ggplot(data = errdtfrdt3, aes(x=Hour, y=Deviation)) + 
  geom_boxplot(aes(fill=Model)) +
  geom_hline(yintercept = 0)+
  #geom_line(data=davgg,aes(x=hour,y=avg))+
  scale_fill_manual(values=c("RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  coord_cartesian(ylim = c(-0.3, 0.3))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())
  
hdev2

#ggsave(paste("",getwd(),"hdev2.png",sep=""))

jpeg(paste(getwd(),"/hdev2.jpeg",sep = ""), width = 1000, height = 750)
hdev2
dev.off()

#distributions

dct<-as_tibble(data.frame(obs,mod))
names(dct)<-c("date","effektobs","date2", "modeffekt")

dctd<-data.frame(dct,rep("RN",length(dct[,1])))
names(dctd)[5]<-c("type")

obsw<-obs[which(as.numeric(format(obs$date,"%m"))<3 | as.numeric(format(obs$date,"%m")) == 12),]
obssp<-obs[which(as.numeric(format(obs$date,"%m"))>2 & as.numeric(format(obs$date,"%m")) < 6),]
obss<-obs[which(as.numeric(format(obs$date,"%m"))>5 & as.numeric(format(obs$date,"%m")) < 9 ),]
obsa<-obs[which(as.numeric(format(obs$date,"%m"))>8 & as.numeric(format(obs$date,"%m")) < 12),]

modw<-mod[which(as.numeric(format(mod$date,"%m"))<3 | as.numeric(format(mod$date,"%m")) == 12),]
modsp<-mod[which(as.numeric(format(mod$date,"%m"))>2 & as.numeric(format(mod$date,"%m")) < 6),]
mods<-mod[which(as.numeric(format(mod$date,"%m"))>5 & as.numeric(format(mod$date,"%m")) < 9 ),]
moda<-mod[which(as.numeric(format(mod$date,"%m"))>8 & as.numeric(format(mod$date,"%m")) < 12),]

dcts<-as_tibble(data.frame(obss,mods))
dcts$date<-1:nrow(dcts)
names(dcts)<-c("no","effektobs","date","modeffekt")

dctsp<-as_tibble(data.frame(obssp,modsp))
dctsp$date<-1:nrow(dctsp)
names(dctsp)<-c("no","effektobs","date","modeffekt")

dcta<-as_tibble(data.frame(obsa,moda))
dcta$date<-1:nrow(dcta)
names(dcta)<-c("no","effektobs","date","modeffekt")

dctw<-as_tibble(data.frame(obsw,modw))
dctw$date<-1:nrow(dctw)
names(dctw)<-c("no","effektobs","date","modeffekt")

dctsd<-data.frame(dcts,rep("RN",length(dcts[,1])))
names(dctsd)[5]<-c("type")

dctspd<-data.frame(dctsp,rep("RN",length(dctsp[,1])))
names(dctspd)[5]<-c("type")

dctad<-data.frame(dcta,rep("RN",length(dcta[,1])))
names(dctad)[5]<-c("type")

dctwd<-data.frame(dctw,rep("RN",length(dctw[,1])))
names(dctwd)[5]<-c("type")

dct2<-as_tibble(data.frame(obs2,mod2))
names(dct2)<-c("date","effektobs","date2", "modeffekt")

modw2<-mod2[which(as.numeric(format(mod2$date,"%m"))<3 | as.numeric(format(mod2$date,"%m")) == 12),]
modsp2<-mod2[which(as.numeric(format(mod2$date,"%m"))>2 & as.numeric(format(mod2$date,"%m")) < 6),]
mods2<-mod2[which(as.numeric(format(mod2$date,"%m"))>5 & as.numeric(format(mod2$date,"%m")) < 9 ),]
moda2<-mod2[which(as.numeric(format(mod2$date,"%m"))>8 & as.numeric(format(mod2$date,"%m")) < 12),]

dcts2<-as_tibble(data.frame(obss,mods2))
dcts2$date<-1:nrow(dcts2)
names(dcts2)<-c("no","effektobs","date","modeffekt")

dctsp2<-as_tibble(data.frame(obssp,modsp2))
dctsp2$date<-1:nrow(dctsp2)
names(dctsp2)<-c("no","effektobs","date","modeffekt")

dcta2<-as_tibble(data.frame(obsa,moda2))
dcta2$date<-1:nrow(dcta2)
names(dcta2)<-c("no","effektobs","date","modeffekt")

dctw2<-as_tibble(data.frame(obsw,modw2))
dctw2$date<-1:nrow(dctw2)
names(dctw2)<-c("no","effektobs","date","modeffekt")

dctd2<-data.frame(dct2,rep("MLM1",length(dct2[,1])))
names(dctd2)[5]<-c("type")

dctsd2<-data.frame(dcts2,rep("MLM1",length(dcts2[,1])))
names(dctsd2)[5]<-c("type")

dctspd2<-data.frame(dctsp2,rep("MLM1",length(dctsp2[,1])))
names(dctspd2)[5]<-c("type")

dctad2<-data.frame(dcta2,rep("MLM1",length(dcta2[,1])))
names(dctad2)[5]<-c("type")

dctwd2<-data.frame(dctw2,rep("MLM1",length(dctw2[,1])))
names(dctwd2)[5]<-c("type")

dct3<-as_tibble(data.frame(obs,mod3))
names(dct3)<-c("date","effektobs","date2", "modeffekt")

modw3<-mod3[which(as.numeric(format(mod3$date,"%m"))<3 | as.numeric(format(mod3$date,"%m")) == 12),]
modsp3<-mod3[which(as.numeric(format(mod3$date,"%m"))>2 & as.numeric(format(mod3$date,"%m")) < 6),]
mods3<-mod3[which(as.numeric(format(mod3$date,"%m"))>5 & as.numeric(format(mod3$date,"%m")) < 9 ),]
moda3<-mod3[which(as.numeric(format(mod3$date,"%m"))>8 & as.numeric(format(mod3$date,"%m")) < 12),]


dcts3<-as_tibble(data.frame(obss,mods3))
dcts3$date<-1:nrow(dcts3)
names(dcts3)<-c("no","effektobs","date","modeffekt")

dctsp3<-as_tibble(data.frame(obssp,modsp3))
dctsp3$date<-1:nrow(dctsp3)
names(dctsp3)<-c("no","effektobs","date","modeffekt")

dcta3<-as_tibble(data.frame(obsa,moda3))
dcta3$date<-1:nrow(dcta3)
names(dcta3)<-c("no","effektobs","date","modeffekt")

dctw3<-as_tibble(data.frame(obsw,modw3))
dctw3$date<-1:nrow(dctw3)
names(dctw3)<-c("no","effektobs","date","modeffekt")

dctd3<-data.frame(dct3,rep("MLM2",length(dct3[,1])))
names(dctd3)[5]<-c("type")

dctsd3<-data.frame(dcts3,rep("MLM2",length(dcts3[,1])))
names(dctsd3)[5]<-c("type")

dctspd3<-data.frame(dctsp3,rep("MLM2",length(dctsp3[,1])))
names(dctspd3)[5]<-c("type")

dctad3<-data.frame(dcta3,rep("MLM2",length(dcta3[,1])))
names(dctad3)[5]<-c("type")

dctwd3<-data.frame(dctw3,rep("MLM2",length(dctw3[,1])))
names(dctwd3)[5]<-c("type")

dctddf<-rbind(dctd,dctd2,dctd3)
names(dctddf)<-c("date1","effekt1","date2","effekt2","type")

colors3<-c("#c72321", "#0d8085", "#efc220")
colorsERC5<-c("#c62220", "#fbd7a8", "#7a6952", "#0d8085", "#f0c220")
"#c72321","#861719","#fbd7a9","#ba9f7c","#7a6952","#6e9b9e","#0d8085","#19484c","#f0c320","#af8f19"

#7a6952
#ba9f7c
#19484c
#af8f19

#dctp <- ggplot() +
#  geom_histogram(data=dctddf,aes(x=effekt1, fill="Observations"),alpha=0.6,position="identity", binwidth=0.01)+
#  geom_histogram(data=dctddf,aes(x=effekt2, fill="Model"),alpha=0.6,position="identity", binwidth=0.01)+
#  scale_fill_manual(values=c("Observations"="green", "Model"="orange"))+
#  facet_grid(type~.)+
#  theme_classic(base_size=30)+
#  theme(legend.title = element_blank())+
#  xlab(paste(unit))+
#  ylab("Frequency")  
#dctp

#dctddf<-

dctddf$type<-factor(as.character(dctddf$type), levels=c("RN","MLM1","MLM2","Observations"))


dctp <- ggplot() +
  geom_histogram(data=dctddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  scale_fill_manual(breaks=c("RN", "MLM1", "MLM2", "Observations"),labels = c("RN", "MLM1", "MLM2", "Observations"),values = c("RN"="#c72321", "MLM1"="#0d8085", "MLM2"="#efc220", "Observations"="darkgrey"))+
  xlab(paste(unit))+
  ylab("Frequency") 

dctp


jpeg(paste(getwd(),"/dctp.jpeg",sep = ""), width = 1000, height = 750)
dctp
dev.off()

params <- as.list(MASS::fitdistr(as.numeric(obs$effekt), "t")$estimate)

dtest<-ggplot() +
  geom_density(data=dctddf,aes(x=effekt1, fill="Observations"),alpha=0.7,position="identity")+
  geom_density(data=dctddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity")+
  facet_grid(type~.,scales = "free")+
  scale_fill_manual(breaks=c("RN", "MLM1", "MLM2", "Observations"),labels = c("RN", "MLM1", "MLM2", "Observations"),values = c("RN"="#c72321", "MLM1"="#0d8085", "MLM2"="#efc220", "Observations"="darkgrey"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  xlab(paste(unit))+
  ylab("Density")

dtest

jpeg(paste(getwd(),"/dtest.jpeg",sep = ""), width = 1000, height = 750)
dtest
dev.off()

dctsddf<-rbind(dctsd,dctsd2,dctsd3)
names(dctsddf)<-c("date1","effekt1","date2","effekt2","type")

#dctsp <- ggplot() +
#  geom_histogram(data=dctsddf,aes(x=effekt1, fill="Observations"),alpha=0.6,position="identity", binwidth=0.01)+
#  geom_histogram(data=dctsddf,aes(x=effekt2, fill="Model"),alpha=0.6,position="identity", binwidth=0.01)+
#  scale_fill_manual(values=c("Observations"="green", "Model"="orange"))+
#  facet_grid(type~.)+
#  theme_classic(base_size=28)+
#  theme(legend.title = element_blank())+
#  xlab(paste(unit))+
#  ylab("Frequency")  
#dctsp

dctsp <- ggplot() +
  geom_histogram(data=dctsddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctsddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  scale_fill_manual(values=c("Observations"="darkgrey", "RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit))+
  ylab("Frequency") 

dctsp

jpeg(paste(getwd(),"/dctsp.jpeg",sep = ""), width = 1000, height = 750)
dctsp
dev.off()

dctspddf<-rbind(dctspd,dctspd2,dctspd3)
names(dctspddf)<-c("date1","effekt1","date2","effekt2","type")

#dctspp <- ggplot() +
#  geom_histogram(data=dctspddf,aes(x=effekt1, fill="Observations"),alpha=0.6,position="identity", binwidth=0.01)+
#  geom_histogram(data=dctspddf,aes(x=effekt2, fill="Model"),alpha=0.6,position="identity", binwidth=0.01)+
#  scale_fill_manual(values=c("Observations"="green", "Model"="orange"))+
#  facet_grid(type~.)+
#  theme_classic(base_size=22)+
#  theme(legend.title = element_blank())+
#  xlab(paste(unit))+
#  ylab("Frequency")  
#dctspp

dctspp <- ggplot() +
  geom_histogram(data=dctspddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctspddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  scale_fill_manual(values=c("Observations"="darkgrey", "RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit))+
  ylab("Frequency") 

dctspp

jpeg(paste(getwd(),"/dctspp.jpeg",sep = ""), width = 1000, height = 750)
dctspp
dev.off()

dctaddf<-rbind(dctad,dctad2,dctad3)
names(dctaddf)<-c("date1","effekt1","date2","effekt2","type")

#dctap <- ggplot() +
#  geom_histogram(data=dctaddf,aes(x=effekt1, fill="Observations"),alpha=0.6,position="identity", binwidth=0.01)+
#  geom_histogram(data=dctaddf,aes(x=effekt2, fill="Model"),alpha=0.6,position="identity", binwidth=0.01)+
#  scale_fill_manual(values=c("Observations"="green", "Model"="orange"))+
#  facet_grid(type~.)+
#  theme_classic(base_size=22)+
#  theme(legend.title = element_blank())+
#  xlab(paste(unit))+
#  ylab("Frequency")  
#dctap

dctap <- ggplot() +
  geom_histogram(data=dctaddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctaddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  scale_fill_manual(values=c("Observations"="darkgrey", "RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit))+
  ylab("Frequency") 

dctap

jpeg(paste(getwd(),"/dctap.jpeg",sep = ""), width = 1000, height = 750)
dctap
dev.off()

dctwddf<-rbind(dctwd,dctwd2,dctwd3)
names(dctwddf)<-c("date1","effekt1","date2","effekt2","type")

#dctwp <- ggplot() +
#  geom_histogram(data=dctwddf,aes(x=effekt1, fill="Observations"),alpha=0.6,position="identity", binwidth=0.01)+
#  geom_histogram(data=dctwddf,aes(x=effekt2, fill="Model"),alpha=0.6,position="identity", binwidth=0.01)+
#  scale_fill_manual(values=c("Observations"="green", "Model"="orange"))+
#  facet_grid(type~.)+
#  theme_classic(base_size=22)+
#  theme(legend.title = element_blank())+
#  xlab(paste(unit))+
#  ylab("Frequency")  
#dctwp

dctwp <- ggplot() +
  geom_histogram(data=dctwddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctwddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  scale_fill_manual(values=c("Observations"="darkgrey", "RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit))+
  ylab("Frequency") 

dctwp

jpeg(paste(getwd(),"/dctwp.jpeg",sep = ""), width = 1000, height = 750)
dctwp
dev.off()


        
#seasonal errors

error<-mod$effekt-obs$effekt

err<-data.frame(obs$effekt,error)
#bre<-seq(0.1,0.9,0.1)
#err.cut = cut(err, bre, right=TRUE) 
#errt<-split(err,err[,1])

errtd<-vector("list", 9)

errt<-split(err,cut(err[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtd<-lapply(1:length(errt),function(i){
  if(length(errt[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(err))
  errtd[[i]]<-hh
  }else{errtd[[i]]<-errt[[i]]
  }
  })

#errtt<-errtd

erb<-lapply(1:length(errt),function(i,errt){
  if(i<9){
  erb<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errt[[i]]$error))}else{rep(">0.8",length(errt[[i]]$error))}
   
  },errt=errt)

errd<-as_tibble(data.frame(bind_rows(errt),unlist(erb)))

errors<-mods$effekt-obss$effekt

errs<-data.frame(obss$effekt,errors)
#bre<-seq(0.1,0.9,0.1)
#err.cut = cut(err, bre, right=TRUE) 
#errt<-split(err,err[,1])

errtds<-vector("list", 9)


errts<-split(errs,cut(errs[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtds<-lapply(1:length(errts),function(i){
  if(length(errts[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errs))
  errtds[[i]]<-hh
  }else{errtds[[i]]<-errts[[i]]
  }
  })

erbs<-lapply(1:length(errts),function(i,errts){
  if(i<9){
  erbs<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errts[[i]]$error))}else{rep(">0.8",length(errts[[i]]$error))}
   
  },errts=errts)

errds<-as_tibble(data.frame(bind_rows(errts),unlist(erbs)))

errorsp<-modsp$effekt-obssp$effekt

errsp<-data.frame(obssp$effekt,errorsp)

errtdsp<-vector("list", 9)

errtsp<-split(errsp,cut(errsp[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdsp<-lapply(1:length(errtsp),function(i){
  if(length(errtsp[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errsp))
  errtdsp[[i]]<-hh
  }else{errtdsp[[i]]<-errtsp[[i]]
  }
  })

errttsp<-errtdsp

erbsp<-lapply(1:length(errtsp),function(i,errtsp){
  if(i<9){
  erbsp<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtsp[[i]]$error))}else{rep(">0.8",length(errtsp[[i]]$error))}
   
  },errtsp=errtsp)

errdsp<-as_tibble(data.frame(bind_rows(errtsp),unlist(erbsp)))
#########

errora<-moda$effekt-obsa$effekt

erra<-data.frame(obsa$effekt,errora)

errtda<-vector("list", 9)


errta<-split(erra,cut(erra[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtda<-lapply(1:length(errta),function(i){
  if(length(errta[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(erra))
  errtda[[i]]<-hh
  }else{errtda[[i]]<-errta[[i]]
  }
  })



erba<-lapply(1:length(errta),function(i,errta){
  if(i<9){
  erba<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errta[[i]]$error))}else{rep(">0.8",length(errta[[i]]$error))}
   
  },errta=errta)

errda<-as_tibble(data.frame(bind_rows(errta),unlist(erba)))

###########

errorw<-modw$effekt-obsw$effekt

errw<-data.frame(obsw$effekt,errorw)

errtdw<-vector("list", 9)


errtw<-split(errw,cut(errw[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdw<-lapply(1:length(errtw),function(i){
  if(length(errtw[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errw))
  errtdw[[i]]<-hh
  }else{errtdw[[i]]<-errtw[[i]]
  }
  })


erbw<-lapply(1:length(errtw),function(i,errtw){
  if(i<9){
  erbw<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtw[[i]]$error))}else{rep(">0.8",length(errtw[[i]]$error))}
   
  },errtw=errtw)

errdw<-as_tibble(data.frame(bind_rows(errtw),unlist(erbw)))

#######

#######

error2<-mod2$effekt-obs$effekt

err2<-data.frame(obs$effekt,error2)

errtd2<-vector("list", 9)


errt2<-split(err2,cut(err2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtd2<-lapply(1:length(errt2),function(i){
  if(length(errt2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(err2))
  errtd2[[i]]<-hh
  }else{errtd2[[i]]<-errt2[[i]]
  }
  })


erb2<-lapply(1:length(errt2),function(i,errt2){
  if(i<9){
  erb2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errt2[[i]]$error))}else{rep(">0.8",length(errt2[[i]]$error))}
   
  },errt2=errt2)

errd2<-as_tibble(data.frame(bind_rows(errt2),unlist(erb2)))

errors2<-mods2$effekt-obss$effekt

errs2<-data.frame(obss$effekt,errors2)

errtds2<-vector("list", 9)

errts2<-split(errs2,cut(errs2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtds2<-lapply(1:length(errts2),function(i){
  if(length(errts2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errs2))
  errtds2[[i]]<-hh
  }else{errtds2[[i]]<-errts2[[i]]
  }
  })


erbs2<-lapply(1:length(errts2),function(i,errts2){
  if(i<9){
  erbs2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errts2[[i]]$error))}else{rep(">0.8",length(errts2[[i]]$error))}
   
  },errts2=errts2)

errds2<-as_tibble(data.frame(bind_rows(errts2),unlist(erbs2)))

errorsp2<-modsp2$effekt-obssp$effekt

errsp2<-data.frame(obssp$effekt,errorsp2)

errtdsp2<-vector("list", 9)


errtsp2<-split(errsp2,cut(errsp2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdsp2<-lapply(1:length(errtsp2),function(i){
  if(length(errtsp2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errsp2))
  errtdsp2[[i]]<-hh
  }else{errtdsp2[[i]]<-errtsp2[[i]]
  }
  })

erbsp2<-lapply(1:length(errtsp2),function(i,errtsp2){
  if(i<9){
  erbsp2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtsp2[[i]]$error))}else{rep(">0.8",length(errtsp2[[i]]$error))}
   
  },errtsp2=errtsp2)

errdsp2<-as_tibble(data.frame(bind_rows(errtsp2),unlist(erbsp2)))

#########

errora2<-moda2$effekt-obsa$effekt

erra2<-data.frame(obsa$effekt,errora2)

errtda2<-vector("list", 9)


errta2<-split(erra2,cut(erra2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtda2<-lapply(1:length(errta2),function(i){
  if(length(errta2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(erra2))
  errtda2[[i]]<-hh
  }else{errtda2[[i]]<-errta2[[i]]
  }
  })


erba2<-lapply(1:length(errta2),function(i,errta2){
  if(i<9){
  erba2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errta2[[i]]$error))}else{rep(">0.8",length(errta2[[i]]$error))}
   
  },errta2=errta2)

errda2<-as_tibble(data.frame(bind_rows(errta2),unlist(erba2)))

###########

errorw2<-modw2$effekt-obsw$effekt

errw2<-data.frame(obsw$effekt,errorw2)

errtdw2<-vector("list", 9)


errtw2<-split(errw2,cut(errw2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdw2<-lapply(1:length(errtw2),function(i){
  if(length(errtw2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errw2))
  errtdw2[[i]]<-hh
  }else{errtdw2[[i]]<-errtw2[[i]]
  }
  })

erbw2<-lapply(1:length(errtw2),function(i,errtw2){
  if(i<9){
  erbw2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtw2[[i]]$error))}else{rep(">0.8",length(errtw2[[i]]$error))}
   
  },errtw2=errtw2)

errdw2<-as_tibble(data.frame(bind_rows(errtw2),unlist(erbw2)))

#######
errorab<-abs(mod$effekt-obs$effekt)

errab<-data.frame(obs$effekt,errorab)

errtdab<-vector("list", 9)

errtab<-split(errab,cut(errab[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdab<-lapply(1:length(errtab),function(i){
  if(length(errtab[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errab))
  errtdab[[i]]<-hh
  }else{errtdab[[i]]<-errtab[[i]]
  }
  })

erbab<-lapply(1:length(errtab),function(i,errtab){
  if(i<9){
  erbab<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtab[[i]]$error))}else{rep(">0.8",length(errtab[[i]]$error))}
   
  },errtab=errtab)

errdab<-as_tibble(data.frame(bind_rows(errtab),unlist(erbab)))

#######
errorab2<-abs(mod2$effekt-obs$effekt)

errab2<-data.frame(obs$effekt,errorab2)
#bre<-seq(0.1,0.9,0.1)
#err.cut = cut(err, bre, right=TRUE) 
#errt<-split(err,err[,1])

errtdab2<-vector("list", 9)

errtab2<-split(errab2,cut(errab2[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdab2<-lapply(1:length(errtab2),function(i){
  if(length(errtab2[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errab2))
  errtdab2[[i]]<-hh
  }else{errtdab2[[i]]<-errtab2[[i]]
  }
  })

erbab2<-lapply(1:length(errtab2),function(i,errtab2){
  if(i<9){
  erbab2<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtab2[[i]]$error))}else{rep(">0.8",length(errtab2[[i]]$error))}
   
  },errtab2=errtab2)

errdab2<-as_tibble(data.frame(bind_rows(errtab2),unlist(erbab2)))
#######

error3<-mod3$effekt-obs$effekt

err3<-data.frame(obs$effekt,error3)

errtd3<-vector("list", 9)

errt3<-split(err3,cut(err3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtd3<-lapply(1:length(errt3),function(i){
  if(length(errt3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(err3))
  errtd3[[i]]<-hh
  }else{errtd3[[i]]<-errt3[[i]]
  }
  })


erb3<-lapply(1:length(errt3),function(i,errt3){
  if(i<9){
  erb3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errt3[[i]]$error))}else{rep(">0.8",length(errt3[[i]]$error))}
   
  },errt3=errt3)

errd3<-as_tibble(data.frame(bind_rows(errt3),unlist(erb3)))

errors3<-mods3$effekt-obss$effekt

errs3<-data.frame(obss$effekt,errors3)

errtds3<-vector("list", 9)

errts3<-split(errs3,cut(errs3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtds3<-lapply(1:length(errts3),function(i){
  if(length(errts3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errs3))
  errtds3[[i]]<-hh
  }else{errtds3[[i]]<-errts3[[i]]
  }
  })


erbs3<-lapply(1:length(errts3),function(i,errts3){
  if(i<9){
  erbs3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errts3[[i]]$error))}else{rep(">0.8",length(errts3[[i]]$error))}
   
  },errts3=errts3)

errds3<-as_tibble(data.frame(bind_rows(errts3),unlist(erbs3)))

errorsp3<-modsp3$effekt-obssp$effekt

errsp3<-data.frame(obssp$effekt,errorsp3)

errtdsp3<-vector("list", 9)


errtsp3<-split(errsp3,cut(errsp3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdsp3<-lapply(1:length(errtsp3),function(i){
  if(length(errtsp3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errsp3))
  errtdsp3[[i]]<-hh
  }else{errtdsp3[[i]]<-errtsp3[[i]]
  }
  })


erbsp3<-lapply(1:length(errtsp3),function(i,errtsp3){
  if(i<9){
  erbsp3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtsp3[[i]]$error))}else{rep(">0.8",length(errtsp3[[i]]$error))}
   
  },errtsp3=errtsp3)

errdsp3<-as_tibble(data.frame(bind_rows(errtsp3),unlist(erbsp3)))

#########

errora3<-moda3$effekt-obsa$effekt

erra3<-data.frame(obsa$effekt,errora3)

errtda3<-vector("list", 9)

errta3<-split(erra3,cut(erra3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtda3<-lapply(1:length(errta3),function(i){
  if(length(errta3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(erra3))
  errtda3[[i]]<-hh
  }else{errtda3[[i]]<-errta3[[i]]
  }
  })


erba3<-lapply(1:length(errta3),function(i,errta3){
  if(i<9){
  erba3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errta3[[i]]$error))}else{rep(">0.8",length(errta3[[i]]$error))}
   
  },errta3=errta3)

errda3<-as_tibble(data.frame(bind_rows(errta3),unlist(erba3)))

###########

errorw3<-modw3$effekt-obsw$effekt

errw3<-data.frame(obsw$effekt,errorw3)

errtdw3<-vector("list", 9)


errtw3<-split(errw3,cut(errw3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdw3<-lapply(1:length(errtw3),function(i){
  if(length(errtw3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errw3))
  errtdw3[[i]]<-hh
  }else{errtdw3[[i]]<-errtw3[[i]]
  }
  })


erbw3<-lapply(1:length(errtw3),function(i,errtw3){
  if(i<9){
  erbw3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtw3[[i]]$error))}else{rep(">0.8",length(errtw3[[i]]$error))}
   
  },errtw3=errtw3)

errdw3<-as_tibble(data.frame(bind_rows(errtw3),unlist(erbw3)))

#######
errorab3<-abs(mod3$effekt-obs$effekt)

errab3<-data.frame(obs$effekt,errorab3)

errtdab3<-vector("list", 9)

errtab3<-split(errab3,cut(errab3[,1], breaks = seq(from = 0, to = 0.9, by = 0.1),include.lowest = TRUE, right = TRUE))

errtdab3<-lapply(1:length(errtab3),function(i){
  if(length(errtab3[[i]][,1])==0){
  hh<-data.frame(0,0)
  names(hh)<-c(names(errab3))
  errtdab3[[i]]<-hh
  }else{errtdab3[[i]]<-errtab3[[i]]
  }
  })

erbab3<-lapply(1:length(errtab3),function(i,errtab3){
  if(i<9){
  erbab3<-rep(paste("0.",i-1,"-","0.",i, sep=""),length(errtab3[[i]]$error))}else{rep(">0.8",length(errtab3[[i]]$error))}
   
  },errtab3=errtab3)

errdab3<-as_tibble(data.frame(bind_rows(errtab3),unlist(erbab3)))

#######

errdd3<-data.frame(errd3,rep("MLM2",length(errd3[,1])))
names(errdd3)<-c("obs","err","class","type")

errddab3<-data.frame(errdab3,rep("MLM2",length(errdab3[,1])))
names(errddab3)<-c("obs","err","class","type")

errdds3<-data.frame(errds3,rep("MLM2",length(errds3[,1])))
names(errdds3)<-c("obs","err","class","type")
errdds3$class<-factor(as.character(errdds3$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddsp3<-data.frame(errdsp3,rep("MLM2",length(errdsp3[,1])))
names(errddsp3)<-c("obs","err","class","type")

errddw3<-data.frame(errdw3,rep("MLM2",length(errdw3[,1])))
names(errddw3)<-c("obs","err","class","type")
errddw3$class<-factor(as.character(errddw3$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errdda3<-data.frame(errda3,rep("MLM2",length(errda3[,1])))
names(errdda3)<-c("obs","err","class","type")

#######
errdd<-data.frame(errd,rep("RN",length(errd[,1])))
names(errdd)<-c("obs","err","class","type")

errddab<-data.frame(errdab,rep("RN",length(errdab[,1])))
names(errddab)<-c("obs","err","class","type")

errdds<-data.frame(errds,rep("RN",length(errds[,1])))
names(errdds)<-c("obs","err","class","type")
errdds$class<-factor(as.character(errdds$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddsp<-data.frame(errdsp,rep("RN",length(errdsp[,1])))
names(errddsp)<-c("obs","err","class","type")

errddw<-data.frame(errdw,rep("RN",length(errdw[,1])))
names(errddw)<-c("obs","err","class","type")
errddw$class<-factor(as.character(errddw$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))


errdda<-data.frame(errda,rep("RN",length(errda[,1])))
names(errdda)<-c("obs","err","class","type")

errdd2<-data.frame(errd2,rep("MLM1",length(errd2[,1])))
names(errdd2)<-c("obs","err","class","type")

errddab2<-data.frame(errdab2,rep("MLM1",length(errdab2[,1])))
names(errddab2)<-c("obs","err","class","type")

errdds2<-data.frame(errds2,rep("MLM1",length(errds2[,1])))
names(errdds2)<-c("obs","err","class","type")
errdds2$class<-factor(as.character(errdds2$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddsp2<-data.frame(errdsp2,rep("MLM1",length(errdsp2[,1])))
names(errddsp2)<-c("obs","err","class","type")

errddw2<-data.frame(errdw2,rep("MLM1",length(errdw2[,1])))
names(errddw2)<-c("obs","err","class","type")
errddw2$class<-factor(as.character(errddw2$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errdda2<-data.frame(errda2,rep("MLM1",length(errda2[,1])))
names(errdda2)<-c("obs","err","class","type")

errdddf<-rbind(errdd,errdd2,errdd3)
names(errdddf)<-c("obs","err","class","type")
errdddf$class<-factor(as.character(errdddf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddabdf<-rbind(errddab,errddab2,errddab3)
names(errddabdf)<-c("obs","err","class","type")
errddabdf$class<-factor(as.character(errddabdf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddsdf<-rbind(errdds,errdds2,errdds3)
names(errddsdf)<-c("obs","err","class","type")
errddsdf$class<-factor(as.character(errddsdf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errsdf<-data.frame(with(errdds, tapply(err, class, function(x){mean(abs(x))})),with(errdds2, tapply(err, class, function(x){mean(abs(x))})),with(errdds, tapply(err, class, function(x){max(x)-min(x)})),with(errdds2, tapply(err, class, function(x){max(x)-min(x)})))
errsdf<-data.frame(with(errdds, tapply(err, class, mean)),with(errdds2, tapply(err, class, mean)),with(errdds, tapply(err, class, function(x){max(x)-min(x)})),with(errdds2, tapply(err, class, function(x){max(x)-min(x)})))

errdf<-data.frame(with(errdd, tapply(err, class, mean)),with(errdd2, tapply(err, class, mean)),with(errdd, tapply(err, class, function(x){max(x)-min(x)})),with(errdd2, tapply(err, class, function(x){max(x)-min(x)})))

###
errdfx<-data.frame(with(errdd, tapply(err, class, median)),with(errdd2, tapply(err, class, median)),with(errdd3, tapply(err, class, median)),with(errdd, tapply(err, class, function(x){max(x)-min(x)})),with(errdd2, tapply(err, class, function(x){max(x)-min(x)})),with(errdd3, tapply(err, class, function(x){max(x)-min(x)})))
names(errdfx)<-c("medianRN","medianmlm1","medianmlm2","rangeRN","rangemlm1","rangemlm2")
errdfx<-errdfx[ order(factor(as.character(row.names(errdfx)), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))),]

#errdfx2<-data.frame(with(errdd, tapply(err, class, mean)),with(errdd2, tapply(err, class, mean)),with(errdd3, tapply(err, class, mean)),with(errdd, tapply(err, class, function(x){max(x)-min(x)})),with(errdd2, tapply(err, class, function(x){max(x)-min(x)})),with(errdd3, tapply(err, class, function(x){max(x)-min(x)})))
#names(errdfx2)<-c("meanRN","meanmlm1","meanmlm2","rangeRN","rangemlm1","rangemlm2")
#errdfx2<-errdfx2[ order(factor(as.character(row.names(errdfx2)), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))),]
###

errdfxw<-data.frame(with(errddw, tapply(err, class, mean)),with(errddw2, tapply(err, class, mean)),with(errddw3, tapply(err, class, mean)),with(errddw, tapply(err, class, function(x){max(x)-min(x)})),with(errddw2, tapply(err, class, function(x){max(x)-min(x)})),with(errddw3, tapply(err, class, function(x){max(x)-min(x)})))
names(errdfxw)<-c("meanRN","meanmlm1","meanmlm2","rangeRN","rangemlm1","rangemlm2")

errdfxw$class<-factor(as.character(errdfxw$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errdfxw<-data.frame(with(errddw, tapply(err, class, median)),with(errddw2, tapply(err, class, median)),with(errddw3, tapply(err, class, median)),with(errddw, tapply(err, class, function(x){max(x)-min(x)})),with(errddw2, tapply(err, class, function(x){max(x)-min(x)})),with(errddw3, tapply(err, class, function(x){max(x)-min(x)})))
names(errdfxw)<-c("medianRN","medianmlm1","medianmlm2","rangeRN","rangemlm1","rangemlm2")


errdfxs<-data.frame(with(errdds, tapply(err, class, mean)),with(errdds2, tapply(err, class, mean)),with(errdds3, tapply(err, class, mean)),with(errdds, tapply(err, class, function(x){max(x)-min(x)})),with(errdds2, tapply(err, class, function(x){max(x)-min(x)})),with(errdds3, tapply(err, class, function(x){max(x)-min(x)})))
names(errdfxs)<-c("meanRN","meanmlm1","meanmlm2","rangeRN","rangemlm1","rangemlm2")


names(errsdf)<-c("")
#errddw3$class<-factor(as.character(errddw3$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddspdf<-rbind(errddsp,errddsp2,errddsp3)
names(errddspdf)<-c("obs","err","class","type")
errddspdf$class<-factor(as.character(errddspdf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errddwdf<-rbind(errddw,errddw2,errddw3)
names(errddwdf)<-c("obs","err","class","type")
errddwdf$class<-factor(as.character(errddwdf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

errwdf<-data.frame(with(errddw, tapply(err, class, function(x){mean(abs(x))})),with(errddw2, tapply(err, class, function(x){mean(abs(x))})),with(errddw, tapply(err, class, function(x){max(x)-min(x)})),with(errddw2, tapply(err, class, function(x){max(x)-min(x)})))
names(errwdf)<-c("")
errddw3$class<-factor(as.character(errddw3$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))



errddadf<-rbind(errdda,errdda2,errdda3)
names(errddadf)<-c("obs","err","class","type")
errddadf$class<-factor(as.character(errddadf$class), levels=c("0.0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.6-0.7","0.7-0.8",">0.8"))

give.n <- function(x){
  return(c(y = median(x)-0.005, label = length(x))) 
}

n_fun <- function(x){
  return(data.frame(y = -0.3, label = paste0("n = ",length(x)/3)))
}

pdevtot <- ggplot(data=errdddf, aes(x=class, y=err)) +
  geom_boxplot(aes(fill=type))+
  stat_summary(fun.data = n_fun, geom = "text") +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme_classic(base_size=30)+
  theme(legend.title=element_blank())+
  #theme(legend.position="none")+
  scale_fill_manual(values=c("RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  #facet_grid(.~type)+
  xlab(paste(unit,"Range")) +
  ylab("Deviations")

pdevtot  

obsx<-data.frame(obs,rep("Observations",NROW(obs)))
names(obsx)<-c("date","effekt","type")

modxx<-data.frame(mod,rep("RN",NROW(mod)))
names(modxx)<-c("date","effekt","type")
mod2xx<-data.frame(mod2,rep("MLM1",NROW(mod2)))
names(mod2xx)<-c("date","effekt","type")
mod3xx<-data.frame(mod3,rep("MLM2",NROW(mod3)))
names(mod3xx)<-c("date","effekt","type")

modx<-data.frame(mod,rep("RN",NROW(mod)),obs$effekt)
names(modx)<-c("date","effekt","type","obs")
mod2x<-data.frame(mod2,rep("MLM1",NROW(mod2)),obs$effekt)
names(mod2x)<-c("date","effekt","type","obs")
mod3x<-data.frame(mod3,rep("MLM2",NROW(mod3)),obs$effekt)
names(mod3x)<-c("date","effekt","type","obs")

xxc<-bind_rows(modx,mod2x,mod3x)
xxc$type<-factor(as.character(xxc$type), levels=c("RN","MLM1","MLM2","Observations"))

xxc2<-bind_rows(obsx,modxx,mod2xx,mod3xx)
xxc2$type<-factor(as.character(xxc2$type), levels=c("RN","MLM1","MLM2","Observations"))


pdevscat <- ggplot(data=xxc) +
  geom_point(aes(x=obs,y=effekt,colour=type))+
  geom_abline(colour="darkgrey",size=1.2)+
  #stat_summary(fun.data = n_fun, geom = "text") +
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme_classic(base_size=30)+
  theme(legend.title=element_blank())+
  #theme(legend.position="none")+
  scale_colour_manual(values=c("RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220","Observations"="darkgrey"))+
  #facet_grid(.~type)+
  coord_cartesian(xlim = c(0, 1),ylim = c(0, 1))+ 
  xlab(paste(unit,"Range Observations")) +
  ylab(paste(unit,"Range Modelled Values"))

pdevscat  

jpeg(paste(getwd(),"/pdevscat.jpeg",sep = ""), width = 1000, height = 750)
pdevscat
dev.off()


pdevtotab <- ggplot(data=errddabdf, aes(x=class, y=err)) +
  geom_boxplot(aes(fill="error"))+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme_classic(base_size=22)+
  theme(legend.title=element_blank())+
  theme(legend.position="none")+
  scale_fill_manual(values=c("error"="#FF6633"))+
  facet_grid(.~type, scales = "free")+
  xlab(paste(unit,"Range")) +
  ylab("Deviations")
pdevtotab  

jpeg(paste(getwd(),"/pdevtotab.jpeg",sep = ""), width = 1000, height = 750)
pdevtotab  
dev.off()

# pdevs <- ggplot(data=errddsdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill="error"))+
#   stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
#   theme_classic(base_size=22)+
#   theme(legend.title=element_blank())+
#   theme(legend.position="none")+
#   scale_fill_manual(values=c("error"="#FF6633"))+
#   facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdevs <- ggplot(data=errddsdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill=type))+
#   stat_summary(fun.data = n_fun, geom = "text") +
#   theme_classic(base_size=30)+
#   theme(legend.title=element_blank())+
#   #theme(legend.position="none")+
#   #scale_fill_manual(values=c("error"="#FF6633"))+
#   #facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdevs
# 
# jpeg(paste(getwd(),"/pdevs.jpeg",sep = ""), width = 1000, height = 750)
# pdevs  
# dev.off()

obssd<-obss

modsd<-mods
modsd2<-mods

#length(obs$effekt[which(obs$effekt<0.01)])

#length(mod$effekt[which(mod$effekt<0.01)])

#length(mod2$effekt[which(mod2$effekt<0.01)])

#length(mod3$effekt[which(mod3$effekt<0.01)])

length(obs$effekt[which(obs$effekt<0.04)])

length(mod$effekt[which(mod$effekt<0.04)])

length(mod2$effekt[which(mod2$effekt<0.04)])

length(mod3$effekt[which(mod3$effekt<0.04)])

length(obs$effekt[which(obs$effekt>0.08&obs$effekt<0.12)])

length(mod$effekt[which(mod$effekt>0.08&mod$effekt<0.12)])

length(mod2$effekt[which(mod2$effekt>0.08&mod2$effekt<0.12)])

length(mod3$effekt[which(mod3$effekt>0.08&mod3$effekt<0.12)])

length(obs$effekt[which(obs$effekt>0.2&obs$effekt<0.5)])

length(mod$effekt[which(mod$effekt>0.2&mod$effekt<0.5)])

length(mod2$effekt[which(mod2$effekt>0.2&mod2$effekt<0.5)])

length(mod3$effekt[which(mod3$effekt>0.2&mod3$effekt<0.5)])

length(obs$effekt[which(obs$effekt>0.8)])

length(mod$effekt[which(mod$effekt>0.8)])

length(mod2$effekt[which(mod2$effekt>0.8)])

length(mod3$effekt[which(mod3$effekt>0.8)])


length(obssd$effekt[which(obssd$effekt<0.12&obssd$effekt>0.08)])

length(modsd$effekt[which(modsd$effekt<0.12&modsd$effekt>0.08)])

length(modsd2$effekt[which(modsd2$effekt<0.12&modsd2$effekt>0.08)])

length(obssd$effekt[which(obssd$effekt<0.65&obssd$effekt>0.52)])

length(modsd$effekt[which(modsd$effekt<0.65&modsd$effekt>0.52)])

length(modsd2$effekt[which(modsd2$effekt<0.65&modsd2$effekt>0.52)])

length(obssd$effekt[which(obssd$effekt>0.65)])

length(modsd$effekt[which(modsd$effekt>0.65)])

length(modsd2$effekt[which(modsd2$effekt>0.65)])


length(obssd$effekt[which(obssd$effekt<0.04)])

length(modsd$effekt[which(modsd$effekt<0.04)])

length(modsd2$effekt[which(modsd2$effekt<0.04)])

length(obssd$effekt[which(obssd$effekt<0.12&obssd$effekt>0.08)])

length(modsd$effekt[which(modsd$effekt<0.12&modsd$effekt>0.08)])

length(modsd2$effekt[which(modsd2$effekt<0.12&modsd2$effekt>0.08)])

length(obssd$effekt[which(obssd$effekt<0.65&obssd$effekt>0.52)])

length(modsd$effekt[which(modsd$effekt<0.65&modsd$effekt>0.52)])

length(modsd2$effekt[which(modsd2$effekt<0.65&modsd2$effekt>0.52)])

length(obssd$effekt[which(obssd$effekt>0.65)])

length(modsd$effekt[which(modsd$effekt>0.65)])

length(modsd2$effekt[which(modsd2$effekt>0.65)])

# pdevsp <- ggplot(data=errddspdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill="error"))+
#   stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
#   theme_classic(base_size=22)+
#   theme(legend.title=element_blank())+
#   theme(legend.position="none")+
#   scale_fill_manual(values=c("error"="#FF6633"))+
#   facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdevsp <- ggplot(data=errddspdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill=type))+
#   stat_summary(fun.data = n_fun, geom = "text") +
#   theme_classic(base_size=30)+
#   theme(legend.title=element_blank())+
#   #theme(legend.position="none")+
#   #scale_fill_manual(values=c("error"="#FF6633"))+
#   #facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdevsp
# 
# jpeg(paste(getwd(),"/pdevsp.jpeg",sep = ""), width = 1000, height = 750)
# pdevsp
# dev.off()
# 
# pdevw <- ggplot(data=errddwdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill="error"))+
#   stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
#   theme_classic(base_size=22)+
#   theme(legend.title=element_blank())+
#   theme(legend.position="none")+
#   scale_fill_manual(values=c("error"="#FF6633"))+
#   facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# n_fun <- function(x){
#   return(data.frame(y = -0.3, label = paste0("n = ",length(x)/3)))
# }
# 
# pdevw <- ggplot(data=errddwdf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill=type))+
#   stat_summary(fun.data = n_fun, geom = "text") +
#   theme_classic(base_size=30) +
#   theme(legend.title=element_blank()) +
#   #theme(legend.position="none")+
#   #scale_fill_manual(values=c("error"="#FF6633"))+
#   #facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdevw

pdevtotabx <- ggplot(data=errddabdf, aes(x=class, y=err)) +
  geom_boxplot(aes(fill=type))+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme(legend.title=element_blank())+
  #theme(legend.position="none")+
  #scale_fill_manual(values=c("error"="#FF6633"))+
  #facet_grid(.~type)+
  xlab(paste(unit,"Range")) +
  ylab("Deviations")
pdevtotabx 

jpeg(paste(getwd(),"/pdevw.jpeg",sep = ""), width = 1000, height = 750)
pdevw
dev.off()

obswd<-obsw

modwd<-modw
modwd2<-modw

length(obswd$effekt[which(obswd$effekt<0.1)])

length(modwd$effekt[which(modwd$effekt<0.1)])

length(modwd2$effekt[which(modwd2$effekt<0.1)])

length(obswd$effekt[which(obswd$effekt<0.24&obswd$effekt>0.22)])

length(modwd$effekt[which(modwd$effekt<0.24&modwd$effekt>0.22)])

length(modwd2$effekt[which(modwd2$effekt<0.24&modwd2$effekt>0.22)])

length(obswd$effekt[which(obswd$effekt<0.65&obswd$effekt>0.52)])

length(modwd$effekt[which(modwd$effekt<0.65&modwd$effekt>0.52)])

length(modwd2$effekt[which(modwd2$effekt<0.65&modwd2$effekt>0.52)])

length(obswd$effekt[which(obswd$effekt>0.75)])

length(modwd$effekt[which(modwd$effekt>0.75)])

length(modwd2$effekt[which(modwd2$effekt>0.75)])


pdeva <- ggplot(data=errddadf, aes(x=class, y=err)) +
  geom_boxplot(aes(fill="error"))+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median) +
  theme(legend.title=element_blank())+
  theme(legend.position="none")+
  scale_fill_manual(values=c("error"="#FF6633"))+
  facet_grid(.~type, scales = "free")+
  xlab(paste(unit,"Range")) +
  ylab("Deviations")

# pdeva <- ggplot(data=errddadf, aes(x=class, y=err)) +
#   geom_boxplot(aes(fill=type))+
#   stat_summary(fun.data = n_fun, geom = "text") +
#   theme_classic(base_size=30) +
#   theme(legend.title=element_blank())+
#   #theme(legend.position="none")+
#   #scale_fill_manual(values=c("error"="#FF6633"))+
#   #facet_grid(.~type)+
#   xlab(paste(unit,"Range")) +
#   ylab("Deviations")
# 
# pdeva
# 
# jpeg(paste(getwd(),"/pdeva.jpeg",sep = ""), width = 1000, height = 750)
# pdeva
# dev.off()

errallseas<-bind_rows(mutate(errddwdf,seas=rep("win",NROW(errddwdf))),mutate(errddspdf,seas=rep("spr",NROW(errddspdf))),mutate(errddsdf,seas=rep("sum",NROW(errddsdf))),mutate(errddadf,seas=rep("aut",NROW(errddadf))))
#errallseas$class<-factor(as.character(errallseas$class), levels=c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8",">0.8"))

classes2<-vector()

NROW(errallseas)

classes2<-sapply(1:NROW(errallseas),function(i,classes){
  print(i/NROW(errallseas))
  if(errallseas$class[i]%in%c("0.0-0.1","0.1-0.2")){return("0.0-0.2")}
  if(errallseas$class[i]%in%c("0.2-0.3","0.3-0.4")){return("0.2-0.4")}
  if(errallseas$class[i]%in%c("0.4-0.5","0.5-0.6")){return("0.4-0.6")}
  if(errallseas$class[i]%in%c("0.6-0.7","0.7-0.8")){return("0.6-0.8")}
  if(errallseas$class[i]%in%c(">0.8")){return(">0.8")}
})

# classes<-sapply(1:NROW(errallseas),function(i,classes){
#   print(i/NROW(errallseas))
#   if(errallseas$class[i]%in%c("0.0-0.1","0.1-0.2")){classes[i]<<-"0.0-0.2"}
#   if(errallseas$class[i]%in%c("0.2-0.3","0.3-0.4")){classes[i]<<-"0.2-0.4"}
#   if(errallseas$class[i]%in%c("0.4-0.5","0.5-0.6")){classes[i]<<-"0.4-0.6"}
#   if(errallseas$class[i]%in%c("0.6-0.7","0.7-0.8")){classes[i]<<-"0.6-0.8"}
#   if(errallseas$class[i]==">0.8"){classes[i]<<-">0.8"}
# },classes=classes)

errallseas$class<-factor(as.character(classes2), levels=c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8",">0.8"))
errallseas$seas<-factor(as.character(errallseas$seas), levels=c("spr","sum","aut","win"))

pdevall <- ggplot(data=errallseas, aes(x=class, y=err)) +
  geom_boxplot(aes(fill=type))+
  geom_hline(yintercept = 0)+
  stat_summary(fun.data = n_fun, geom = "text") +
  theme_classic(base_size=30) +
  theme(legend.title=element_blank())+
  #theme(legend.position="none")+
  scale_fill_manual(values=c("RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  facet_grid(seas~., scales = "free")+
  xlab(paste(unit,"Range")) +
  ylab("Deviations")

pdevall

#pdevall <- ggplot(data=errallseas, aes(x=class, y=err)) +
#  geom_boxplot(aes(fill=type))+
  #stat_summary(fun.data = n_fun, geom = "text") +
#  theme_classic(base_size=30) +
#  theme(legend.title=element_blank())+
  #theme(legend.position="none")+
#  scale_fill_manual(values=c("RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
#  facet_grid(type~., scales = "free")+
#  xlab(paste(unit,"Range")) +
#  ylab("Deviations")

#pdevall

 # pdevall <- ggplot(data=errallseas, aes(x=class, y=err)) +
 #   geom_boxplot(aes(fill=seas))+
 #   stat_summary(fun.data = n_fun, geom = "text") +
 #   theme_classic(base_size=30) +
 #   theme(legend.title=element_blank())+
 #   #theme(legend.position="none")+
 #   #scale_fill_manual(values=c("error"="#FF6633"))+
 #   facet_grid(type~.)+
 #   xlab(paste(unit,"Range")) +
 #   ylab("Deviations")
 # 
 # pdevall

jpeg(paste(getwd(),"/pdevall2.jpeg",sep = ""), width = 1000, height = 750)
pdevall
dev.off()


###########

  filtersx<-function(x,upperl,lowerl){
  return(x<upperl & x>lowerl)
}
  
  lengthEvents<-function(x){
   r<-rle(x)
   return(r$lengths[r$values])
  }

hthres <- max(mod$effekt,obs$effekt,mod2$effekt,mod3$effekt)

hbreaks<-c(0.75,0.8)

h1<-paste(">",hbreaks[1] ,sep="")
h2<-paste(">",hbreaks[2] ,sep="")
#h3<-paste(">",hbreaks[3] ,sep="")

lbreaks<-c(0.005,0.01)

l1<-paste("<",lbreaks[1] ,sep="")
l2<-paste("<",lbreaks[2] ,sep="")
#l3<-paste("<",lbreaks[3] ,sep="")
  
perslowobs<-lengthEvents(filtersx(obs$effekt,lbreaks[1],-1))
mlobs<-mean(perslowobs)
maxlobs<-max(perslowobs)
m24lobs<-mean(perslowobs[which(perslowobs>=24)])
flobs<-length(which(filtersx(obs$effekt,lbreaks[1],-1)==TRUE))
f24lobs<-length(perslowobs[which(perslowobs>=24)])

perslow1obs<-lengthEvents(filtersx(obs$effekt,lbreaks[2],lbreaks[1]))
mlobs1<-mean(perslow1obs)
maxlobs1<-max(perslow1obs)
m24lobs1<-mean(perslow1obs[which(perslow1obs>=24)])
flobs1<-length(which(filtersx(obs$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lobs1<-length(perslow1obs[which(perslow1obs>=24)])

# perslow2obs<-lengthEvents(filtersx(obs$effekt,lbreaks[3],-1))
# mlobs2<-mean(perslow2obs)
# maxlobs2<-max(perslow2obs)
# m24lobs2<-mean(perslow2obs[which(perslow2obs>=24)])
# flobs2<-length(which(filtersx(obs$effekt,lbreaks[3],-1)==TRUE))
# f24lobs2<-length(perslow2obs[which(perslow2obs>=24)])

perslowmod<-lengthEvents(filtersx(mod$effekt,lbreaks[1],-1))
mlmod<-mean(perslowmod)
maxlmod<-max(perslowmod)
m24lmod<-mean(perslowmod[which(perslowmod>=24)])
flmod<-length(which(filtersx(mod$effekt,lbreaks[1],-1)==TRUE))
f24lmod<-length(perslowmod[which(perslowmod>=24)])

perslow1mod<-lengthEvents(filtersx(mod$effekt,lbreaks[2],lbreaks[1]))
mlmod1<-mean(perslow1mod)
maxlmod1<-max(perslow1mod)
m24lmod1<-mean(perslow1mod[which(perslow1mod>=24)])
flmod1<-length(which(filtersx(mod$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod1<-length(perslow1mod[which(perslow1mod>=24)])

# perslow2mod<-lengthEvents(filtersx(mod$effekt,lbreaks[3],-1))
# mlmod2<-mean(perslow2mod)
# maxlmod2<-max(perslow2mod)
# m24lmod2<-mean(perslow2mod[which(perslow2mod>=24)])
# flmod2<-length(which(filtersx(mod$effekt,lbreaks[3],-1)==TRUE))
# f24lmod2<-length(perslow2mod[which(perslow2mod>=24)])

pershighobs<-lengthEvents(filtersx(obs$effekt,hbreaks[2],hbreaks[1]))
mhobs<-mean(pershighobs)
maxhobs<-max(pershighobs)
m24hobs<-mean(pershighobs[which(pershighobs>=24)])
fhobs<-length(which(filtersx(obs$effekt,hthres,hbreaks[1])==TRUE))
f24hobs<-length(pershighobs[which(pershighobs>=24)])

pershigh1obs<-lengthEvents(filtersx(obs$effekt,hthres,hbreaks[2]))
mhobs1<-mean(pershigh1obs)
maxhobs1<-max(pershigh1obs)
m24hobs1<-mean(pershigh1obs[which(pershigh1obs>=24)])
fhobs1<-length(which(filtersx(obs$effekt,hthres,hbreaks[2])==TRUE))
f24hobs1<-length(pershigh1obs[which(pershigh1obs>=24)])

# pershigh2obs<-lengthEvents(filtersx(obs$effekt,hthres,hbreaks[3]))
# mhobs2<-mean(pershigh2obs)
# maxhobs2<-max(pershigh2obs)
# m24hobs2<-mean(pershigh2obs[which(pershigh2obs>=24)])
# fhobs2<-length(which(filtersx(obs$effekt,hthres,hbreaks[3])==TRUE))
# f24hobs2<-length(pershigh2obs[which(pershigh2obs>=24)])

pershighmod<-lengthEvents(filtersx(mod$effekt,hbreaks[2],hbreaks[1]))
mhmod<-mean(pershighmod)
maxhmod<-max(pershighmod)
m24hmod<-mean(pershighmod[which(pershighmod>=24)])
fhmod<-length(which(filtersx(mod$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod<-length(pershighmod[which(pershighmod>=24)])

pershigh1mod<-lengthEvents(filtersx(mod$effekt,hthres,hbreaks[2]))
mhmod1<-mean(pershigh1mod)
maxhmod1<-max(pershigh1mod)
m24hmod1<-mean(pershigh1mod[which(pershigh1mod>=24)])
fhmod1<-length(which(filtersx(mod$effekt,hthres,hbreaks[2])==TRUE))
f24hmod1<-length(pershigh1mod[which(pershigh1mod>=24)])

# pershigh2mod<-lengthEvents(filtersx(mod$effekt,hthres,hbreaks[3]))
# mhmod2<-mean(pershigh2mod)
# maxhmod2<-max(pershigh2mod)
# m24hmod2<-mean(pershigh2mod[which(pershigh2mod>=24)])
# fhmod2<-length(which(filtersx(mod$effekt,hthres,hbreaks[3])==TRUE))
# f24hmod2<-length(pershigh2mod[which(pershigh2mod>=24)])

breaks7 = seq(1, max(pershighmod), by=1) 
persduration.cut7 = cut(pershighmod, breaks7, right=FALSE) 
persduration.freq7 = c(table(persduration.cut7),0)
high1mod<-as_tibble(data.frame(breaks7,persduration.freq7))

if(length(pershigh1mod)>0){breaks8 = seq(1, max(pershigh1mod), by=1) 
if(length(breaks8) > 1) {persduration.cut8 = cut(pershigh1mod, breaks8, right=FALSE) 
persduration.freq8 = c(table(persduration.cut8),0)
high2mod<-as_tibble(data.frame(breaks8,persduration.freq8))}else{high2mod<-data.frame(breaks8,pershigh1mod)}}else {
  breaks8<-0
  persduration.freq8<-0
  high2mod<-as_tibble(data.frame(breaks8,persduration.freq8))}



# if(length(pershigh2mod)>0){breaks9 = seq(1, max(pershigh2mod), by=1) 
# if(length(breaks9) > 1) {persduration.cut9 = cut(pershigh2mod, breaks9, right=FALSE) 
# persduration.freq9 = c(table(persduration.cut9),0)
# high3mod<-as_tibble(data.frame(breaks9,persduration.freq9))}else{high3mod<-data.frame(breaks9,pershigh2mod)}}else {
#   breaks9<-0
#   persduration.freq9<-0
#   high3mod<-as_tibble(data.frame(breaks9,persduration.freq9))}

breaks10 = seq(1, max(pershighobs), by=1) 
persduration.cut10 = cut(pershighobs, breaks10, right=FALSE) 
persduration.freq10 = c(table(persduration.cut10),0)
high1obs<-as_tibble(data.frame(breaks10,persduration.freq10))

if(length(pershigh1obs)>0){breaks11 = seq(1, max(pershigh1obs), by=1) 
if(length(breaks11) > 1) {persduration.cut11 = cut(pershigh1obs, breaks11, right=FALSE) 
persduration.freq11 = c(table(persduration.cut11),0)
high2obs<-as_tibble(data.frame(breaks11,persduration.freq11))}else{high2obs<-data.frame(breaks11,pershigh1obs)}}else {
  breaks11<-0
  persduration.freq11<-0
  high2obs<-as_tibble(data.frame(breaks11,persduration.freq11))}

h1m<-data.frame(high1mod, rep(h1,length(high1mod$breaks7)), rep("Model",length(high1mod$breaks7)))
names(h1m)<-c("breaks","Frequency","CF","type")
h2m<-data.frame(high2mod, rep(h2,length(high2mod$breaks8)), rep("Model",length(high2mod$breaks8)))
names(h2m)<-c("breaks","Frequency","CF","type")
#h3m<-data.frame(high3mod, rep(h3,length(high3mod$breaks9)), rep("Model",length(high3mod$breaks9)))
#names(h3m)<-c("breaks","Frequency","CF","type")
h1o<-data.frame(high1obs, rep(h1,length(high1obs$breaks10)), rep("Observations",length(high1obs$breaks10)))
names(h1o)<-c("breaks","Frequency","CF","type")
h2o<-data.frame(high2obs, rep(h2,length(high2obs$breaks11)), rep("Observations",length(high2obs$breaks11))) 
names(h2o)<-c("breaks","Frequency","CF","type")
#h3o<-data.frame(high3obs, rep(h3,length(high3obs$breaks12)), rep("Observations",length(high3obs$breaks12)))
#names(h3o)<-c("breaks","Frequency","CF","type")
high<-rbind(h1m,h2m,h1o,h2o)

breaks = seq(1, max(perslowmod), by=1) 
persduration.cut = cut(perslowmod, breaks, right=FALSE) 
persduration.freq = c(table(persduration.cut),0)
low1mod<-as_tibble(data.frame(breaks,persduration.freq))

breaks2 = seq(1, max(perslow1mod), by=1) 
persduration.cut2 = cut(perslow1mod, breaks2, right=FALSE) 
persduration.freq2 = c(table(persduration.cut2),0)
low2mod<-as_tibble(data.frame(breaks2,persduration.freq2))

# breaks3 = seq(1, max(perslow2mod), by=1) 
# persduration.cut3 = cut(perslow2mod, breaks3, right=FALSE) 
# persduration.freq3 = c(table(persduration.cut3),0)
# low3mod<-as_tibble(data.frame(breaks3,persduration.freq3))

breaks4 = seq(1, max(perslowobs), by=1) 
persduration.cut4 = cut(perslowobs, breaks4, right=FALSE) 
persduration.freq4 = c(table(persduration.cut4),0)
low1obs<-as_tibble(data.frame(breaks4,persduration.freq4))

breaks5 = seq(1, max(perslow1obs), by=1) 
persduration.cut5 = cut(perslow1obs, breaks5, right=FALSE) 
persduration.freq5 = c(table(persduration.cut5),0)
low2obs<-as_tibble(data.frame(breaks5,persduration.freq5))

# breaks6 = seq(1, max(perslow2obs), by=1) 
# persduration.cut6 = cut(perslow2obs, breaks6, right=FALSE) 
# persduration.freq6 = c(table(persduration.cut6),0)
# low3obs<-as_tibble(data.frame(breaks6,persduration.freq6))

l1m<-data.frame(low1mod, rep(l1,length(low1mod$breaks)), rep("Model",length(low1mod$breaks)))
names(l1m)<-c("breaks","Frequency","CF","type")
l2m<-data.frame(low2mod, rep(l2,length(low2mod$breaks2)), rep("Model",length(low2mod$breaks2)))
names(l2m)<-c("breaks","Frequency","CF","type")
#l3m<-data.frame(low3mod, rep(l3,length(low3mod$breaks3)), rep("Model",length(low3mod$breaks3)))
#names(l3m)<-c("breaks","Frequency","CF","type")
l1o<-data.frame(low1obs, rep(l1,length(low1obs$breaks4)), rep("Observations",length(low1obs$breaks4)))
names(l1o)<-c("breaks","Frequency","CF","type")
l2o<-data.frame(low2obs, rep(l2,length(low2obs$breaks5)), rep("Observations",length(low2obs$breaks5))) 
names(l2o)<-c("breaks","Frequency","CF","type")
#l3o<-data.frame(low3obs, rep(l3,length(low3obs$breaks6)), rep("Observations",length(low3obs$breaks6)))
#names(l3o)<-c("breaks","Frequency","CF","type")
low<-rbind(l1m,l2m,l1o,l2o)

########## 2

perslowmod2<-lengthEvents(filtersx(mod2$effekt,lbreaks[1],-1))
mlmod2<-mean(perslowmod2)
maxlmod2<-max(perslowmod2)
m24lmod2<-mean(perslowmod2[which(perslowmod2>=24)])
flmod2<-length(which(filtersx(mod2$effekt,lbreaks[1],-1)==TRUE))
f24lmod2<-length(perslowmod2[which(perslowmod2>=24)])

perslow1mod2<-lengthEvents(filtersx(mod2$effekt,lbreaks[2],lbreaks[1]))
mlmod12<-mean(perslow1mod2)
maxlmod12<-max(perslow1mod2)
m24lmod12<-mean(perslow1mod2[which(perslow1mod2>=24)])
flmod12<-length(which(filtersx(mod2$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod12<-length(perslow1mod2[which(perslow1mod2>=24)])

# perslow2mod2<-lengthEvents(filtersx(mod2$effekt,lbreaks[3],-1))
# mlmod22<-mean(perslow2mod2)
# maxlmod22<-max(perslow2mod2)
# m24lmod22<-mean(perslow2mod2[which(perslow2mod2>=24)])
# flmod22<-length(which(filtersx(mod2$effekt,lbreaks[3],-1)==TRUE))
# f24lmod22<-length(perslow2mod2[which(perslow2mod2>=24)])

pershighmod2<-lengthEvents(filtersx(mod2$effekt,hbreaks[2],hbreaks[1]))
mhmod2<-mean(pershighmod2)
maxhmod2<-max(pershighmod2)
m24hmod2<-mean(pershighmod2[which(pershighmod2>=24)])
fhmod2<-length(which(filtersx(mod2$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod2<-length(pershighmod2[which(pershighmod2>=24)])

pershigh1mod2<-lengthEvents(filtersx(mod2$effekt,hthres,hbreaks[2]))
mhmod12<-mean(pershigh1mod2)
maxhmod12<-max(pershigh1mod2)
m24hmod12<-mean(pershigh1mod2[which(pershigh1mod2>=24)])
fhmod12<-length(which(filtersx(mod2$effekt,hthres,hbreaks[2])==TRUE))
f24hmod12<-length(pershigh1mod2[which(pershigh1mod2>=24)])

# pershigh2mod2<-lengthEvents(filtersx(mod2$effekt,hthres,hbreaks[3]))
# mhmod22<-mean(pershigh2mod2)
# maxhmod22<-max(pershigh2mod2)
# m24hmod22<-mean(pershigh2mod2[which(pershigh2mod2>=24)])
# fhmod22<-length(which(filtersx(mod2$effekt,hthres,hbreaks[3])==TRUE))
# f24hmod22<-length(pershigh2mod2[which(pershigh2mod2>=24)])

breaks72 = seq(1, max(pershighmod2), by=1) 
persduration.cut72 = cut(pershighmod2, breaks72, right=FALSE) 
persduration.freq72 = c(table(persduration.cut72),0)
high1mod2<-as_tibble(data.frame(breaks72,persduration.freq72))

if(length(pershigh1mod2)>0){breaks82 = seq(1, max(pershigh1mod2), by=1) 
if(length(breaks82) > 1) {persduration.cut82 = cut(pershigh1mod2, breaks82, right=FALSE) 
persduration.freq82 = c(table(persduration.cut82),0)
high2mod2<-as_tibble(data.frame(breaks82,persduration.freq82))}else{high2mod2<-data.frame(breaks82,pershigh1mod2)}}else {
  breaks82<-0
  persduration.freq82<-0
  high2mod2<-as_tibble(data.frame(breaks82,persduration.freq82))}

# if(length(pershigh2mod2)>0){breaks92 = seq(1, max(pershigh2mod2), by=1) 
# if(length(breaks92) > 1) {persduration.cut92 = cut(pershigh2mod2, breaks92, right=FALSE) 
# persduration.freq92 = c(table(persduration.cut92),0)
# high3mod2<-as_tibble(data.frame(breaks92,persduration.freq92))}else{high3mod2<-data.frame(breaks92,pershigh2mod2)}}else {
#   breaks92<-0
#   persduration.freq92<-0
#   high3mod2<-as_tibble(data.frame(breaks92,persduration.freq92))}

h1m2<-data.frame(high1mod2, rep(h1,length(high1mod2$breaks72)), rep("Model",length(high1mod2$breaks72)))
names(h1m2)<-c("breaks","Frequency","CF","type")
h2m2<-data.frame(high2mod2, rep(h2,length(high2mod2$breaks82)), rep("Model",length(high2mod2$breaks82)))
names(h2m2)<-c("breaks","Frequency","CF","type")
# h3m2<-data.frame(high3mod2, rep(h3,length(high3mod2$breaks92)), rep("Model",length(high3mod2$breaks92)))
# names(h3m2)<-c("breaks","Frequency","CF","type")

high2<-rbind(h1m2,h2m2,h1o,h2o)

breaks2 = seq(1, max(perslowmod2), by=1) 
persduration.cut2 = cut(perslowmod2, breaks2, right=FALSE) 
persduration.freq2 = c(table(persduration.cut2),0)
low1mod2<-as_tibble(data.frame(breaks2,persduration.freq2))

breaks22 = seq(1, max(perslow1mod2), by=1) 
persduration.cut22 = cut(perslow1mod2, breaks22, right=FALSE) 
persduration.freq22 = c(table(persduration.cut22),0)
low2mod2<-as_tibble(data.frame(breaks22,persduration.freq22))

# breaks32 = seq(1, max(perslow2mod2), by=1) 
# persduration.cut32 = cut(perslow2mod2, breaks32, right=FALSE) 
# persduration.freq32 = c(table(persduration.cut32),0)
# low3mod2<-as_tibble(data.frame(breaks32,persduration.freq32))

l1m2<-data.frame(low1mod2, rep(l1,length(low1mod2$breaks2)), rep("Model",length(low1mod2$breaks2)))
names(l1m2)<-c("breaks","Frequency","CF","type")
l2m2<-data.frame(low2mod2, rep(l2,length(low2mod2$breaks22)), rep("Model",length(low2mod2$breaks22)))
names(l2m2)<-c("breaks","Frequency","CF","type")
# l3m2<-data.frame(low3mod2, rep(l3,length(low3mod2$breaks32)), rep("Model",length(low3mod2$breaks32)))
# names(l3m2)<-c("breaks","Frequency","CF","type")

low2<-rbind(l1m2,l2m2,l1o,l2o)

########### 3

perslowmod3<-lengthEvents(filtersx(mod3$effekt,lbreaks[1],-1))
mlmod3<-mean(perslowmod3)
maxlmod3<-max(perslowmod3)
m24lmod3<-mean(perslowmod3[which(perslowmod3>=24)])
flmod3<-length(which(filtersx(mod3$effekt,lbreaks[1],-1)==TRUE))
f24lmod3<-length(perslowmod3[which(perslowmod3>=24)])

perslow1mod3<-lengthEvents(filtersx(mod3$effekt,lbreaks[2],lbreaks[1]))
mlmod13<-mean(perslow1mod3)
maxlmod13<-max(perslow1mod3)
m24lmod13<-mean(perslow1mod3[which(perslow1mod3>=24)])
flmod13<-length(which(filtersx(mod3$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod13<-length(perslow1mod3[which(perslow1mod3>=24)])

# perslow2mod3<-lengthEvents(filtersx(mod3$effekt,lbreaks[3],-1))
# mlmod23<-mean(perslow2mod3)
# maxlmod23<-max(perslow2mod3)
# m24lmod23<-mean(perslow2mod3[which(perslow2mod3>=24)])
# flmod23<-length(which(filtersx(mod3$effekt,lbreaks[3],-1)==TRUE))
# f24lmod23<-length(perslow2mod3[which(perslow2mod3>=24)])

pershighmod3<-lengthEvents(filtersx(mod3$effekt,hbreaks[2],hbreaks[1]))
mhmod3<-mean(pershighmod3)
maxhmod3<-max(pershighmod3)
m24hmod3<-mean(pershighmod3[which(pershighmod3>=24)])
fhmod3<-length(which(filtersx(mod3$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod3<-length(pershighmod3[which(pershighmod3>=24)])

pershigh1mod3<-lengthEvents(filtersx(mod3$effekt,hthres,hbreaks[2]))
mhmod13<-mean(pershigh1mod3)
maxhmod13<-max(pershigh1mod3)
m24hmod13<-mean(pershigh1mod3[which(pershigh1mod3>=24)])
fhmod13<-length(which(filtersx(mod3$effekt,hthres,hbreaks[2])==TRUE))
f24hmod13<-length(pershigh1mod3[which(pershigh1mod3>=24)])

# pershigh2mod3<-lengthEvents(filtersx(mod3$effekt,hthres,hbreaks[3]))
# mhmod23<-mean(pershigh2mod3)
# maxhmod23<-max(pershigh2mod3)
# m24hmod23<-mean(pershigh2mod3[which(pershigh2mod3>=24)])
# fhmod23<-length(which(filtersx(mod3$effekt,hthres,hbreaks[3])==TRUE))
# f24hmod23<-length(pershigh2mod3[which(pershigh2mod3>=24)])

breaks73 = seq(1, max(pershighmod3), by=1) 
persduration.cut73 = cut(pershighmod3, breaks73, right=FALSE) 
persduration.freq73 = c(table(persduration.cut73),0)
high1mod3<-as_tibble(data.frame(breaks73,persduration.freq73))

if(length(pershigh1mod3)>0){breaks83 = seq(1, max(pershigh1mod3), by=1) 
if(length(breaks83) > 1) {persduration.cut83 = cut(pershigh1mod3, breaks83, right=FALSE) 
persduration.freq83 = c(table(persduration.cut83),0)
high2mod3<-as_tibble(data.frame(breaks83,persduration.freq83))}else{high2mod3<-data.frame(breaks83,pershigh1mod3)}}else {
  breaks83<-0
  persduration.freq83<-0
  high2mod3<-as_tibble(data.frame(breaks83,persduration.freq83))}

# if(length(pershigh2mod3)>0){breaks93 = seq(1, max(pershigh2mod3), by=1) 
# if(length(breaks93) > 1) {persduration.cut93 = cut(pershigh2mod3, breaks93, right=FALSE) 
# persduration.freq93 = c(table(persduration.cut93),0)
# high3mod3<-as_tibble(data.frame(breaks93,persduration.freq93))}else{high3mod3<-data.frame(breaks93,pershigh2mod3)}}else {
#   breaks93<-0
#   persduration.freq93<-0
#   high3mod3<-as_tibble(data.frame(breaks93,persduration.freq93))}


h1m3<-data.frame(high1mod3, rep(h1,length(high1mod3$breaks73)), rep("Model",length(high1mod3$breaks73)))
names(h1m3)<-c("breaks","Frequency","CF","type")
h2m3<-data.frame(high2mod3, rep(h2,length(high2mod3$breaks83)), rep("Model",length(high2mod3$breaks83)))
names(h2m3)<-c("breaks","Frequency","CF","type")
# h3m3<-data.frame(high3mod3, rep(h3,length(high3mod3$breaks93)), rep("Model",length(high3mod3$breaks93)))
# names(h3m3)<-c("breaks","Frequency","CF","type")

high3<-rbind(h1m3,h2m3,h1o,h2o)

breaks3 = seq(1, max(perslowmod3), by=1) 
persduration.cut3 = cut(perslowmod3, breaks3, right=FALSE) 
persduration.freq3 = c(table(persduration.cut3),0)
low1mod3<-as_tibble(data.frame(breaks3,persduration.freq3))

breaks23 = seq(1, max(perslow1mod3), by=1) 
persduration.cut23 = cut(perslow1mod3, breaks23, right=FALSE) 
persduration.freq23 = c(table(persduration.cut23),0)
low2mod3<-as_tibble(data.frame(breaks23,persduration.freq23))

# breaks33 = seq(1, max(perslow2mod3), by=1) 
# persduration.cut33 = cut(perslow2mod3, breaks33, right=FALSE) 
# persduration.freq33 = c(table(persduration.cut33),0)
# low3mod3<-as_tibble(data.frame(breaks33,persduration.freq33))

l1m3<-data.frame(low1mod3, rep(l1,length(low1mod3$breaks3)), rep("Model",length(low1mod3$breaks3)))
names(l1m3)<-c("breaks","Frequency","CF","type")
l2m3<-data.frame(low2mod3, rep(l2,length(low2mod3$breaks23)), rep("Model",length(low2mod3$breaks23)))
names(l2m3)<-c("breaks","Frequency","CF","type")
# l3m3<-data.frame(low3mod3, rep(l3,length(low3mod3$breaks33)), rep("Model",length(low3mod3$breaks33)))
# names(l3m3)<-c("breaks","Frequency","CF","type")

low3<-rbind(l1m3,l2m3,l1o,l2o)

###########

highg<-data.frame(high,rep("RN",length(high[,1])))
names(highg)<-c("breaks","Frequency","CF","type","type2")
highg<-highg[,-5]
levels(highg$type)<-c("RN", "Observations")
highg$type[highg$type=="Model"]<-"RN"

highg2<-data.frame(high2,rep("MLM1",length(high2[,1])))
names(highg2)<-c("breaks","Frequency","CF","type","type2")
highg2<-highg2[highg2$type=="Model",-5]
levels(highg2$type)<-c("MLM1", "Observations")
highg2$type[highg2$type=="RN"]<-"MLM1"

highg3<-data.frame(high3,rep("MLM2",length(high3[,1])))
names(highg3)<-c("breaks","Frequency","CF","type","type2")
highg3<-highg3[highg3$type=="Model",-5]
levels(highg3$type)<-c("MLM2", "Observations")
highg3$type[highg3$type=="RN"]<-"MLM2"


lowg<-data.frame(low,rep("RN",length(low[,1])))
names(lowg)<-c("breaks","Frequency","CF","type","type2")
lowg<-lowg[,-5]
levels(lowg$type)<-c("RN", "Observations")
lowg$type[lowg$type=="Model"]<-"RN"

lowg2<-data.frame(low2,rep("MLM1",length(low2[,1])))
names(lowg2)<-c("breaks","Frequency","CF","type","type2")
lowg2<-lowg2[lowg2$type=="Model",-5]
levels(lowg2$type)<-c("MLM1", "Observations")
lowg2$type[lowg2$type=="Model"]<-"MLM1"

lowg3<-data.frame(low3,rep("MLM2",length(low3[,1])))
names(lowg3)<-c("breaks","Frequency","CF","type","type2")
lowg3<-lowg3[lowg3$type=="Model",-5]
levels(lowg3$type)<-c("MLM2", "Observations")
lowg3$type[lowg3$type=="Model"]<-"MLM2"


#highg3<-data.frame(high3,rep("MLM2",length(high3[,1])))
#names(highg3)<-c("breaks","Frequency","CF","type","type3")
#highg3<-highg3[highg3$type=="Model",-5]
#levels(highg3$type)<-c("MLM2", "Observations")
#highg3$type[highg3$type=="RN"]<-"MLM2"

#lowg3<-data.frame(low3,rep("MLM2",length(low3[,1])))
#names(lowg3)<-c("breaks","Frequency","CF","type","type3")
#lowg3<-lowg3[lowg3$type=="Model",-5]
#levels(lowg3$type)<-c("MLM2", "Observations")
#lowg3$type[lowg3$type=="Model"]<-"MLM2"

highgg<-rbind(highg,highg2,highg3)
names(highgg)<-c("breaks","Frequency","CF","type")
levels(highgg$type)<-c("Observations","RN","MLM1","MLM2")

lowgg<-rbind(lowg,lowg2,lowg3)
names(lowgg)<-c("breaks","Frequency","CF","type")
levels(lowgg$type)<-c("Observations","RN","MLM1","MLM2")

phighfreqdur<- ggplot(data=highgg[which(highgg$Frequency!=0),], aes(x = CF, y = breaks, z = Frequency)) +
  geom_tile(aes(fill = Frequency)) +
  theme_grey(base_size = 30)+
  scale_color_manual(values=c("Model"="orange", "Observations"="green"))+
  facet_grid(.~type, scales = "free")+
  xlab(paste(unit,"Threshold"))+
  ylab("Duration of Events(h)")

phighfreqdur  

jpeg(paste(getwd(),"/phighfreqdur.jpeg",sep = ""), width = 1000, height = 750)
phighfreqdur
dev.off()

plowfreqdur<- ggplot(data=lowgg[which(lowgg$Frequency!=0),], aes(x = CF, y = breaks, z = Frequency)) +
  geom_tile(aes(fill = Frequency)) +
  theme_grey(base_size = 30)+
  scale_color_manual(values=c("Model"="orange", "Observations"="green"))+
  facet_grid(.~type, scales = "free")+
  xlab(paste(unit,"Threshold"))+
  ylab("Duration of Events(h)")

plowfreqdur
  
jpeg(paste(getwd(),"/plowfreqdur.jpeg",sep = ""), width = 1000, height = 750)
plowfreqdur
dev.off()
############obs

perslowobs<<-lengthEvents(filtersx(obs$effekt,lbreaks[1],-1))
mlobs<-mean(perslowobs)
maxlobs<-max(perslowobs)
m24lobs<-mean(perslowobs[which(perslowobs>=24)])
flobs<-length(which(filtersx(obs$effekt,lbreaks[1],-1)==TRUE))
f24lobs<-length(perslowobs[which(perslowobs>=24)])

perslow1obs<<-lengthEvents(filtersx(obs$effekt,lbreaks[2],lbreaks[1]))
mlobs1<-mean(perslow1obs)
maxlobs1<-max(perslow1obs)
m24lobs1<-mean(perslow1obs[which(perslow1obs>=24)])
flobs1<-length(which(filtersx(obs$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lobs1<-length(perslow1obs[which(perslow1obs>=24)])

pershighobs<<-lengthEvents(filtersx(obs$effekt,hbreaks[2],hbreaks[1]))
mhobs<-mean(pershighobs)
maxhobs<-max(pershighobs)
m24hobs<-mean(pershighobs[which(pershighobs>=24)])
fhobs<-length(which(filtersx(obs$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hobs<-length(pershighobs[which(pershighobs>=24)])

pershigh1obs<<-lengthEvents(filtersx(obs$effekt,hthres,hbreaks[2]))
mhobs1<-mean(pershigh1obs)
maxhobs1<-max(pershigh1obs)
m24hobs1<-mean(pershigh1obs[which(pershigh1obs>=24)])
fhobs1<-length(which(filtersx(obs$effekt,hthres,hbreaks[2])==TRUE))
f24hobs1<-length(pershigh1obs[which(pershigh1obs>=24)])


##mod

perslowmod<<-lengthEvents(filtersx(mod$effekt,lbreaks[1],-1))
mlmod<-mean(perslowmod)
maxlmod<-max(perslowmod)
m24lmod<-mean(perslowmod[which(perslowmod>=24)])
flmod<-length(which(filtersx(mod$effekt,lbreaks[1],-1)==TRUE))
f24lmod<-length(perslowmod[which(perslowmod>=24)])

perslow1mod<<-lengthEvents(filtersx(mod$effekt,lbreaks[2],lbreaks[1]))
mlmod1<-mean(perslow1mod)
maxlmod1<-max(perslow1mod)
m24lmod1<-mean(perslow1mod[which(perslow1mod>=24)])
flmod1<-length(which(filtersx(mod$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod1<-length(perslow1mod[which(perslow1mod>=24)])


pershighmod<<-lengthEvents(filtersx(mod$effekt,hbreaks[2],hbreaks[1]))
mhmod<-mean(pershighmod)
maxhmod<-max(pershighmod)
m24hmod<-mean(pershighmod[which(pershighmod>=24)])
fhmod<-length(which(filtersx(mod$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod<-length(pershighmod[which(pershighmod>=24)])

pershigh1mod<<-lengthEvents(filtersx(mod$effekt,hthres,hbreaks[2]))
mhmod1<-mean(pershigh1mod)
maxhmod1<-max(pershigh1mod)
m24hmod1<-mean(pershigh1mod[which(pershigh1mod>=24)])
fhmod1<-length(which(filtersx(mod$effekt,hthres,hbreaks[2])==TRUE))
f24hmod1<-length(pershigh1mod[which(pershigh1mod>=24)])

##mod2

perslowmod2<<-lengthEvents(filtersx(mod2$effekt,lbreaks[1],-1))
mlmod2<-mean(perslowmod2)
maxlmod2<-max(perslowmod2)
m24lmod2<-mean(perslowmod2[which(perslowmod2>=24)])
flmod2<-length(which(filtersx(mod2$effekt,lbreaks[1],-1)==TRUE))
f24lmod2<-length(perslowmod2[which(perslowmod2>=24)])

perslow1mod2<<-lengthEvents(filtersx(mod2$effekt,lbreaks[2],lbreaks[1]))
mlmod12<-mean(perslow1mod2)
maxlmod12<-max(perslow1mod2)
m24lmod12<-mean(perslow1mod2[which(perslow1mod2>=24)])
flmod12<-length(which(filtersx(mod2$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod12<-length(perslow1mod2[which(perslow1mod2>=24)])


pershighmod2<<-lengthEvents(filtersx(mod2$effekt,hbreaks[2],hbreaks[1]))
mhmod2<-mean(pershighmod2)
maxhmod2<-max(pershighmod2)
m24hmod2<-mean(pershighmod2[which(pershighmod2>=24)])
fhmod2<-length(which(filtersx(mod2$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod2<-length(pershighmod2[which(pershighmod2>=24)])

pershigh1mod2<<-lengthEvents(filtersx(mod2$effekt,hthres,hbreaks[2]))
mhmod12<-mean(pershigh1mod2)
maxhmod12<-max(pershigh1mod2)
m24hmod12<-mean(pershigh1mod2[which(pershigh1mod2>=24)])
fhmod12<-length(which(filtersx(mod2$effekt,hthres,hbreaks[2])==TRUE))
f24hmod12<-length(pershigh1mod2[which(pershigh1mod2>=24)])
##mod3

perslowmod3<<-lengthEvents(filtersx(mod3$effekt,lbreaks[1],-1))
mlmod3<-mean(perslowmod3)
maxlmod3<-max(perslowmod3)
m24lmod3<-mean(perslowmod3[which(perslowmod3>=24)])
flmod3<-length(which(filtersx(mod3$effekt,lbreaks[1],-1)==TRUE))
f24lmod3<-length(perslowmod3[which(perslowmod3>=24)])

perslow1mod3<<-lengthEvents(filtersx(mod3$effekt,lbreaks[2],lbreaks[1]))
mlmod13<-mean(perslow1mod3)
maxlmod13<-max(perslow1mod3)
m24lmod13<-mean(perslow1mod3[which(perslow1mod3>=24)])
flmod13<-length(which(filtersx(mod3$effekt,lbreaks[2],lbreaks[1])==TRUE))
f24lmod13<-length(perslow1mod3[which(perslow1mod3>=24)])

pershighmod3<<-lengthEvents(filtersx(mod3$effekt,hbreaks[2],hbreaks[1]))
mhmod3<-mean(pershighmod3)
maxhmod3<-max(pershighmod3)
m24hmod3<-mean(pershighmod3[which(pershighmod3>=24)])
fhmod3<-length(which(filtersx(mod3$effekt,hbreaks[2],hbreaks[1])==TRUE))
f24hmod3<-length(pershighmod3[which(pershighmod3>=24)])

pershigh1mod3<<-lengthEvents(filtersx(mod3$effekt,hthres,hbreaks[2]))
mhmod13<-mean(pershigh1mod3)
maxhmod13<-max(pershigh1mod3)
m24hmod13<-mean(pershigh1mod3[which(pershigh1mod3>=24)])
fhmod13<-length(which(filtersx(mod3$effekt,hthres,hbreaks[2])==TRUE))
f24hmod13<-length(pershigh1mod3[which(pershigh1mod3>=24)])

##

dft<<-data.frame(rbind(flobs,flmod,flmod2,flmod3,mlobs,mlmod,mlmod2,mlmod3,maxlobs,maxlmod,maxlmod2,maxlmod3),
                 rbind(flobs1,flmod1,flmod12,flmod13,mlobs1,mlmod1,mlmod12,mlmod13,maxlobs1,maxlmod1,maxlmod12,maxlmod13),
                 
                 rbind(fhobs,fhmod,fhmod2,fhmod3,mhobs,mhmod,mhmod2,mhmod3,maxhobs,maxhmod,maxhmod2,maxhmod3),
                 rbind(fhobs1,fhmod1,fhmod12,fhmod13,mhobs1,mhmod1,mhmod12,mhmod13,maxhobs1,maxhmod1,maxhmod12,maxhmod13))

library(gridExtra)
               
  
rownames(dft)<-c("o-freq","m-freq","m2-freq","m3-freq","o-mean","m-mean","m2-mean","m3-mean","o-max","m-max","m2-max","m3-max")
names(dft)<-c(l1,l2,h1,h2)

dev.off()

grid.table(dft)

png(paste("",getwd(),"/dft.png",sep=""), height = 50*nrow(dft), width = 200*ncol(dft))
grid.table(dft)
dev.off()

#########ramps

se<-unlist(lapply(1:length(mod$effekt),function(i){res<-seq(i+1,i+12,1)}))
tsx<-mod$effekt[se]-rep(mod$effekt,each=12)

breaksx = seq(-1, 1, by=0.01) 
tsx.cut = cut(tsx, breaksx, right=FALSE) 
tsx.freq = table(tsx.cut)
tsx12h<-data.frame(breaksx[2:length(breaksx)], tsx.freq)
names(tsx12h)<-c("CFchange","bins","Frequency")
rc12h<-as_tibble(tsx12h)

se2<-unlist(lapply(1:length(mod2$effekt),function(i){res2<-seq(i+1,i+12,1)}))
tsx2<-mod2$effekt[se2]-rep(mod2$effekt,each=12)

breaksx2 = seq(-1, 1, by=0.01) 
tsx.cut2 = cut(tsx2, breaksx2, right=FALSE) 
tsx.freq2 = table(tsx.cut2)
tsx12h2<-data.frame(breaksx2[2:length(breaksx2)], tsx.freq2)
names(tsx12h2)<-c("CFchange","bins","Frequency")
rc12h2<-as_tibble(tsx12h2)

se3<-unlist(lapply(1:length(mod3$effekt),function(i){res3<-seq(i+1,i+12,1)}))
tsx3<-mod3$effekt[se3]-rep(mod3$effekt,each=12)

breaksx3 = seq(-1, 1, by=0.01) 
tsx.cut3 = cut(tsx3, breaksx3, right=FALSE) 
tsx.freq3 = table(tsx.cut3)
tsx12h3<-data.frame(breaksx3[2:length(breaksx3)], tsx.freq3)
names(tsx12h3)<-c("CFchange","bins","Frequency")
rc12h3<-as_tibble(tsx12h3)

seo<-unlist(lapply(1:length(obs$effekt),function(i){res<-seq(i+1,i+12,1)}))
tsxo<<-obs$effekt[seo]-rep(obs$effekt,each=12)

breaksxo = seq(-1, 1, by=0.01) 
tsxo.cut = cut(tsxo, breaksxo, right=FALSE) 
tsxo.freq = table(tsxo.cut)
tsxo12h<-data.frame(breaksxo[2:length(breaksxo)], tsxo.freq)
names(tsxo12h)<-c("CFchange","bins","Frequency")
rco12h<-as_tibble(tsxo12h)

se1<-unlist(lapply(1:length(mod$effekt),function(i){res<-seq(i+1,i+1,1)}))
tsx1<-mod$effekt[se1]-rep(mod$effekt,each=1)

breaksx1 = seq(-1, 1, by=0.01) 
tsx1.cut = cut(tsx1, breaksx1, right=FALSE) 
tsx1.freq = table(tsx1.cut)
tsx1h<-data.frame(breaksx1[2:length(breaksx1)], tsx1.freq)
names(tsx1h)<-c("CFchange","bins","Frequency")
rc1h<-as_tibble(tsx1h)

se12<-unlist(lapply(1:length(mod2$effekt),function(i){res12<-seq(i+1,i+1,1)}))
tsx12<-mod2$effekt[se12]-rep(mod2$effekt,each=1)

breaksx12 = seq(-1, 1, by=0.01) 
tsx1.cut2 = cut(tsx12, breaksx12, right=FALSE) 
tsx1.freq2 = table(tsx1.cut2)
tsx1h2<-data.frame(breaksx12[2:length(breaksx12)], tsx1.freq2)
names(tsx1h2)<-c("CFchange","bins","Frequency")
rc1h2<-as_tibble(tsx1h2)

se13<-unlist(lapply(1:length(mod3$effekt),function(i){res13<-seq(i+1,i+1,1)}))
tsx13<-mod3$effekt[se13]-rep(mod3$effekt,each=1)

breaksx13 = seq(-1, 1, by=0.01) 
tsx1.cut3 = cut(tsx13, breaksx13, right=FALSE) 
tsx1.freq3 = table(tsx1.cut3)
tsx1h3<-data.frame(breaksx13[2:length(breaksx13)], tsx1.freq3)
names(tsx1h3)<-c("CFchange","bins","Frequency")
rc1h3<-as_tibble(tsx1h3)

se1o<-unlist(lapply(1:length(obs$effekt),function(i){res<-seq(i+1,i+1,1)}))
tsx1o<-obs$effekt[se1o]-rep(obs$effekt,each=1)

breaksx1o = seq(-1, 1, by=0.01) 
tsx1o.cut = cut(tsx1o, breaksx1o, right=FALSE) 
tsx1o.freq = table(tsx1o.cut)
tsxo1h<-data.frame(breaksx1o[2:length(breaksx1o)], tsx1o.freq)
names(tsxo1h)<-c("CFchange","bins","Frequency")
rco1h<-as_tibble(tsxo1h)


se20<-unlist(lapply(1:length(mod$effekt),function(i){res<-seq(i+1,i+3,1)}))
tsx20<-mod$effekt[se20]-rep(mod$effekt,each=3)

breaksx20 = seq(-1, 1, by=0.01) 
tsx20.cut = cut(tsx20, breaksx20, right=FALSE) 
tsx20.freq = table(tsx20.cut)
tsx3h<-data.frame(breaksx20[2:length(breaksx20)], tsx20.freq)
names(tsx3h)<-c("CFchange","bins","Frequency")
rc3h<-as_tibble(tsx3h)

se22<-unlist(lapply(1:length(mod2$effekt),function(i){res<-seq(i+1,i+3,1)}))
tsx22<-mod2$effekt[se22]-rep(mod2$effekt,each=3)

breaksx22 = seq(-1, 1, by=0.01) 
tsx2.cut2 = cut(tsx22, breaksx22, right=FALSE) 
tsx2.freq2 = table(tsx2.cut2)
tsx3h2<-data.frame(breaksx22[2:length(breaksx22)], tsx2.freq2)
names(tsx3h2)<-c("CFchange","bins","Frequency")
rc3h2<-as_tibble(tsx3h2)

se23<-unlist(lapply(1:length(mod3$effekt),function(i){res<-seq(i+1,i+3,1)}))
tsx23<-mod3$effekt[se23]-rep(mod3$effekt,each=3)

breaksx23 = seq(-1, 1, by=0.01) 
tsx2.cut3 = cut(tsx23, breaksx23, right=FALSE) 
tsx2.freq3 = table(tsx2.cut3)
tsx3h3<-data.frame(breaksx23[2:length(breaksx23)], tsx2.freq3)
names(tsx3h3)<-c("CFchange","bins","Frequency")
rc3h3<-as_tibble(tsx3h3)

se2o<-unlist(lapply(1:length(obs$effekt),function(i){res<-seq(i+1,i+3,1)}))
tsx2o<-obs$effekt[se2o]-rep(obs$effekt,each=3)
breaksx2o = seq(-1, 1, by=0.01) 
tsx2o.cut = cut(tsx2o, breaksx2o, right=FALSE) 
tsx2o.freq = table(tsx2o.cut)
tsxo3h<-data.frame(breaksx2o[2:length(breaksx2o)], tsx2o.freq)
names(tsxo3h)<-c("CFchange","bins","Frequency")
rco3h<-as_tibble(tsxo3h)


se30<-unlist(lapply(1:length(mod$effekt),function(i){res<-seq(i+1,i+6,1)}))
tsx30<-mod$effekt[se30]-rep(mod$effekt,each=6)

breaksx30 = seq(-1, 1, by=0.01) 
tsx30.cut = cut(tsx30, breaksx30, right=FALSE) 
tsx30.freq = table(tsx30.cut)
tsx6h<-data.frame(breaksx30[2:length(breaksx30)], tsx30.freq)
names(tsx6h)<-c("CFchange","bins","Frequency")
rc6h<-as_tibble(tsx6h)

se32<-unlist(lapply(1:length(mod2$effekt),function(i){res<-seq(i+1,i+6,1)}))
tsx32<-mod2$effekt[se32]-rep(mod2$effekt,each=6)

breaksx32 = seq(-1, 1, by=0.01) 
tsx3.cut2 = cut(tsx32, breaksx32, right=FALSE) 
tsx3.freq2 = table(tsx3.cut2)
tsx6h2<-data.frame(breaksx32[2:length(breaksx32)], tsx3.freq2)
names(tsx6h2)<-c("CFchange","bins","Frequency")
rc6h2<-as_tibble(tsx6h2)

se33<-unlist(lapply(1:length(mod3$effekt),function(i){res<-seq(i+1,i+6,1)}))
tsx33<-mod3$effekt[se33]-rep(mod3$effekt,each=6)

breaksx33 = seq(-1, 1, by=0.01) 
tsx3.cut3 = cut(tsx33, breaksx33, right=FALSE) 
tsx3.freq3 = table(tsx3.cut3)
tsx6h3<-data.frame(breaksx33[2:length(breaksx33)], tsx3.freq3)
names(tsx6h3)<-c("CFchange","bins","Frequency")
rc6h3<-as_tibble(tsx6h3)

se3o<<-unlist(lapply(1:length(obs$effekt),function(i){res<-seq(i+1,i+6,1)}))
tsx3o<<-obs$effekt[se3o]-rep(obs$effekt,each=6)

breaksx3o = seq(-1, 1, by=0.01) 
tsx3o.cut = cut(tsx3o, breaksx3o, right=FALSE) 
tsx3o.freq = table(tsx3o.cut)
tsxo6h<-data.frame(breaksx3o[2:length(breaksx3o)], tsx3o.freq)
names(tsxo6h)<-c("CFchange","bins","Frequency")
rco6h<-as_tibble(tsxo6h)


ts1o<-data.frame(tsx1o,rep("1h",length(tsx1o)),rep("Observations",length(tsx1o)))
names(ts1o)<-c("roc","tf","type")

ts1m<-data.frame(tsx1,rep("1h",length(tsx1)),rep("RN",length(tsx1)))
names(ts1m)<-c("roc","tf","type")

ts1m2<-data.frame(tsx12,rep("1h",length(tsx12)),rep("MLM1",length(tsx12)))
names(ts1m2)<-c("roc","tf","type")

ts1m3<-data.frame(tsx13,rep("1h",length(tsx13)),rep("MLM2",length(tsx13)))
names(ts1m3)<-c("roc","tf","type")

ts3o<-data.frame(tsx2o,rep("3h",length(tsx2o)),rep("Observations",length(tsx2o)))
names(ts3o)<-c("roc","tf","type")

ts3m<-data.frame(tsx20,rep("3h",length(tsx20)),rep("RN",length(tsx20)))
names(ts3m)<-c("roc","tf","type")

ts3m2<-data.frame(tsx22,rep("3h",length(tsx22)),rep("MLM1",length(tsx22)))
names(ts3m2)<-c("roc","tf","type")

ts3m3<-data.frame(tsx23,rep("3h",length(tsx23)),rep("MLM2",length(tsx23)))
names(ts3m3)<-c("roc","tf","type")

ts6o<-data.frame(tsx3o,rep("6h",length(tsx3o)),rep("Observations",length(tsx3o)))
names(ts6o)<-c("roc","tf","type")

ts6m<-data.frame(tsx30,rep("6h",length(tsx30)),rep("RN",length(tsx30)))
names(ts6m)<-c("roc","tf","type")

ts6m2<-data.frame(tsx32,rep("6h",length(tsx32)),rep("MLM1",length(tsx32)))
names(ts6m2)<-c("roc","tf","type")

ts6m3<-data.frame(tsx33,rep("6h",length(tsx33)),rep("MLM2",length(tsx33)))
names(ts6m3)<-c("roc","tf","type")

ts12o<-data.frame(tsxo,rep("12h",length(tsxo)),rep("Observations",length(tsxo)))
names(ts12o)<-c("roc","tf","type")

ts12m<-data.frame(tsx,rep("12h",length(tsx)),rep("RN",length(tsx)))
names(ts12m)<-c("roc","tf","type")

ts12m2<-data.frame(tsx2,rep("12h",length(tsx2)),rep("MLM1",length(tsx2)))
names(ts12m2)<-c("roc","tf","type")

ts12m3<-data.frame(tsx3,rep("12h",length(tsx3)),rep("MLM2",length(tsx3)))
names(ts12m3)<-c("roc","tf","type")

tst<-as_tibble(rbind(ts1o,ts3o,ts6o,ts12o,ts1m,ts3m,ts6m,ts12m,ts1m2,ts3m2,ts6m2,ts12m2,ts1m3,ts3m3,ts6m3,ts12m3))
tst$type<-factor(as.character(tst$type), levels=c("RN","MLM1","MLM2","Observations"))

proc<-ggplot(data = tst, aes(x=tf, y=roc)) +
  geom_boxplot(aes(fill=type)) +
  scale_fill_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220")) +
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  xlab("Timeframe")+
  ylab("CF Change")
  

proc

png(paste("",getwd(),"/proc.png",sep=""), , width = 1000, height = 750)
proc
dev.off()

proc2<-ggplot(data = tst, aes(x=tf, y=roc)) +
  geom_boxplot(aes(fill=type)) +
  scale_fill_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220")) +
  facet_grid(tf~.)+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  xlab("Timeframe")+
  ylab("CF Change")
  

proc2

png(paste("",getwd(),"/proc2.png",sep=""), , width = 1000, height = 750)
proc2
dev.off()

ttt<-ggplot(data = tst, aes(x = roc)) +
    geom_density(aes(y = ..count..), fill = "darkgrey")

p6 <- ggplot() +
  stat_ecdf(data=tst,aes(x=roc, color=type),geom="step",size=1.2)+
  theme_classic(base_size = 30)+
  #stat_ecdf(data=roc1h,aes(x=roc2r1h, color="Model"),geom="step")+
  theme(legend.title=element_blank())+
  xlab(paste("1h",paste(unit),"Change"))+
  ylab("Cumulative Density")+
  coord_cartesian(xlim = c(-0.2, 0.2))+
  scale_color_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))

p6  

png(paste("",getwd(),"/p6.png",sep=""),, width = 1000, height = 750)
p6
dev.off()

maxh1mod<-max(tsx1,na.rm=TRUE)
maxh3mod<-max(tsx20,na.rm=TRUE)
maxh6mod<-max(tsx30,na.rm=TRUE)
maxh12mod<-max(tsx,na.rm=TRUE)

maxh1mod2<-max(tsx12,na.rm=TRUE)
maxh3mod2<-max(tsx22,na.rm=TRUE)
maxh6mod2<-max(tsx32,na.rm=TRUE)
maxh12mod2<-max(tsx2,na.rm=TRUE)

maxh1mod3<-max(tsx13,na.rm=TRUE)
maxh3mod3<-max(tsx23,na.rm=TRUE)
maxh6mod3<-max(tsx33,na.rm=TRUE)
maxh12mod3<-max(tsx3,na.rm=TRUE)

maxh1obs<-max(tsx1o,na.rm=TRUE)
maxh3obs<-max(tsx2o,na.rm=TRUE)
maxh6obs<-max(tsx3o,na.rm=TRUE)
maxh12obs<-max(tsxo,na.rm=TRUE)

freh1mod<-length(tsx1[which(tsx1>=0.2)])
freh3mod<-length(tsx20[which(tsx20>=0.2)])
freh6mod<-length(tsx30[which(tsx30>=0.2)])
freh12mod<-length(tsx[which(tsx>=0.2)])

freh1mod2<-length(tsx12[which(tsx12>=0.2)])
freh3mod2<-length(tsx22[which(tsx22>=0.2)])
freh6mod2<-length(tsx32[which(tsx32>=0.2)])
freh12mod2<-length(tsx2[which(tsx2>=0.2)])

freh1mod3<-length(tsx13[which(tsx13>=0.2)])
freh3mod3<-length(tsx23[which(tsx23>=0.2)])
freh6mod3<-length(tsx33[which(tsx33>=0.2)])
freh12mod3<-length(tsx3[which(tsx3>=0.2)])

freh1obs<-length(tsx1o[which(tsx1o>=0.2)])
freh3obs<-length(tsx2o[which(tsx2o>=0.2)])
freh6obs<-length(tsx3o[which(tsx3o>=0.2)])
freh12obs<-length(tsxo[which(tsxo>=0.2)])


frel1mod<-length(tsx1[which(tsx1<=-0.2)])
frel3mod<-length(tsx20[which(tsx20<=-0.2)])
frel6mod<-length(tsx30[which(tsx30<=-0.2)])
frel12mod<-length(tsx[which(tsx<=-0.2)])

frel1mod2<-length(tsx12[which(tsx12<=-0.2)])
frel3mod2<-length(tsx22[which(tsx22<=-0.2)])
frel6mod2<-length(tsx32[which(tsx32<=-0.2)])
frel12mod2<-length(tsx2[which(tsx2<=-0.2)])

frel1mod3<-length(tsx13[which(tsx13<=-0.2)])
frel3mod3<-length(tsx23[which(tsx23<=-0.2)])
frel6mod3<-length(tsx33[which(tsx33<=-0.2)])
frel12mod3<-length(tsx3[which(tsx3<=-0.2)])

frel1obs<-length(tsx1o[which(tsx1o<=-0.2)])
frel3obs<-length(tsx2o[which(tsx2o<=-0.2)])
frel6obs<-length(tsx3o[which(tsx3o<=-0.2)])
frel12obs<-length(tsxo[which(tsxo<=-0.2)])

minh1mod<-min(tsx1,na.rm=TRUE)
minh3mod<-min(tsx20,na.rm=TRUE)
minh6mod<-min(tsx30,na.rm=TRUE)
minh12mod<-min(tsx,na.rm=TRUE)

minh1mod2<-min(tsx12,na.rm=TRUE)
minh3mod2<-min(tsx22,na.rm=TRUE)
minh6mod2<-min(tsx32,na.rm=TRUE)
minh12mod2<-min(tsx2,na.rm=TRUE)

minh1mod3<-min(tsx13,na.rm=TRUE)
minh3mod3<-min(tsx23,na.rm=TRUE)
minh6mod3<-min(tsx33,na.rm=TRUE)
minh12mod3<-min(tsx3,na.rm=TRUE)

minh1obs<-min(tsx1o,na.rm=TRUE)
minh3obs<-min(tsx2o,na.rm=TRUE)
minh6obs<-min(tsx3o,na.rm=TRUE)
minh12obs<-min(tsxo,na.rm=TRUE)

meanh1mod<-format(mean(tsx1,na.rm=TRUE),scientific=FALSE)
meanh3mod<-format(mean(tsx20,na.rm=TRUE),scientific=FALSE)
meanh6mod<-format(mean(tsx30,na.rm=TRUE),scientific=FALSE)
meanh12mod<-format(mean(tsx,na.rm=TRUE),scientific=FALSE)

nmeanh1mod<-format(mean(tsx1[which(tsx1<0)],na.rm=TRUE),scientific=FALSE)
nmeanh3mod<-format(mean(tsx20[which(tsx20<0)],na.rm=TRUE),scientific=FALSE)
nmeanh6mod<-format(mean(tsx30[which(tsx30<0)],na.rm=TRUE),scientific=FALSE)
nmeanh12mod<-format(mean(tsx[which(tsx<0)],na.rm=TRUE),scientific=FALSE)

pmeanh1mod<-format(mean(tsx1[which(tsx1>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh3mod<-format(mean(tsx20[which(tsx20>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh6mod<-format(mean(tsx30[which(tsx30>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh12mod<-format(mean(tsx[which(tsx>=0)],na.rm=TRUE),scientific=FALSE)

meanh1mod2<-format(mean(tsx12,na.rm=TRUE),scientific=FALSE)
meanh3mod2<-format(mean(tsx22,na.rm=TRUE),scientific=FALSE)
meanh6mod2<-format(mean(tsx32,na.rm=TRUE),scientific=FALSE)
meanh12mod2<-format(mean(tsx2,na.rm=TRUE),scientific=FALSE)

nmeanh1mod2<-format(mean(tsx12[which(tsx12<0)],na.rm=TRUE),scientific=FALSE)
nmeanh3mod2<-format(mean(tsx22[which(tsx22<0)],na.rm=TRUE),scientific=FALSE)
nmeanh6mod2<-format(mean(tsx32[which(tsx32<0)],na.rm=TRUE),scientific=FALSE)
nmeanh12mod2<-format(mean(tsx2[which(tsx2<0)],na.rm=TRUE),scientific=FALSE)

pmeanh1mod2<-format(mean(tsx12[which(tsx12>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh3mod2<-format(mean(tsx22[which(tsx22>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh6mod2<-format(mean(tsx32[which(tsx32>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh12mod2<-format(mean(tsx2[which(tsx2>=0)],na.rm=TRUE),scientific=FALSE)

meanh1mod3<-format(mean(tsx13,na.rm=TRUE),scientific=FALSE)
meanh3mod3<-format(mean(tsx23,na.rm=TRUE),scientific=FALSE)
meanh6mod3<-format(mean(tsx33,na.rm=TRUE),scientific=FALSE)
meanh12mod3<-format(mean(tsx3,na.rm=TRUE),scientific=FALSE)

nmeanh1mod3<-format(mean(tsx13[which(tsx13<0)],na.rm=TRUE),scientific=FALSE)
nmeanh3mod3<-format(mean(tsx23[which(tsx23<0)],na.rm=TRUE),scientific=FALSE)
nmeanh6mod3<-format(mean(tsx33[which(tsx33<0)],na.rm=TRUE),scientific=FALSE)
nmeanh12mod3<-format(mean(tsx3[which(tsx3<0)],na.rm=TRUE),scientific=FALSE)

pmeanh1mod3<-format(mean(tsx13[which(tsx13>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh3mod3<-format(mean(tsx23[which(tsx23>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh6mod3<-format(mean(tsx33[which(tsx33>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh12mod3<-format(mean(tsx3[which(tsx3>=0)],na.rm=TRUE),scientific=FALSE)


meanh1obs<-format(mean(tsx1o,na.rm=TRUE),scientific=FALSE)
meanh3obs<-format(mean(tsx2o,na.rm=TRUE),scientific=FALSE)
meanh6obs<-format(mean(tsx3o,na.rm=TRUE),scientific=FALSE)
meanh12obs<-format(mean(tsxo,na.rm=TRUE),scientific=FALSE)

nmeanh1obs<-format(mean(tsx1o[which(tsx1o<0)],na.rm=TRUE),scientific=FALSE)
nmeanh3obs<-format(mean(tsx2o[which(tsx2o<0)],na.rm=TRUE),scientific=FALSE)
nmeanh6obs<-format(mean(tsx3o[which(tsx3o<0)],na.rm=TRUE),scientific=FALSE)
nmeanh12obs<-format(mean(tsxo[which(tsxo<0)],na.rm=TRUE),scientific=FALSE)

pmeanh1obs<-format(mean(tsx1o[which(tsx1o>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh3obs<-format(mean(tsx2o[which(tsx2o>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh6obs<-format(mean(tsx3o[which(tsx3o>=0)],na.rm=TRUE),scientific=FALSE)
pmeanh12obs<-format(mean(tsxo[which(tsxo>=0)],na.rm=TRUE),scientific=FALSE)

############## frequencies

nfrh1mod<-length(tsx1[which(tsx1<0)])
nfrh3mod<-length(tsx20[which(tsx20<0)])
nfrh6mod<-length(tsx30[which(tsx30<0)])
nfrh12mod<-length(tsx[which(tsx<0)])

pfrh1mod<-length(tsx1[which(tsx1>=0)])
pfrh3mod<-length(tsx20[which(tsx20>=0)])
pfrh6mod<-length(tsx30[which(tsx30>=0)])
pfrh12mod<-length(tsx[which(tsx>=0)])


nfrh1mod2<-length(tsx12[which(tsx12<0)])
nfrh3mod2<-length(tsx22[which(tsx22<0)])
nfrh6mod2<-length(tsx32[which(tsx32<0)])
nfrh12mod2<-length(tsx2[which(tsx2<0)])

pfrh1mod2<-length(tsx12[which(tsx12>=0)])
pfrh3mod2<-length(tsx22[which(tsx22>=0)])
pfrh6mod2<-length(tsx32[which(tsx32>=0)])
pfrh12mod2<-length(tsx2[which(tsx2>=0)])


nfrh1mod3<-length(tsx13[which(tsx13<0)])
nfrh3mod3<-length(tsx23[which(tsx23<0)])
nfrh6mod3<-length(tsx33[which(tsx33<0)])
nfrh12mod3<-length(tsx3[which(tsx3<0)])

pfrh1mod3<-length(tsx13[which(tsx13>=0)])
pfrh3mod3<-length(tsx23[which(tsx23>=0)])
pfrh6mod3<-length(tsx33[which(tsx33>=0)])
pfrh12mod3<-length(tsx3[which(tsx3>=0)])

nfrh1obs<-length(tsx1o[which(tsx1o<0)])
nfrh3obs<-length(tsx2o[which(tsx2o<0)])
nfrh6obs<-length(tsx3o[which(tsx3o<0)])
nfrh12obs<-length(tsxo[which(tsxo<0)])

pfrh1obs<-length(tsx1o[which(tsx1o>=0)])
pfrh3obs<-length(tsx2o[which(tsx2o>=0)])
pfrh6obs<-length(tsx3o[which(tsx3o>=0)])
pfrh12obs<-length(tsxo[which(tsxo>=0)])


#dr<-data.frame(rbind(minh1mod,minh3mod,minh6mod,minh12mod,minh1mod2,minh3mod2,minh6mod2,minh12mod2,minh1mod3,minh3mod3,minh6mod3,minh12mod3,minh1obs,minh3obs,minh6obs,minh12obs),rbind(meanh1mod,meanh3mod,meanh6mod,meanh12mod,meanh1mod2,meanh3mod2,meanh6mod2,meanh12mod2,meanh1mod3,meanh3mod3,meanh6mod3,meanh12mod3,meanh1obs,meanh3obs,meanh6obs,meanh12obs),rbind(maxh1mod,maxh3mod,maxh6mod,maxh12mod,maxh1mod2,maxh3mod2,maxh6mod2,maxh12mod2,maxh1mod3,maxh3mod3,maxh6mod3,maxh12mod3,maxh1obs,maxh3obs,maxh6obs,maxh12obs),rbind(freh1mod,freh3mod,freh6mod,freh12mod,freh1mod2,freh3mod2,freh6mod2,freh12mod2,freh1mod3,freh3mod3,freh6mod3,freh12mod3,freh1obs,freh3obs,freh6obs,freh12obs),rbind(frel1mod,frel3mod,frel6mod,frel12mod,frel1mod2,frel3mod2,frel6mod2,frel12mod2,frel1mod3,frel3mod3,frel6mod3,frel12mod3,frel1obs,frel3obs,frel6obs,frel12obs))
#names(dr)<-c("Min","Mean","Max", "Frequency (> 0.2 ?? CF)", "Frequency (< -0.2 ?? CF)")
#rownames(dr)<-c("1h-m", "3h-m", "6h-m", "12h-m","1h-m2", "3h-m2", "6h-m2", "12h-m2","1h-m3", "3h-m3", "6h-m3", "12h-m3","1h-o","3h-o","6h-o","12h-o")

#dr<-data.frame(rbind(minh1obs,minh1mod,minh1mod2,minh1mod3,minh3obs,minh3mod,minh3mod2,minh3mod3,minh6obs,minh6mod,minh6mod2,minh6mod3,minh12obs,minh12mod,minh12mod2,minh12mod3),rbind(meanh1obs,meanh1mod,meanh1mod2,meanh1mod3,meanh3obs,meanh3mod,meanh3mod2,meanh3mod3,meanh6obs,meanh6mod,meanh6mod2,meanh6mod3,meanh12obs,meanh12mod,meanh12mod2,meanh12mod3),rbind(maxh1obs,maxh1mod,maxh1mod2,maxh1mod3,maxh3obs,maxh3mod,maxh3mod2,maxh3mod3,maxh6obs,maxh6mod,maxh6mod2,maxh6mod3,maxh12obs,maxh12mod,maxh12mod2,maxh12mod3),rbind(freh1obs,freh1mod,freh1mod2,freh1mod3,freh3obs,freh3mod,freh3mod2,freh3mod3,freh6obs,freh6mod,freh6mod2,freh6mod3,freh12obs,freh12mod,freh12mod2,freh12mod3),rbind(frel1obs,frel1mod,frel1mod2,frel1mod3,frel3obs,frel3mod,frel3mod2,frel3mod3,frel6obs,frel6mod,frel6mod2,frel6mod3,frel12obs,frel12mod,frel12mod2,frel12mod3))
#names(dr)<-c("Min","Mean","Max", "Frequency (> 0.2 ?? CF)", "Frequency (< -0.2 ?? CF)")
#rownames(dr)<-c("1h-o","1h-m","1h-m2","1h-m3","3h-o", "3h-m", "3h-m2", "3h-m3","6h-o", "6h-m", "6h-m2", "6h-m3","12h-o", "12h-m", "12h-m2", "12h-m3")


dr<-data.frame(rbind(minh1obs,minh1mod,minh1mod2,minh1mod3,minh3obs,minh3mod,minh3mod2,minh3mod3,minh6obs,minh6mod,minh6mod2,minh6mod3,minh12obs,minh12mod,minh12mod2,minh12mod3),rbind(nmeanh1obs,nmeanh1mod,nmeanh1mod2,nmeanh1mod3,nmeanh3obs,nmeanh3mod,nmeanh3mod2,nmeanh3mod3,nmeanh6obs,nmeanh6mod,nmeanh6mod2,nmeanh6mod3,nmeanh12obs,nmeanh12mod,nmeanh12mod2,nmeanh12mod3),rbind(nfrh1obs,nfrh1mod,nfrh1mod2,nfrh1mod3,nfrh3obs,nfrh3mod,nfrh3mod2,nfrh3mod3,nfrh6obs,nfrh6mod,nfrh6mod2,nfrh6mod3,nfrh12obs,nfrh12mod,nfrh12mod2,nfrh12mod3),rbind(pmeanh1obs,pmeanh1mod,pmeanh1mod2,pmeanh1mod3,pmeanh3obs,pmeanh3mod,pmeanh3mod2,pmeanh3mod3,pmeanh6obs,pmeanh6mod,pmeanh6mod2,pmeanh6mod3,pmeanh12obs,pmeanh12mod,pmeanh12mod2,pmeanh12mod3),rbind(pfrh1obs,pfrh1mod,pfrh1mod2,pfrh1mod3,pfrh3obs,pfrh3mod,pfrh3mod2,pfrh3mod3,pfrh6obs,pfrh6mod,pfrh6mod2,pfrh6mod3,pfrh12obs,pfrh12mod,pfrh12mod2,pfrh12mod3),rbind(maxh1obs,maxh1mod,maxh1mod2,maxh1mod3,maxh3obs,maxh3mod,maxh3mod2,maxh3mod3,maxh6obs,maxh6mod,maxh6mod2,maxh6mod3,maxh12obs,maxh12mod,maxh12mod2,maxh12mod3),rbind(freh1obs,freh1mod,freh1mod2,freh1mod3,freh3obs,freh3mod,freh3mod2,freh3mod3,freh6obs,freh6mod,freh6mod2,freh6mod3,freh12obs,freh12mod,freh12mod2,freh12mod3),rbind(frel1obs,frel1mod,frel1mod2,frel1mod3,frel3obs,frel3mod,frel3mod2,frel3mod3,frel6obs,frel6mod,frel6mod2,frel6mod3,frel12obs,frel12mod,frel12mod2,frel12mod3))
names(dr)<-c("Min","nMean","nfreq","pMean","pfreq","Max", "Frequency (> 0.2 ?? CF)", "Frequency (< -0.2 ?? CF)")
rownames(dr)<-c("1h-o","1h-m","1h-m2","1h-m3","3h-o", "3h-m", "3h-m2", "3h-m3","6h-o", "6h-m", "6h-m2", "6h-m3","12h-o", "12h-m", "12h-m2", "12h-m3")


png(paste("",getwd(),"/dr.png",sep=""), height = 50*nrow(dr), width = 200*ncol(dr))
grid.table(dr)
dev.off()

p3 <- ggplot() +
  geom_histogram(data=tst, aes(x = roc, fill=type), size=1,binwidth = 0.01)+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  facet_grid(tf~.)+
  xlab(paste(unit)) +
  ylab("Frequency") +
  coord_cartesian(xlim = c(-0.35, 0.35))+
  scale_fill_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))

p3  

png(paste("",getwd(),"/p3.png",sep=""), , width = 1000, height = 750)
p3
dev.off()


p3<-ggplot(data = tst, aes(x=tf, y=roc)) + 
  geom_boxplot(aes(fill=type)) +
  theme_classic(base_size = 30)

p3 <- ggplot() +
  geom_boxplot(data=tst, aes(x=tf,y = roc, fill=type), size=1)+
  theme(legend.title=element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit)) +
  ylab("Frequency") +
  coord_cartesian(xlim = c(0, 0.82))+
  

p3  

######extreme value duration comparison
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

lobslength<-rle(filtersx(obs$effekt,lbreaks[2],lbreaks[1]))
spllobs<-cumsum(lobslength$lengths)+1
xtobs<-splitAt(obs$effekt, spllobs)
View(xtobs)

lmodlength<-rle(filtersx(mod$effekt,lbreaks[2],lbreaks[1]))
spllmod<-cumsum(lmodlength$lengths)+1
xtmod<-splitAt(mod$effekt, spllmod)
View(xtmod)

lmod2length<-rle(filtersx(mod2$effekt,lbreaks[2],lbreaks[1]))
spllmod2<-cumsum(lmod2length$lengths)+1
xtmod2<-splitAt(mod2$effekt, spllmod2)
View(xtmod2)

lmod3length<-rle(filtersx(mod3$effekt,lbreaks[2],lbreaks[1]))
spllmod3<-cumsum(lmod3length$lengths)+1
xtmod3<-splitAt(mod3$effekt, spllmod3)
View(xtmod3)

hobslength<-rle(filtersx(obs$effekt,hthres,hbreaks[2]))
splhobs<-cumsum(hobslength$lengths)+1
xthobs<-splitAt(obs$effekt, splhobs)
View(xthobs)

hmodlength<-rle(filtersx(mod$effekt,hthres,hbreaks[2]))
splhmod<-cumsum(hmodlength$lengths)+1
xthmod<-splitAt(mod$effekt, splhmod)
View(xthmod)

hmod2length<-rle(filtersx(mod2$effekt,hthres,hbreaks[2]))
splhmod2<-cumsum(hmod2length$lengths)+1
xthmod2<-splitAt(mod2$effekt, splhmod2)
View(xthmod2)

hmod3length<-rle(filtersx(mod3$effekt,hthres,lbreaks[1]))
splhmod3<-cumsum(hmod3length$lengths)+1
xthmod3<-splitAt(mod3$effekt, splhmod3)
View(xthmod3)

17520

obsy1<-obs[1:8760,]
mody1<-mod[1:8760,]
mod2y1<-mod2[1:8760,]
mod3y1<-mod3[1:8760,]

obsy2<-obs[8761:17520,]
mody2<-mod[8761:17520,]
mod2y2<-mod2[8761:17520,]
mod3y2<-mod3[8761:17520,]

obsy3<-obs[17521:26304,]
mody3<-mod[17521:26304,]
mod2y3<-mod2[17521:26304,]
mod3y3<-mod3[17521:26304,]




p20 <- ggplot() +
  geom_point(data=obsy2[which(obsy2$effekt<0.005),],aes(x=date,y=effekt, color="Observations"))+
  geom_point(data=mody2[which(mody2$effekt<0.005),],aes(x=date,y=effekt, color="RN"))+
  geom_point(data=mod2y2[which(mod2y2$effekt<0.005),],aes(x=date,y=effekt, color="MLM1"))+
  geom_point(data=mod3y2[which(mod3y2$effekt<0.005),],aes(x=date,y=effekt, color="MLM2"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  xlab("Generation Hour")+
  ylab(paste(unit))+
  scale_color_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))

p20  

mean(diff(which(obs$effekt<0.005)))
mean(diff(which(mod$effekt<0.005)))
mean(diff(which(mod2$effekt<0.005)))
mean(diff(which(mod3$effekt<0.005)))

kurtosis(which(obs$effekt<0.005))
kurtosis(which(mod$effekt<0.005))
kurtosis(which(mod2$effekt<0.005))
kurtosis(which(mod3$effekt<0.005))

kurtosis(diff(which(obs$effekt<0.005)))
kurtosis(diff(which(mod$effekt<0.005)))
kurtosis(diff(which(mod2$effekt<0.005)))
kurtosis(diff(which(mod3$effekt<0.005)))

summary(diff(which(obs$effekt<0.005)))
summary(diff(which(mod$effekt<0.005)))
summary(diff(which(mod2$effekt<0.005)))
summary(diff(which(mod3$effekt<0.005)))


differs<-data.frame(diff(which(obs$effekt<0.005)),diff(which(mod$effekt<0.005)),diff(which(mod2$effekt<0.005)),diff(which(mod3$effekt<0.005)))

p20h <- ggplot() +
  geom_histogram(data=dctddf,aes(x=effekt1, fill="Observations"),alpha=0.5,position="identity", binwidth=0.01)+
  geom_histogram(data=dctddf,aes(x=effekt2, fill=type),alpha=0.5,position="identity", binwidth=0.01)+
  scale_fill_manual(values=c("Observations"="darkgrey", "RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))+
  theme_classic(base_size=30)+
  theme(legend.title = element_blank())+
  facet_grid(type~., scales = "free")+
  xlab(paste(unit))+
  ylab("Frequency") 

p20h


p20 <- ggplot() +
  geom_point(data=obs[which(obs$effekt[1:8760]<0.005),],aes(x=date,y=effekt, color="Observations"))+
  geom_point(data=mod[which(mod$effekt[1:8760]<0.005),],aes(x=date,y=effekt, color="RN"))+
  geom_point(data=mod2[which(mod2$effekt[1:8760]<0.005),],aes(x=date,y=effekt, color="MLM1"))+
  geom_point(data=mod3[which(mod3$effekt[1:8760]<0.005),],aes(x=date,y=effekt, color="MLM2"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  xlab("Generation Hour")+
  ylab(paste(unit))+
  scale_color_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))

p20  

png(paste("",getwd(),"/p20.png",sep=""), , width = 1000, height = 750)
p20
dev.off()

p20x <- ggplot() +
  geom_point(data=obs[which(obs$effekt>0.005 & obs$effekt<0.01),],aes(x=date,y=effekt, color="Observations"))+
  geom_point(data=mod[which(mod$effekt>0.005 & mod$effekt<0.01),],aes(x=date,y=effekt, color="RN"))+
  geom_point(data=mod2[which(mod2$effekt>0.005 & mod2$effekt<0.01),],aes(x=date,y=effekt, color="MLM1"))+
  geom_point(data=mod3[which(mod3$effekt>0.005 & mod3$effekt<0.01),],aes(x=date,y=effekt, color="MLM2"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  xlab("Generation Hour")+
  ylab(paste(unit))+
  scale_color_manual(values=c("Observations"="darkgrey","RN"="#c72321","MLM1"="#0d8085","MLM2"="#efc220"))

p20x  

png(paste("",getwd(),"/p20x.png",sep=""), , width = 1000, height = 750)
p20x
dev.off()


errallseasm<- errallseas %>%
     group_by(class, type, seas) %>% 
     summarise(errmean=mean(abs(err)), errrange=abs(min(err))+abs(max(err)))

write.xlsx(errallseasm,"errallseasm.xlsx")


######## model ext values
t3<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelmlm2x206.rds")

View(t3$finalModel$xNames)

datestest<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/datasfmlm2final2.rds")[1:24,3:45]

cdtest<-matrix(rep(0,len=(length(t3$finalModel$xNames)-43)*24),nrow=24)

datasftst<-data.frame(datestest,cdtest)

tstpred<-predict(t3,datasftst)
  
tstpreddesc<- tstpred*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst<-data.frame(datasf$date[1:24],tstpreddesc)
names(mod3tst)<-c("date","effekt")

Sys.setlocale("LC_TIME", "English") 


png(paste("",getwd(),"/pstst.png",sep=""), , width = 1000, height = 750)
pstst
dev.off()

#####1

cdtest2<-matrix(rep(1,len=(length(t3$finalModel$xNames)-43)*24),nrow=24)

datasftst2<-data.frame(datestest,cdtest2)

tstpred2<-predict(t3,datasftst2)
  
tstpreddesc2<- tstpred2*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst2<-data.frame(datasf$date[1:24],tstpreddesc2)
names(mod3tst2)<-c("date","effekt")

#######99

cdtest3<-matrix(rep(99,len=(length(t3$finalModel$xNames)-43)*24),nrow=24)

datasftst3<-data.frame(datestest,cdtest3)

tstpred3<-predict(t3,datasftst3)
  
tstpreddesc3<- tstpred3*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst3<-data.frame(datasf$date[1:24],tstpreddesc3)
names(mod3tst3)<-c("date","effekt")

#### random

cdtest4 <- matrix(data = runif((length(t3$finalModel$xNames)-43)*24), nrow = 24)

datasftst4<-data.frame(datestest,cdtest4)

tstpred4<-predict(t3,datasftst4)
  
tstpreddesc4<- tstpred4*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst4<-data.frame(datasf$date[1:24],tstpreddesc4)
names(mod3tst4)<-c("date","effekt")

library(scales)

pstst2 <- ggplot() +
  geom_line(data=mod3[1:24,],aes(x=date,y=effekt, color="MLM2"),size=2)+
  geom_line(data=mod3tst,aes(x=date,y=effekt, color="MLM2min"),size=2)+
  geom_line(data=mod3tst2,aes(x=date,y=effekt, color="MLM2max"),size=2)+
  #geom_line(data=mod3tst3,aes(x=date,y=effekt, color="MLM2max"),size=2)+
  geom_line(data=mod3tst4,aes(x=date,y=effekt, color="MLM2rand"),size=2)+
  xlab("Generation Hour")+
  ylab(paste("CF"))+
  scale_x_datetime(labels = date_format("%H:%M"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())+
  scale_color_manual(values=c("MLM2"="#efc220",
                              "MLM2min"="#ba9f7c",
                              "MLM2max"="#7a6952",
                              "MLM2rand"="#dfbf99"))

pstst2 

png(paste("",getwd(),"/pstst2.png",sep=""), width = 1000, height = 750)
pstst2
dev.off()

######################
######## model ext values range
t3<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelmlm2x206.rds")

View(t3$finalModel$xNames)

datestest<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/datasfmlm2final2.rds")[12,3:45]

cdtestr<-data.frame(matrix(rep(seq(0,1,0.1),each=(length(t3$finalModel$xNames)-43)),nrow=11,byrow=TRUE))
colnames(cdtestr)<-paste("X",seq(1,length(t3$finalModel$xNames)-43),sep="")

View(cdtestr)

predrange<-lapply(1:NROW(cdtestr),function(i){
  
datasftst<-data.frame(datestest,cdtestr[i,])

tstpred<-predict(t3,datasftst)
  
tstpreddesc<- tstpred*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst<-data.frame(datasf$date[12],tstpreddesc)
names(mod3tst)<-c("date","effekt")
return(mod3tst)
})

predrangedf<-data.frame(ldply(predrange),seq(0,1,0.1))
names(predrangedf)[3]<-c("Inp")

Sys.setlocale("LC_TIME", "English") 

library(scales)

pstst2x <- ggplot() +
  geom_line(data=predrangedf,aes(x=Inp,y=effekt),size=2,colour="#efc220")+
  xlab("Input Value")+
  ylab(paste("CF"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())

pstst2x 

png(paste("",getwd(),"/pstst2x.png",sep=""), width = 1000, height = 750)
pstst2x
dev.off()

######## model ext values range 2
t3<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelmlm2x206.rds")

View(t3$finalModel$xNames)

datestest<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/datasfmlm2final2.rds")[1:8760,3:45]

cdtestr2<-lapply(1:length(seq(0,1,0.1)),function(i){
  
  cdtestr2<-data.frame(matrix(rep(rep(seq(0,1,0.1)[i],each=(length(t3$finalModel$xNames)-43)),8760),nrow=8760,byrow=TRUE))
  colnames(cdtestr2)<-paste("X",seq(1,length(t3$finalModel$xNames)-43),sep="")
  return(cdtestr2)
 
  
})
  
View(cdtestr2)

datestest2<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/datasfmlm2final2.rds")[1:8760,3:45]

predrange2<-lapply(1:length(cdtestr2),function(i){
  
datasftst2<-data.frame(datestest2,cdtestr2[[i]])

tstpred2<-predict(t3,datasftst2)
  
tstpreddesc2<- tstpred2*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

mod3tst2<-data.frame(datasf$date[1:8760],tstpreddesc2)
names(mod3tst2)<-c("date","effekt")
return(mod3tst2)
})

predrangedf2<-data.frame(ldply(predrange2),rep(seq(0,1,0.1),each=8760))
names(predrangedf2)[3]<-c("inp")

predrangedf2x<-predrangedf2[1:8760,]

Sys.setlocale("LC_TIME", "English") 

library(scales)

pstst3x <- ggplot() +
  geom_line(data=predrangedf2x,aes(x=date,y=effekt,col=inp),size=1)+
  #facet_grid(.~Inp, scales = "free")+
  xlab("Date")+
  ylab(paste("CF"))+
  theme_classic(base_size = 30)+
  theme(legend.title=element_blank())

pstst3x 

png(paste("",getwd(),"/pstst2x.png",sep=""), width = 1000, height = 750)
pstst2x
dev.off()

############### mlm2 80 training overfitting? 

ofpred<-predict(t3,t3$trainingData[2:NCOL(t3$trainingData)])

ofpreddesc<- ofpred*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx<-t3$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx,ofpreddesc)

mlm280sqer<-(sqrt(mean((ofpreddesc-wtx)^2))/mean(wtx))

mlm280mner<-(mean(abs(ofpreddesc-wtx))/mean(wtx))

#### mlm2 60 training

t32<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm2model206_60.rds")

ofpred2<-predict(t32,t32$trainingData[2:NCOL(t32$trainingData)])

ofpreddesc2<- ofpred2*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx2<-t32$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx2,ofpreddesc2)

mlm260sqer<-(sqrt(mean((ofpreddesc2-wtx2)^2))/mean(wtx2))

mlm260mner<-(mean(abs(ofpreddesc2-wtx2))/mean(wtx2))

rm(t32)

# mlm1 80 training


t33<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm1model80.rds")

ofpred3<-predict(t33,t33$trainingData[2:NCOL(t33$trainingData)])

ofpreddesc3<- ofpred3*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx3<-t33$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx3,ofpreddesc3)

mlm180sqer<-(sqrt(mean((ofpreddesc3-wtx3)^2))/mean(wtx3))

mlm180mner<-(mean(abs(ofpreddesc3-wtx3))/mean(wtx3))

rm(t33)

# mlm1 60 training

t34<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm1model_60.rds")

ofpred4<-predict(t34,t34$trainingData[2:NCOL(t34$trainingData)])

ofpreddesc4<- ofpred4*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx4<-t34$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx4,ofpreddesc4)

mlm160sqer<-(sqrt(mean((ofpreddesc4-wtx4)^2))/mean(wtx4))

mlm160mner<-(mean(abs(ofpreddesc4-wtx4))/mean(wtx4))

rm(t34)

# mlm3 80 training

t35<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm3model_80.rds")

ofpred5<-predict(t35,t35$trainingData[2:NCOL(t35$trainingData)])

ofpreddesc5<- ofpred5*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx5<-t35$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx5,ofpreddesc5)

mlm380sqer<-(sqrt(mean((ofpreddesc5-wtx5)^2))/mean(wtx5))

mlm380mner<-(mean(abs(ofpreddesc5-wtx5))/mean(wtx5))

rm(t35)

# mlm3 60 training

t36<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm3model_60n.rds")

ofpred6<-predict(t36,t36$trainingData[2:NCOL(t36$trainingData)])

ofpreddesc6<- ofpred6*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

wtx6<-t36$trainingData$.outcome*(max(obs$effekt)-min(obs$effekt))+min(obs$effekt)

cor(wtx6,ofpreddesc6)

mlm360sqer<-(sqrt(mean((ofpreddesc6-wtx6)^2))/mean(wtx6))

mlm360mner<-(mean(abs(ofpreddesc6-wtx6))/mean(wtx6))


#mlm1 80 pred

ts33<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm1ts80.rds")

mlm180sqerp<-(sqrt(mean((unlist(ts33)-obs$effekt)^2))/mean(obs$effekt))

mlm180mnerp<-(mean(abs(unlist(ts33)-obs$effekt))/mean(obs$effekt))

#mlm1 60 pred

ts34<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm1ts_60.rds")

mlm160sqerp<-(sqrt(mean((unlist(ts34)-obs$effekt)^2))/mean(obs$effekt))

mlm160mnerp<-(mean(abs(unlist(ts34)-obs$effekt))/mean(obs$effekt))

#mlm2 80 pred

ts3<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm2x206ts.rds")

mlm280sqerp<-(sqrt(mean((unlist(ts3)-obs$effekt)^2))/mean(obs$effekt))

mlm280mnerp<-(mean(abs(unlist(ts3)-obs$effekt))/mean(obs$effekt))

#mlm2 60 pred

ts32<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm2ts206_60.rds")

mlm260sqerp<-(sqrt(mean((unlist(ts32)-obs$effekt)^2))/mean(obs$effekt))

mlm260mnerp<-(mean(abs(unlist(ts32)-obs$effekt))/mean(obs$effekt))

#mlm3 80 pred

ts35<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm3ts_80.rds")

mlm380sqerp<-(sqrt(mean((unlist(ts35)-obs$effekt)^2))/mean(obs$effekt))

mlm380mnerp<-(mean(abs(unlist(ts35)-obs$effekt))/mean(obs$effekt))

#mlm3 60 pred

ts36<-readRDS("C:/Users/Johann Baumgartner/Desktop/Uni/Doktorat/Paper 1/modelsresults/mlm3ts_60n.rds")

mlm360sqerp<-(sqrt(mean((unlist(ts36)-obs$effekt)^2))/mean(obs$effekt))

mlm360mnerp<-(mean(abs(unlist(ts36)-obs$effekt))/mean(obs$effekt))

#

mltp<-data.frame(rbind(mlm160mner,mlm160sqer,mlm160mnerp,mlm160sqerp,mlm180mner,mlm180sqer,mlm180mnerp,mlm180sqerp),rbind(mlm260mner,mlm260sqer,mlm260mnerp,mlm260sqerp,mlm280mner,mlm280sqer,mlm280mnerp,mlm280sqerp),rbind(mlm360mner,mlm360sqer,mlm360mnerp,mlm360sqerp,mlm380mner,mlm380sqer,mlm380mnerp,mlm380sqerp))
rownames(mltp)<-c("NMAE_60ntrain","NRMSE_60ntrain","NMAE_60npred","NRMSE_60npred","NMAE_80ntrain","NRMSE_80ntrain","NMAE_80npred","NRMSE_80npred")
colnames(mltp)<-c("MLM1","MLM2","MLM3")

png(paste("",getwd(),"/mltp.png",sep=""), height = 50*nrow(mltp), width = 200*ncol(mltp))
grid.table(mltp)
dev.off()
