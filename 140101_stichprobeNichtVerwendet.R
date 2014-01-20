                                    ##############################
                                    #Stichprobenziehung Zeitwerte#
                                    #V2 20.12.13                 #
                                    #Gordon Wiegand              #
                                    ############################## 

#Change log
#############################################
#V2 PSt Gebiete als Auswahl eingefügt
#############################################



#Hinweise
#############################################
#Was gegebenenfalls noch anzupassen ist
# -> xlsx Datei als CSV ohne Zeilen ohne Daten als zeitwerte.csv exportieren
# -> Pfad anpassen          
#############################################


#Datei lesen
#############################################
rm(list=ls())
setwd("/Volumes/hd2/131216_Zeitwerte")
d <- as.data.frame(read.csv("zeitwerte.csv"),stringsAsFactors = F)
dim(d) # Falls Dimensionen der Matrix nicht plausibel sind
# d <- read.csv2("zeitwerte.csv")
d0 <- apply(d,1:2,as.character) #aus Faktoren Strings machen
d0<-as.data.frame(d0,stringsAsFactors = FALSE)
d0[d0==""] <- NA  #Fehlende Werte als missing
#############################################


#Kontrolle 1
#############################################
print(paste(c("Zeilen:","Spalten:"),dim(d0)))
print(paste("Gültig FAD:", sum(!is.na(d0[,22]))))
print(paste("Gültig B/P:", sum(!is.na(d0[,23]))))
print(paste("Gültig PFZ:", sum(!is.na(d0[,24]))))
#############################################


#Ziehung Poststellengebiete
#############################################
dPg <- as.data.frame(cbind(table(d0$PG_ABK),names(table(d0$PG_ABK))),stringsAsFactors=F)
dPg[,1] <- as.numeric(dPg[,1])
samPg <- sample(dPg[,2],round(nrow(dPg)/3),prob=dPg[,1]/sum(dPg[,1]))
for (i in 1:length(samPg)){
    if (i==1) {d1 <- d0[d0$PG_ABK==samPg[i],]} 
    else {d1 <- rbind(d1,d0[d0$PG_ABK==samPg[i],])}
} 
#############################################


#Ziehung FAD_FO Spezialfall A-D mit F
#############################################
dh <- d0[(!is.na(d0[,22]) & d0[,22]=="A-Dmit Führung"),] #Spezialfall A-D mit Führung
samFad <- sample(dh[,1],3)
############################################# 


#Ziehung FAD_FO
#############################################
fadTab <- table(d0[,22]) #Zellen bilden
for (i in 2:length(fadTab)) {    #Ziehen und in sam schreiben // 2 weil 1 == A bis D mit F
    dh <- d1[(!is.na(d1[,22]) & d1[,22]==names(fadTab)[i]),]
    samFad <- c(samFad,sample(dh[,1],round(fadTab[i]*.1)))
}
#############################################


#Ziehung BV
#############################################
d1$flagSamFad <- F 
rownames(d1) <- d1$SAP_ID
d1[samFad,]$flagSamFad <- T #Bereits gezogene markieren
d1$flagSamFad[(!is.na(d1[,22]) & d1[,22]=="A-Dmit Führung")] <- T # A-D mit F als gezogen
d1$flagFadBv <- F #Flag gehört zu Bv
d1$flagFadBv[!is.na(d1[,22]) & !is.na(d1[,23])] <- T
d1$wBv <- 1 ##korrigierte Einschlusswahrscheinlichkeiten
d2 <- d1[!d1$flagSamFad,] #Sample FAD raus
d2$wBv[d2$flagFadBv] <- (sum(d1$flagFadBv)/sum(d2$flagFadBv))/nrow(d2)
d2$wBv[!d2$flagFadBv] <- (sum(!d1$flagFadBv)/sum(!d2$flagFadBv))/nrow(d2)
samBv <- numeric()
bvTab <- table(d0[,23]) #Zellen bilden
for (i in 1:length(bvTab)) {    #Ziehen und in samBv schreiben
dh <- d2[(!is.na(d2[,23]) & d2[,23]==names(bvTab)[i]),]
samBv <- c(samBv,sample(dh[,1],round(bvTab[i]*.05),prob=dh$wBv))
}
#############################################


#Ziehung PZ
#############################################
d1[samBv,]$flagSamFad <- T #Bereits gezogene markieren
d1$flagFadPz <- F #Flag gehört zu PZ
d1$flagFadPz[(!is.na(d1[,22]) & !is.na(d1[,24])) | (!is.na(d1[,23]) & !is.na(d1[,24]))] <- T
d1$wPz <- 1 #korrigierte Einschlusswahrscheinlichkeiten
d3 <- d1[!d1$flagSamFad,] #schon gezogene raus
d3$wPz[d3$flagFadPz] <- (sum(d1$flagFadPz)/sum(d3$flagFadPz))/nrow(d3)
d3$wPz[!d3$flagFadPz] <- (sum(!d1$flagFadPz)/sum(!d3$flagFadPz))/nrow(d3)
samPz <- numeric()
pzTab <- table(d0[,24]) #Zellen bilden
for (i in 1:length(pzTab)) {    #Ziehen und in samPz schreiben
    dh <- d3[(!is.na(d3[,24]) & d3[,24]==names(pzTab)[i]),]
    samPz <- c(samPz,sample(dh[,1],round(pzTab[i]*.05),prob=dh$wPz))
}
#############################################


#Kontrolle 2 -> Srichprobengrösse
#############################################
nFad <- round(sum(!is.na(d0[,22]))*.1)
nBv <- round(sum(!is.na(d0[,23]))*.05)
nPz <- round(sum(!is.na(d0[,24]))*.05)
print(paste("Fad Soll: ",nFad,"   Ist: ",nrow(outFad),sep=""))
print(paste("Bv Soll: ",nBv,"   Ist: ",nrow(outBv),sep=""))
print(paste("Pz Soll: ",nPz,"   Ist: ",nrow(outPz),sep=""))
#############################################


#Output und fertig
#############################################
d1$flagSamFad <- NULL   #Hilfsvariablen raus
d1$flagFadBv <- NULL
d1$wBv <- NULL
d1$flagFadPz <- NULL
d1$wPz <- NULL
outFad <- d1[samFad,]   #Datensätze erzeugen
outBv <- d1[samBv,]
outPz <- d1[samPz,]
write.csv2(outFad,"PoststellenFad.csv",fileEncoding="CP1252")   #Daten schreiben (drei Dateien)
write.csv2(outBv,"PoststellenBv.csv",fileEncoding="CP1252")
write.csv2(outPz,"PoststellenPz.csv",fileEncoding="CP1252")
#############################################
