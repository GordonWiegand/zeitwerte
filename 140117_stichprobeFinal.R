                                    ##############################
                                    #Stichprobenziehung Zeitwerte#
                                    #V2 20.12.13                 #
                                    #Gordon Wiegand              #
                                    ############################## 

#Change log
#############################################
#V2 PSt Gebiete als Auswahl eingefügt
#Stichprobenyiehung komplett umgestellt = typen FAD BZ PV nur je PG
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
set.seed(99)
setwd("/Volumes/hd2/131216_Zeitwerte")
d <- as.data.frame(read.csv("zeitwerte.csv"),stringsAsFactors = F)
dim(d) # Falls Dimensionen der Matrix nicht plausibel sind
# d <- read.csv2("zeitwerte.csv")
d0 <- apply(d,1:2,as.character) #aus Faktoren Strings machen
d0<-as.data.frame(d0,stringsAsFactors = FALSE)
d0[d0==""] <- NA  #Fehlende Werte als missing
d0$Schicht.Postfachzustellung[d0$Schicht.Postfachzustellung==0] <- NA
d0$Schicht.Brief..Paketversand[d0$Schicht.Brief..Paketversand==0] <- NA
#############################################


#Ziehung Poststellengebiete
#############################################
dPg <- as.data.frame(cbind(table(d0$PG_ABK),names(table(d0$PG_ABK))),stringsAsFactors=F)
dPg[,1] <- as.numeric(dPg[,1])
samPgFad <- sample(dPg[,2],30,prob=dPg[,1]/sum(dPg[,1]))
#Fad
for (i in 1:length(samPgFad)){
if (i==1) {d1Fad <- d0[d0$PG_ABK==samPgFad[i],]} 
else {d1Fad <- rbind(d1Fad,d0[d0$PG_ABK==samPgFad[i],])}
} 

#BV
dPg <- dPg[!(dPg[,2] %in% samPgFad),]
samPgBv <- sample(dPg[,2],15,prob=dPg[,1]/sum(dPg[,1]))
for (i in 1:length(samPgBv)){
    if (i==1) {d1Bv <- d0[d0$PG_ABK==samPgBv[i],]} 
    else {d1Bv <- rbind(d1Bv,d0[d0$PG_ABK==samPgBv[i],])}
} 
#Pz
dPg <- dPg[!(dPg[,2] %in% samPgBv),]
samPgPz <- sample(dPg[,2],15,prob=dPg[,1]/sum(dPg[,1]))
for (i in 1:length(samPgPz)){
    if (i==1) {d1Pz <- d0[d0$PG_ABK==samPgPz[i],]} 
    else {d1Pz <- rbind(d1Pz,d0[d0$PG_ABK==samPgPz[i],])}
} 
#############################################


#Ziehung FAD_FO Spezialfall A-D mit F
#############################################
dh <- d0[(!is.na(d0[,37]) & d0[,37]=="A-Dmit Führung"),] #Spezialfall A-D mit Führung
samTotFadFo <- sample(dh[,1],3)
samTot <- samTotFadFo
############################################# 


#Ziehung FAD_FO
#############################################
tabFad <- table(d0[,37]) #Zellen bilden
for (i in 2:length(tabFad)) {    #Ziehen und in sam schreiben // 2 weil 1 == A bis D mit F
    dh <- d1Fad[(!is.na(d1Fad[,37]) & d1Fad[,37]==names(tabFad)[i]),]
    samTot <- c(samTot,sample(dh[,1],round(tabFad[i]*.1)))
}
#############################################


#Ziehung BV
#############################################
tabBv <- table(d0[,38]) #Zellen bilden
for (i in 1:length(tabBv)) {   
    dh <- d1Bv[(!is.na(d1Bv[,38]) & d1Bv[,38]==names(tabBv)[i]),]
    samTot <- c(samTot,sample(dh[,1],round(tabBv[i]*.05)))
}
#############################################


#Ziehung PZ 
#############################################
tabPz <- table(d0[,39]) #Zellen bilden
for (i in 1:length(tabPz)) {   
    dh <- d1Pz[(!is.na(d1Pz[,39]) & d1Pz[,39]==names(tabPz)[i]),]
    samTot <- c(samTot,sample(dh[,1],round(tabPz[i]*.05)))
}
#############################################



#Output und fertig
#############################################
dOut <- d0[d0$SAP_ID %in% samTot,] 
dOut$typ <- NA
dOut$typ[dOut$PG_ABK %in% samPgFad] <- "FAD"
dOut$typ[dOut$PG_ABK %in% samPgBv] <- "BV"
dOut$typ[dOut$PG_ABK %in% samPgPz] <- "PZ"
dOut$typ[dOut$SAP_ID %in% samTotFadFo] <- "FAD_FO" 

write.csv2(dOut,"Poststellen_ersatz.csv",fileEncoding="CP1252")
#############################################


#Check
#############################################
table(dOut$typ)
table(dOut$PG_BEZ_D[dOut$typ=="FAD"])
table(dOut$PG_BEZ_D[dOut$typ=="BV"])
table(dOut$PG_BEZ_D[dOut$typ=="PZ"])
#############################################


