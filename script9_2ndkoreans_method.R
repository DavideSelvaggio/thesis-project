rm(list=ls())
library(randomForest)
vettNrSNP<-matrix(NA,29,1)
chr<-c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29")
for (h in 1:29){
  nomi<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/", chr[h],"_ret.txt"))
  vettNrSNP[h]<-nrow(nomi)
}


##/home/selvaggio/tesi/dati_razze/

################################# passo 1)  ##############################################################
seme<-87648364                           #### parametro in input
set.seed(seme)
nrazze<-c(749,2093,410,479)
esclusa<-2                           #### parametro in input
trfract<-0.75                         #### parametro in input
lperm<-list()
lidtr<-list()

for (w in 1:4){
  if (w == esclusa) {lperm[[w]]<-NULL
  lidtr[[w]]<-NULL}
  else{
    lperm[[w]]<-sample(1:nrazze[w],nrazze[w])
    lidtr[[w]]<-lperm[[w]][1:round(nrazze[w]*trfract)]
  }
}


flist<-list()
flist[[1]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/b", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[2]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/f", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[3]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/m", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[4]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/p", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))


trainlist<-list()
testlist<-list()
esclusalist<-list()

train<-list()
test<-list()
for (w in 1:4){
  if (w == esclusa) {train[[w]]<-NULL
  test[[w]]<-flist[[w]][,-1]}
  else {
    train[[w]]<-flist[[w]][lidtr[[w]],-1]
    test[[w]]<-flist[[w]][-lidtr[[w]],-1]
  }
}

trainlist[[1]]<-train
testlist[[1]]<-test


for (i in c(2:29)){
  
  train<-list()
  test<-list()
  
  flist[[1]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/b", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[2]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/f", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[3]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/m", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[4]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/p", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  
  for (w in 1:4){
    if (w == esclusa) {train[[w]]<-NULL
    test[[w]]<-flist[[w]][,-1]}
    else {
      train[[w]]<-flist[[w]][lidtr[[w]],-1]
      test[[w]]<-flist[[w]][-lidtr[[w]],-1]
    }
  }
  trainlist[[i]]<-train
  testlist[[i]]<-test
  
}

bindlist<-function(x){
  y<-x[[1]]
  for (i in 2:length(x)){
    y<-rbind(y,x[[i]])
  }
  y
}

cbindlist<-function(x){
  y<-x[[1]]
  for (i in 2:length(x)){
    y<-cbind(y,x[[i]])
  }
  y
}

pcasel<-function(x,ncomp=3){
  pca<-prcomp(t(x))
  scores<-pca$x[,1:ncomp]%*%diag(1/pca$sdev[1:ncomp])
  sel<-order(apply(scores[,1:ncomp]^2,1,sum),decreasing=TRUE)
  sel
}


star<-function(xjj,idlist){ #da qui viene fuori una matrice che contiene le righe selezionate da idlist in ogni razza, un blocco sotto l'altro
  g<-NULL
  for (i in 1:length(xjj)){
    gi<-xjj[[i]]
    g<-rbind(g,gi[idlist[[i]],])
  }
  g
}

starmeno<-function(xjj,idlist){ #da qui viene fuori una matrice che contiene le righe NON selezionate da idlist in ogni razza, un blocco sotto l'altro
  g<-NULL
  for (i in 1:length(xjj)){
    gi<-xjj[[i]]
    g<-rbind(g,gi[-idlist[[i]],])
  }
  g
}

qtprimo<-function(x,v,ciclo) x[v[-ciclo]]

qr<-function(x,v,ciclo) x[v[ciclo]]

sel<-function(x,nsel) x[1:nsel]

lnr<-lapply(lidtr,length)
v<-(1:4)[-esclusa]
k<-3 # nr classi presenti nel training

pcasellist<-list()
for (uu in c(1:29)){
  print(paste("uu=",uu))
  qqq<-as.matrix(bindlist(trainlist[[uu]]))+2.5  # costante aggiunta per evitare LINPACK su cromosoma 5
  pcasellist[[uu]]<-pcasel(qqq)      
}

######## RANDOM FOREST  ##########

proxtrainlist<-list()
proxtargetlist<-list()
proxoutlist<-list()

seqnsel<-seq(4,30,by=2)	
ciclosel<-0
for (nsel in seqnsel)	{
  ciclosel<-ciclosel+1
  print(paste("CICLOSEL=",ciclosel))
  pcasellist1<-lapply(pcasellist,sel,nsel)	
  
  trnsel<-bindlist(trainlist[[1]])
  trnsel<-trnsel[,pcasellist1[[1]]]
  
  tsnsel<-bindlist(testlist[[1]][v])
  tsnsel<-tsnsel[,pcasellist1[[1]]]
  
  outsel<-testlist[[1]][[esclusa]]
  outsel<-outsel[,pcasellist1[[1]]]
  
  for (ii in 2:29){
    trnseli<-bindlist(trainlist[[ii]])
    trnsel<-cbind(trnsel,trnseli[,pcasellist1[[ii]]])
    
    tsnseli<-bindlist(testlist[[ii]][v])
    tsnsel<-cbind(tsnsel,tsnseli[,pcasellist1[[ii]]])
    
    outseli<-testlist[[ii]][[esclusa]]
    outsel<-cbind(outsel,outseli[,pcasellist1[[ii]]])
    
  }
  
  testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
  traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
  testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))
  
  razza<-as.factor(rep(v,times=unlist(lapply(lidtr,length))[-esclusa]))
  numtarget<-(nrazze-unlist(lapply(lidtr,length)))[-esclusa]
  razzatarget<-as.factor(rep(v,times=numtarget))
  outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
  
  proxtrainlist[[ciclosel]]<-predict(outRF,proximity=TRUE)$proximity
  proxtargetlist[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
  proxoutlist[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity
  
}

######## CALCOLO DELLE SOGLIE  ##########
ntr<-table(razza)
a<-cumsum(ntr)
a<-c(0,a)
pimed<-list()
for(c in 1:ciclosel){
  pimedc<-NULL
  for(l in 1:(length(a)-1)){
    vec<-apply((proxtrainlist[[c]][(a[l]+1):a[l+1],(a[l]+1):a[l+1]]),2,sum)
    pimedc<-c(pimedc,sum(vec)/ntr[l]^2)
  }
  pimed[[c]]<-pimedc
}

proxmatrixtestS<-list()
b<-length(razza)
for(c in 1:ciclosel){
  print(paste("prox matrix S n.",c,"on 14"))
  proxmatrixtestS[[c]]<-cbind(proxtargetlist[[c]][(1:b),],proxoutlist[[c]][(1:b),-(1:b)])
}

Wdi<-list()
for(c in 1:ciclosel){
  wdic<-NULL
  print(paste("ciclosel n.",c,"on 14"))
  for(l in 1:(length(a)-1)){
    vec<-apply(proxmatrixtestS[[c]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxmatrixtestS[[c]])],2,sum)
    wdic<-rbind(wdic,vec/(ntr[l]))
  }
  Wdi[[c]]<-(wdic)
}
####### APPLICAZIONE DELLE SOGLIE #######

decided<-list()
alpha<-1                             ### parametro in input
for(c in 1:ciclosel){
  decidedj<-NULL
  decidedj<-Wdi[[c]]<alpha*pimed[[c]]
  decidedj<-apply(decidedj,2,prod)
  decidedj[decidedj==1]<-'O'
  decidedj[decidedj==0]<-'T'
  decided[[c]]<-decidedj
}


###### VALUTAZIONE DEL SISTEMA ####

real<-c(rep('T',length(razzatarget)),rep('O',dim(testout)[1]))
ss1<-list()
for(c in 1:ciclosel){
  confmat<-table(real,decided[[c]])
  sens<-confmat[2,2]/sum(confmat[2,])
  spec<-confmat[1,1]/sum(confmat[1,])
  ss1[[c]]<-1-(sens*spec)/(sens+spec) 
}


##### RAPPRESENTAZIONE GRAFICA ####

colors1<-decided[[14]]
colors1[colors1=='T']<-'darkorange2'
colors1[colors1=='O']<-'grey45'
colors2<-c(razzatarget,rep('grey45',dim(testout)[1]))
colors2[colors2==1]<-'red3'
colors2[colors2==3]<-'yellowgreen'
colors2[colors2==2]<-'royalblue3'

par(mfrow=c(2,3))
plot(Wdi[[14]][1,],Wdi[[14]][2,],col=colors2,ylab = 'proximity to class 3',
     xlab = 'proximity to class 1',main = 'Proximity Distribution')
legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
       col=c('red3','royalblue3','yellowgreen','darkgrey'))

plot(Wdi[[14]][1,],Wdi[[14]][3,],col=colors2,ylab = 'proximity to class 4',
     xlab = 'proximity to class 1',main = 'Proximity Distribution')
legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
       col=c('red3','royalblue3','yellowgreen','darkgrey'))

plot(Wdi[[14]][2,],Wdi[[14]][3,],col=colors2,ylab = 'proximity to class 4',
     xlab = 'proximity to class 3',main = 'Proximity Distribution')
legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
       col=c('red3','royalblue3','yellowgreen','darkgrey'))

plot(Wdi[[14]][1,],Wdi[[14]][2,],col=colors1,ylab = 'proximity to class 3',
     xlab = 'proximity to class 1',main = 'Proximity Distribution')
legend("topright", c('known breeds','novelty'), title='classes',pch=1,
       col=c('darkorange2','darkgrey'))

plot(Wdi[[14]][1,],Wdi[[14]][3,],col=colors1,ylab = 'proximity to class 4',
     xlab = 'proximity to class 1',main = 'Proximity Distribution')
legend("topright", c('known breeds','novelty'), title='classes',pch=1,
       col=c('darkorange2','darkgrey'))

plot(Wdi[[14]][2,],Wdi[[14]][3,],col=colors1,ylab = 'proximity to class 4',
     xlab = 'proximity to class 3',main = 'Proximity Distribution')
legend("topright", c('known breeds','novelty'), title='classes',pch=1,
       col=c('darkorange2','darkgrey'))