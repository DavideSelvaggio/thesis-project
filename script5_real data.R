### REAL DATA STANDARDIZED OUTLIERS ###

rm(list=ls())
library(randomForest)
vettNrSNP<-matrix(NA,29,1)
chr<-c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29")
for (h in 1:29){
  nomi<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/", chr[h],"_ret.txt"))
  vettNrSNP[h]<-nrow(nomi)
}

seme<-87648364                           #### parametro in input
set.seed(seme)
nrazze<-c(749,2093,410,479)
nr<-length(nrazze)
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
######################################### fine passo 1) e inizio passo 2) ##############################################################


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

######################################### fine passo 1) e inizio passo 2) ##############################################################

lnr<-lapply(lidtr,length)
v<-(1:nr)[-esclusa]
k<-length(v)
esclusaOut<- 4

############### ciclo per i da 1 a k   ########################

proxlist<-list()
proxlisth<-list()
proxlistt<-list()
razzajlist<-list()
razzajthreshlist<-list()
razzajtargetlist<-list()
idlist1<-list()
idlist2<-list()
idlist3<-list()
listidlist1<-list()
listidlist2<-list()
listidlist3<-list()
pcasellistj<-list()

semi<-42   #### parametro in input
fr<-c(.50,.75)								#### parametro in input
set.seed(semi)

############### training random forest  ########################
seqnsel<-seq(4,30,by=2)

tprimolist<-lapply(trainlist,qtprimo,v=(1:nr),ciclo=c(esclusa,esclusaOut))
rlist<-lapply(trainlist,qr,v=(1:nr),ciclo=esclusaOut)

num<-unlist(lapply(tprimolist[[1]],nrow))
rnum<-nrow(rlist[[1]][[1]])

for(jj in 1:length(tprimolist[[1]])){
  permjj<-sample(1:num[jj],num[jj])
  idlist1[[jj]]<-permjj[1:round(num[jj]*fr[1])]
  idlist2[[jj]]<-permjj[(round(num[jj]*fr[1])+1):round(num[jj]*fr[2])]
  idlist3[[jj]]<-permjj[(round(num[jj]*fr[2])+1):num[jj]]
}

rperm<-sample(1:rnum,rnum)
ridlist<-rperm[1:round(rnum*fr[1])]

trainstar<-lapply(tprimolist,star,idlist=idlist1)
trainthresh<-lapply(tprimolist,star,idlist=idlist2)
testtarget<-lapply(tprimolist,star,idlist=idlist3)


for (uu in 1:29){
  print(paste("ciclo=",0,"j=", 0, "uu=", uu))
  pcasellistj[[uu]]<-pcasel(trainstar[[uu]])
}

proxlistj<-list()
proxlisthj<-list()
proxlisttj<-list()
ciclosel<-0
for (nsel in seqnsel){
  ciclosel<-ciclosel+1
  pcasellistj1<-lapply(pcasellistj,sel,nsel)	
  
  trnselj<-trainstar[[1]]
  trnselj<-trnselj[,pcasellistj1[[1]]]
  tsnselj<-testtarget[[1]]
  tsnselj<-tsnselj[,pcasellistj1[[1]]]
  ttnselj<-trainthresh[[1]]
  ttnselj<-ttnselj[,pcasellistj1[[1]]]
  outselj<-rlist[[1]][[1]][,pcasellistj1[[1]]]
  
  for (ii in 2:29){
    trnselij<-trainstar[[ii]]
    trnselj<-cbind(trnselj,trnselij[,pcasellistj1[[ii]]])
    tsnselij<-testtarget[[ii]]
    tsnselj<-cbind(tsnselj,tsnselij[,pcasellistj1[[ii]]])
    ttnselij<-trainthresh[[ii]]
    ttnselj<-cbind(ttnselj,ttnselij[,pcasellistj1[[ii]]])
    outselj<-cbind(outselj,rlist[[ii]][[1]][,pcasellistj1[[ii]]])
  }
 
  trainstarj<-data.frame(lapply(trnselj,factor,levels=c("0","1","2")))
  trainthreshj<-data.frame(lapply(ttnselj,factor,levels=c("0","1","2")))
  testtargetj<-data.frame(lapply(rbind(tsnselj),factor,levels=c("0","1","2")))
  testoutj<-data.frame(lapply(outselj[ridlist,],factor,levels=c("0","1","2")))
  
  razzaj<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist1,length))))
  razzajthresh<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist2,length))))
  razzajtarget<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist3,length))))
  
  outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE)
  proxlisthj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,trainthreshj),proximity=TRUE)$proximity
  proxlistj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,testtargetj),proximity=TRUE)$proximity
  proxlisttj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,testoutj),proximity=TRUE)$proximity
  
  
}

listidlist1[[1]]<-idlist1
listidlist2[[1]]<-idlist2
listidlist3[[1]]<-idlist3
razzajlist[[1]]<-razzaj
razzajtargetlist[[1]]<-razzajtarget
razzajthreshlist[[1]]<-razzajthresh
proxlist[[1]]<-proxlistj
proxlisth[[1]]<-proxlisthj
proxlistt[[1]]<-proxlisttj

# fine training random forest
# calcolo degli outliers per il training set delle soglie
rawoutth<-list()
outth<-list()
medth<-list()
madth<-list()


  
  razzaj<-razzajlist[[1]]
  ntr<-as.numeric(table(razzaj))
  a<-cumsum(ntr)
  a<-c(0,a)
  razzajthresh<-razzajthreshlist[[1]]
  nth<-as.numeric(table(razzajthresh))
  b<-cumsum(nth)
  b<-c(0,b)
  idlist<-listidlist2[[1]]
  
  for(j in 1:ciclosel){
    
    rawoutthj<-NULL
  
    for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlisth[[1]][[j]][(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
    rawoutthj<-c(rawoutthj,sum(ntr)/vec)
    
  }
  
  rawoutth[[j]]<-rawoutthj
  medth[[j]]<-as.numeric(by(rawoutth[[j]],razzajthresh,median))
  madth[[j]]<-as.numeric(by(rawoutth[[j]],razzajthresh,mad))
  medthrep<-rep(medth[[j]],times=unlist(lapply(idlist,length)))
  madthrep<-rep(madth[[j]],times=unlist(lapply(idlist,length)))
  outth[[j]]<-(rawoutth[[j]]-medthrep)/madthrep
  
  }
  
#creo test* = test_target + test_out
 
proxlistts<-list()
razzaj<-razzajlist[[1]]
a<-sum(table(razzaj))

for(j in 1:ciclosel){
  
    proxlistts[[j]]<-cbind(proxlist[[1]][[j]][(1:a),],proxlistt[[1]][[j]][(1:a),-(1:a)])
    
}  

#calcolo degli outliers per il test set
rawoute<-list()

  
  razzaj<-razzajlist[[1]]
    a<-cumsum(table(razzaj))
  a<-c(0,a)
  
  for(j in 1:ciclosel){
  
    matrix<-NULL
    
  for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlistts[[j]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxlistts[[j]])])^2,2,sum)
    matrix<-rbind(matrix,a[length(a)]/vec)
    
  }
  
  rawoute[[j]]<-matrix
  
}

soute<-list()
for(j in 1:ciclosel){
  
  d<-dim(rawoute[[j]])[2]
  med<-NULL
  mad<-NULL
  for(c in 1:length(medth[[j]])){
    med<-rbind(med,rep(medth[[j]][c],d))
    mad<-rbind(mad,rep(madth[[j]][c],d))
  }
  soute[[j]]<-(rawoute[[j]]-med)/(mad)
  
}

#regola

decision<-function(soute,par){ #par vettore di parametri di dimensione pari al numero di righe (classi) di soute. 
  
  outregola <- soute > par
  
  outregola<-apply(outregola,2,prod)
  outregola[outregola==0] <- 'T'
  outregola[outregola==1] <- 'O'
  return(outregola)
  
}

## funzione obiettivo

funobb<-function(par,soute,cl){
  
  ss1<-1
  outreg<-decision(soute,par)
  if(length(unique(outreg))!=1){
    confmat<-table(cl,outreg)
    sens<-confmat[2,2]/sum(confmat[2,])
    spec<-confmat[1,1]/sum(confmat[1,])
    ss1<-1-(sens*spec)/(sens+spec)  
  }
  
  ss1
  
}

cl<-list()

num<-sum(table(razzajtargetlist[[1]]))
cl[[1]]<-c(rep("T",num),rep("O",(dim(soute[[1]])[2]-num)))

numiniz<-10
dimso<-dim(soute[[1]])[1]
inizpar<-matrix(NA,numiniz,dimso)   ### numiniz da dare in input

for(u in 1:numiniz){
  inizpar[u,]<-runif(dimso)
}

#parametri ottimi

parmat<-list()
optpar<-list()
for (c in 1:ciclosel){
  parmat[[c]]<-matrix(NA,numiniz,dimso+1)
  for (u in 1:numiniz){
    print(paste("c=", c, "u=", u))
    outmin<-optim(inizpar[u,],funobb,soute=soute[[c]],cl=cl[[1]])
    if (outmin$convergence==0) parmat[[c]][u,]<-c(outmin$par,outmin$value)
  }
  optpar[[c]]<-parmat[[c]][which.min(parmat[[c]][,dimso+1]),]
}

#rappresentazione grafica ciclosel=14

ind<-cumsum(c(table(razzajtargetlist[[1]]),length(ridlist)))
plot(soute[[14]][1,(ind[2]+1):ind[3]],col='grey',main='Distribution of class outlier
     and novelty outlier
     -Real data-',ylab='outlyingness value',xlim=c(0,180),ylim=c(-2,150))
#points(soute[[14]][1,(ind[2]+1):ind[3]],col='grey')
points((soute[[14]][1,1:ind[1]]),col='red')
legend("topright", c('class outlier ','novelty'), title='class 2', pch=1,
       col=c('red','grey'))
abline(h=optpar[[14]][1])

## grafico 2

plot(soute[[14]][1,1:ind[1]],soute[[14]][2,1:ind[1]],col='red',main='Distribution of standardized outlier
     between classes
     -Real data-',xlab='outlyingness value class 2',ylab='outlyingness value class 3',xlim=c(-1,750),ylim=c(-1,1000))
points(soute[[14]][1,(ind[1]+1):ind[2]],soute[[14]][2,(ind[1]+1):ind[2]],col='blue')
points(soute[[14]][1,(ind[2]+1):ind[3]],soute[[14]][2,(ind[2]+1):ind[3]],col='grey')
abline(v=optpar[[14]][1])
abline(h=optpar[[14]][2])
legend("topright", c('class 2 ','class 3','novelty'), title='classes', pch=1,
       col=c('red','blue','grey'))

### TRAINING TOTALE

pcasellist<-list()
for (uu in c(1:29)){
  print(paste("uu=",uu))
  qqq<-as.matrix(bindlist(trainlist[[uu]]))+2.5  # costante aggiunta per evitare LINPACK su cromosoma 5
  pcasellist[[uu]]<-pcasel(qqq)      
}

rz<-(1:nr)[-esclusaOut]
vv<-(1:nr)[-c(esclusa,esclusaOut)]

proxmatrixtestT<-list()
proxmatrixtestO<-list()

ciclosel<-0
for(nsel in seqnsel){
  ciclosel<-ciclosel+1
  print(paste('ciclosel=',ciclosel))
  pcasellist1<-lapply(pcasellist,sel,nsel)
  
  trnsel<-bindlist(trainlist[[1]][rz])
  trnsel<-trnsel[,pcasellist1[[1]]]
  
  tsnsel<-bindlist(testlist[[1]][vv])
  tsnsel<-tsnsel[,pcasellist1[[1]]]
  
  outsel<-testlist[[1]][[esclusa]]
  outsel<-outsel[,pcasellist1[[1]]]
  
  for (ii in 2:29){
    trnseli<-bindlist(trainlist[[ii]][rz])
    trnsel<-cbind(trnsel,trnseli[,pcasellist1[[ii]]])
  
    tsnseli<-bindlist(testlist[[ii]][vv])
    tsnsel<-cbind(tsnsel,tsnseli[,pcasellist1[[ii]]])
  
    outseli<-testlist[[ii]][[esclusa]]
    outsel<-cbind(outsel,outseli[,pcasellist1[[ii]]])
  
  }
  
  testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
  traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
  testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))
  
  razza<-as.factor(rep(vv,times=unlist(lapply(lidtr,length))[-c(esclusa,esclusaOut)]))
  numtarget<-(nrazze-unlist(lapply(lidtr,length)))[-c(esclusa,esclusaOut)]
  razzatarget<-as.factor(rep(vv,times=numtarget))
  outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
  proxmatrixtestT[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
  proxmatrixtestO[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity
  
  
}

proxmatrixtestS<-list()
a<-sum(table(razza))

for(c in 1:ciclosel){
  proxmatrixtestS[[c]]<-cbind(proxmatrixtestT[[c]][(1:a),],proxmatrixtestO[[c]][(1:a),-(1:a)])
}

a<-cumsum(table(razza))
a<-c(0,a)
rawout<-list()
for(c in 1:ciclosel){
  rawoutc<-NULL
  for(l in 1:(length(a)-1)){
  
    vec<-apply((proxmatrixtestS[[c]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxmatrixtestS[[c]])])^2,2,sum)
    rawoutc<-rbind(rawoutc,a[length(a)]/vec)
  
  }
rawout[[c]]<-rawoutc
}

#calcolo outliers standardizzati
sout<-list()
for(c in 1:ciclosel){
  d<-dim(rawout[[c]])[2]
  med<-NULL
  mad<-NULL
  for(k in 1:length(medth[[c]])){
    med<-rbind(med,rep(medth[[c]][k],d))
    mad<-rbind(mad,rep(madth[[c]][k],d))
  }
 sout[[c]]<-(rawout[[c]]-med)/(mad)
}


#valutazione performance del sistema
ss1<-list()
for(c in 1:ciclosel){
  decided<-decision(sout[[c]],optpar[[c]][1:2])
  real<-c(rep("T",sum(numtarget)),rep("O",(dim(sout[[1]])[2]-sum(numtarget))))
  confmat<-table(real,decided)
  sens<-confmat[2,2]/sum(confmat[2,])
  spec<-confmat[1,1]/sum(confmat[1,])
  ss1[[c]]<-1-(sens*spec)/(sens+spec) 
}

# rappresentazione grafica, ciclosel=14
index1<-cumsum(c(numtarget,(dim(sout[[14]])[2]-sum(numtarget))))
index2<-cumsum(as.numeric(table(decision(sout[[14]],optpar[[14]][1:2]))))
plot(sout[[14]][1,(index1[1]+1):index1[2]],col='blue',ylim=c(-5,4000),ylab='outlyingness value',main='Distribution of standardized outlier
     complete training
     -Real data-')
points(sout[[14]][1,(index1[2]+1):index1[3]],col='grey')
points(sout[[14]][1,1:index1[1]],col='red')
legend("topright", c('class 2 ','class 3','novelty'), title='class 2', pch=1,
       col=c('red','blue','grey'))
abline(h=optpar[[1]][1])
