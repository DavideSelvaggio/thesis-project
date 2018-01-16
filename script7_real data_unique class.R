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
trfract<-c(0.60,0.80)                         #### parametro in input
lperm<-list()
lidtr<-list()
lidth<-list()
lidtt<-list()

for (w in 1:nr){
  if (w == esclusa) {lperm[[w]]<-NULL
  lidtr[[w]]<-NULL
  lidth[[w]]<-NULL
  lidtt[[w]]<-NULL}
  else{
    lperm[[w]]<-sample(1:nrazze[w],nrazze[w])
    lidtr[[w]]<-lperm[[w]][1:round(nrazze[w]*trfract[1])]
    lidth[[w]]<-lperm[[w]][(round(nrazze[w]*trfract[1])+1):round(nrazze[w]*trfract[2])]
    lidtt[[w]]<-lperm[[w]][(round(nrazze[w]*trfract[2])+1):nrazze[w]]
  }
}

flist<-list()
flist[[1]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/b", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[2]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/f", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[3]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/m", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))
flist[[4]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/p", 1, "_imp.txt"), col.names = c("id",paste0("Ch1_", 1:vettNrSNP[1])))


trainlist<-list()
threshlist<-list()
testlist<-list()
esclusalist<-list()

train<-list()
thresh<-list()
test<-list()
for (w in 1:4){
  if (w == esclusa) {train[[w]]<-NULL
  thresh[[w]]<-NULL
  test[[w]]<-flist[[w]][,-1]}
  else {
    train[[w]]<-flist[[w]][lidtr[[w]],-1]
    thresh[[w]]<-flist[[w]][lidth[[w]],-1]
    test[[w]]<-flist[[w]][lidtt[[w]],-1]
  }
}

trainlist[[1]]<-train
threshlist[[1]]<-thresh
testlist[[1]]<-test


for (i in c(2:29)){
  
  train<-list()
  thresh<-list()
  test<-list()
  
  flist[[1]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/b", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[2]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/f", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[3]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/m", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  flist[[4]]<-read.table(paste0("C:/Users/Davide/Documents/documenti/scuola/Tesi/materiale_tesi/dati_razze/p", i, "_imp.txt"),col.names = c("id",paste0("Ch", i, "_", 1:vettNrSNP[i])))
  
  for (w in 1:4){
    if (w == esclusa) {train[[w]]<-NULL
    thresh[[w]]<-NULL
    test[[w]]<-flist[[w]][,-1]}
    else {
      train[[w]]<-flist[[w]][lidtr[[w]],-1]
      thresh[[w]]<-flist[[w]][lidth[[w]],-1]
      test[[w]]<-flist[[w]][lidtt[[w]],-1]
    }
  }
  trainlist[[i]]<-train
  threshlist[[i]]<-thresh
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

v<-(1:nr)[-esclusa]
k<-length(v)

############### ciclo per i da 1 a k   ########################

proxlist<-list()
proxlisth<-list()
proxlistt<-list()
razzajlist<-list()
razzajthreshlist<-list()
razzajtargetlist<-list()
listidlist1<-list()
listidlist2<-list()
rridlist<-list()

m<-1                                                  #### parametro in input
semi<-matrix(seq(from=1,by=3476,length.out=m),k,m)   #### parametro in input
fr<-c(.50,.75)								                       #### parametro in input
ciclo<-0
############### training random forest  ########################
seqnsel<-seq(4,30,by=2)
for(i in v){
  ciclo<-ciclo+1
  print(paste('razza esclusa=',i))
  tprimolist<-lapply(trainlist,qtprimo,v=v,ciclo=ciclo) #da sistemare questa lista, noi abbiamo una sola lista che raggruppa tutti i cromosomi.
  rlist<-lapply(trainlist,qr,v=v,ciclo=ciclo)

  num<-unlist(lapply(tprimolist[[1]],nrow))
  rnum<-nrow(rlist[[1]][[1]])
  pcasellistj<-list()
  set.seed(semi[ciclo,m])
  idlist1<-list()
  idlist2<-list()
  idlist3<-list()

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
  print(paste("ciclo=",ciclo, "uu=", uu))
  pcasellistj[[uu]]<-pcasel(trainstar[[uu]])
}

proxlistj<-list()
proxlisthj<-list()
proxlisttj<-list()
razzaj<-list()
razzajthresh<-list()
razzajtargetl<-list()

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
  
  razzaj[[ciclosel]]<-as.factor(rep(v[-ciclo],unlist(lapply(idlist1,length))))
  razzajthresh[[ciclosel]]<-as.factor(rep(v[-ciclo],unlist(lapply(idlist2,length))))
  razzajtarget<-as.factor(rep(v[-ciclo],unlist(lapply(idlist3,length))))
  razzajtargetl[[ciclosel]]<-(rep(v[-ciclo],unlist(lapply(idlist3,length))))
  
  outRFj<-randomForest(x=trainstarj,y=razzaj[[ciclosel]],proximity=TRUE)
  proxlisthj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,trainthreshj),proximity=TRUE)$proximity
  proxlistj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,testtargetj),proximity=TRUE)$proximity
  proxlisttj[[ciclosel]]<-predict(outRFj,newdata=rbind(trainstarj,testoutj),proximity=TRUE)$proximity
  
  
}

listidlist1[[ciclo]]<-idlist1
listidlist2[[ciclo]]<-idlist2
razzajlist[[ciclo]]<-razzaj
razzajtargetlist[[ciclo]]<-razzajtargetl
razzajthreshlist[[ciclo]]<-razzajthresh
proxlist[[ciclo]]<-proxlistj
proxlisth[[ciclo]]<-proxlisthj
proxlistt[[ciclo]]<-proxlisttj
rridlist[[ciclo]]<-length(ridlist)
}
# fine training random forest

# calcolo degli outliers per il training set delle soglie
rawoutth<-list()
medth<-list()
madth<-list()

for(j in 1:k){
  
  razzaj<-razzajlist[[j]]
  razzajthresh<-razzajthreshlist[[j]]
  rawoutthj<-list()
  medthj<-list()
  madthj<-list()
  
  for(c in 1:ciclosel){
    
    ntr<-as.numeric(table(razzaj[[c]]))
    a<-cumsum(ntr)
    a<-c(0,a)
    nth<-as.numeric(table(razzajthresh[[c]]))
    b<-cumsum(nth)
    b<-c(0,b)

    rawoutthjj<-NULL

    
    for(l in 1:(length(a)-1)){
      
      vec<-apply((proxlisth[[j]][[c]][(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
      rawoutthjj<-c(rawoutthjj,sum(ntr)/vec)
      
    }
    
    rawoutthj[[c]]<-rawoutthjj
    medthj[[c]]<-as.numeric(by(rawoutthj[[c]],razzajthresh[[c]],median))
    madthj[[c]]<-as.numeric(by(rawoutthj[[c]],razzajthresh[[c]],mad))
    
  }
    
  rawoutth[[j]]<-rawoutthj
  medth[[j]]<-medthj
  madth[[j]]<-madthj
}

#creo test* = test_target + test_out

proxlistts<-list()

for(j in 1:k){
  
razzaj<-razzajlist[[j]]
proxlisttsj<-list()

  for(c in 1:ciclosel){
  
    a<-sum(table(razzaj[[c]]))
    proxlisttsj[[c]]<-cbind(proxlist[[j]][[c]][(1:a),],proxlistt[[j]][[c]][(1:a),-(1:a)])
  
  }  

proxlistts[[j]]<-proxlisttsj

}

#calcolo degli outliers per il test set
rawoute<-list()
for(j in 1:k){
  razzaj<-razzajlist[[j]]
  rawoutej<-list()
  for(c in 1:ciclosel){
    a<-cumsum(table(razzaj[[c]]))
    a<-c(0,a) 
    matrix<-NULL
    for(l in 1:(length(a)-1)){
      vec<-apply((proxlistts[[j]][[c]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxlistts[[j]][[c]])])^2,2,sum)
      matrix<-rbind(matrix,a[length(a)]/vec)
    }
    rawoutej[[c]]<-matrix
  }
  rawoute[[j]]<-rawoutej
}

soute<-list()
for(j in 1:k){
  soutej<-list()
  for(c in 1:ciclosel){
    d<-dim(rawoute[[j]][[c]])[2]
    med<-NULL
    mad<-NULL
    for(s in 1:length(medth[[j]][[c]])){
      med<-rbind(med,rep(medth[[j]][[c]][s],d))
      mad<-rbind(mad,rep(madth[[j]][[c]][s],d))
    }
    soutej[[c]]<-(rawoute[[j]][[c]]-med)/(mad)
    }
  soute[[j]]<-soutej
}
second.min<-function(x){
  n <- length(x)
  sort(x)[2]
}
soutematrix<-list()
soutematriy<-list()
tab<-list()
for(c in 1:ciclosel){
  soutematrixj<-NULL
  for(j in 1:k){
    soutematrixj<-cbind(soutematrixj,rbind(c(razzajtargetlist[[j]][[c]],rep(9,rridlist[[j]])),soute[[j]][[c]]))
  }
  soutematrixj<-soutematrixj[,order(soutematrixj[1,])]
  tab[[c]]<-table(soutematrixj[1,])
  soutematriy[[c]]<-apply(soutematrixj[2:(dim(soutematrixj)[1]),],2,second.min)
  soutematrix[[c]]<-apply(soutematrixj[2:(dim(soutematrixj)[1]),],2,min)
}

###rappresentazione grafica di soutematrix e soutematriy cilosel = 14

plot(soutematrix[[14]],soutematriy[[14]],col=rep(c('red3','royalblue3','yellowgreen','darkgrey'),tab[[14]]),
     main='Distribution of minimum values
     -Real Data-',xlab='1st minimum value',ylab='2nd minimum value')
legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
       col=c('red3','royalblue3','yellowgreen','darkgrey'))


#regola

decision<-function(soute,par){ #par vettore di parametri di dimensione pari al numero di righe (classi) di soute. 
  
  outregola <- soute > par
  
  outregola[outregola==FALSE] <- 'T'
  outregola[outregola==TRUE] <- 'O'
  return(outregola)
  
}

## funzione obiettivo

funobb<-function(par,soute,cl){
  
  ss1<-1
  if(prod(unique(par)>0)==1){
    outreg<-decision(soute,par)
    if(length(unique(outreg))!=1){
      confmat<-table(cl,outreg)
      sens<-confmat[2,2]/sum(confmat[2,])
      spec<-confmat[1,1]/sum(confmat[1,])
      ss1<-1-(sens*spec)/(sens+spec)  
    }
  }
  ss1
  
}

cl<-list()
for(c in 1:ciclosel){
  num<-sum(tab[[c]][1:k])
  cl[[c]]<-c(rep("T",num),rep("O",tab[[c]][k+1]))
}

#parametri ottimi
optpar<-list()
for(c in 1:ciclosel){
  optpar[[c]]<-optimize(funobb,soute=soutematrix[[c]],cl=cl[[c]],interval=range(soutematrix[[c]]))
}

x<-NULL
y1<-NULL
y2<-NULL
for(c in 1:ciclosel){
  x<-c(x,c)
  y1<-c(y1,optpar[[c]]$minimum)
  y2<-c(y2,optpar[[c]]$objective)
}
plot(x,y2,type='b',pch=17,col='orangered',xlab='ciclosel',ylab='objective value',main='objective function convergence')

#rappresentazione grafica ciclosel=14
index<-cumsum(tab[[14]])
plot(density(soutematrix[[14]][1:index[1]]),col='yellowgreen',ylim=c(0,.53),xlim=c(-3,25),main='Comparison of standardized outlier
     -Real Data-')
lines(density(soutematrix[[14]][(index[1]+1):index[2]]),col='red3')
lines(density(soutematrix[[14]][(index[2]+1):index[3]]),col='royalblue3')
lines(density(soutematrix[[14]][(index[3]+1):index[4]]),col='darkgrey')
legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',lty=c(1,1),
       col=c('yellowgreen','red3','royalblue3','darkgrey'))
abline(v=optpar[[14]]$minimum)

plot((soutematrix[[14]][(index[3]+1):index[4]]),col='grey45',ylim=c(-1,25),main='Distribution of unique class outlier
     and novelty outlier
     -Real Data-',ylab='outlyingness value')
points((soutematrix[[14]][1:index[3]]),col='darkorange2')
legend("topright", c('unique breed class','novelty'), pch=1, title='classes',
       col=c('darkorange2','grey45'))
abline(h=optpar[[14]]$minimum)

### TRAINING TOTALE

pcasellist<-list()
for (uu in c(1:29)){
  print(paste("uu=",uu))
  qqq<-as.matrix(bindlist(trainlist[[uu]]))+2.5  # costante aggiunta per evitare LINPACK su cromosoma 5
  pcasellist[[uu]]<-pcasel(qqq)      
}

proxmatrixtestTh<-list()
proxmatrixtestT<-list()
proxmatrixtestO<-list()

ciclosel<-0
for(nsel in seqnsel){
  ciclosel<-ciclosel+1
  print(paste('ciclosel=',ciclosel))
  pcasellist1<-lapply(pcasellist,sel,nsel)
  
  trnsel<-bindlist(trainlist[[1]])
  trnsel<-trnsel[,pcasellist1[[1]]]
  
  thnsel<-bindlist(threshlist[[1]])
  thnsel<-thnsel[,pcasellist1[[1]]]
  
  tsnsel<-bindlist(testlist[[1]][v])
  tsnsel<-tsnsel[,pcasellist1[[1]]]
  
  outsel<-testlist[[1]][[esclusa]]
  outsel<-outsel[,pcasellist1[[1]]]
  
  for (ii in 2:29){
    trnseli<-bindlist(trainlist[[ii]])
    trnsel<-cbind(trnsel,trnseli[,pcasellist1[[ii]]])
    
    thnseli<-bindlist(threshlist[[ii]])
    thnsel<-cbind(thnsel,thnseli[,pcasellist1[[ii]]])
    
    tsnseli<-bindlist(testlist[[ii]][v])
    tsnsel<-cbind(tsnsel,tsnseli[,pcasellist1[[ii]]])
    
    outseli<-testlist[[ii]][[esclusa]]
    outsel<-cbind(outsel,outseli[,pcasellist1[[ii]]])
    
  }
  
  testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
  traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
  threshtarget<-data.frame(lapply(thnsel,factor,levels=c("0","1","2")))
  testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))
  
  razza<-as.factor(rep(v,times=unlist(lapply(lidtr,length))[-esclusa]))
  razzac<-rep(v,times=unlist(lapply(lidtr,length))[-esclusa])
  razzathresh<-as.factor(rep(v,times=unlist(lapply(lidth,length))[-esclusa]))
  razzathreshc<-rep(v,times=unlist(lapply(lidth,length))[-esclusa])
  razzatarget<-as.factor(rep(v,times=unlist(lapply(lidtt,length))[-esclusa]))
  razzatargetc<-rep(v,times=unlist(lapply(lidtt,length))[-esclusa])
  
#   ## rappresentazione grafica tramite prime due componenti principali
#     dAtA<-rbind(trnsel,thnsel,tsnsel,outsel)
#     pca.dAtA<-prcomp(dAtA)
#     col<-c(razzac,razzathreshc,razzatargetc,rep('grey45',dim(outsel)[1]))
#     col[col==1]<-'red3'
#     col[col==3]<-'royalblue3'
#     col[col==4]<-'lawngreen'
#     plot(pca.dAtA$x[,1:2],col=col,main='Principal component analysis plot (a)
#     -Nsel = 30 - Real data-')
#     legend("topleft", c('Italian Brown','Marchigiana','Italian Simmental','Italian Holstein'), title='breeds',pch=1,
#          col=c('red3','royalblue3','lawngreen','grey45'))
#   
#     ## rappresentazione grafica tramite prime due componenti principali
#       dAtA<-trnsel
#       pca.dAtA<-prcomp(dAtA)
#       pca.dAtA<-predict(pca.dAtA,newdata=rbind(dAtA,outsel))
#       col<-c(razzac,rep('grey45',dim(outsel)[1]))
#       col[col==1]<-'red3'
#       col[col==3]<-'royalblue3'
#       col[col==4]<-'lawngreen'
#       plot(pca.dAtA[,1:2],col=col,main='Principal component analysis plot (b)
#       -Nsel = 30 - Real data-')
#       legend("topleft", c('Italian Brown','Marchigiana','Italian Simmental','Italian Holstein'), title='breeds',pch=1,
#            col=c('red3','royalblue3','lawngreen','grey45'))
  
  outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
  proxmatrixtestTh[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,threshtarget),proximity=TRUE)$proximity
  proxmatrixtestT[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
  proxmatrixtestO[[ciclosel]]<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity
  
  
}

#calcolo delle med e mad relativo al campione completo
outthc<-list()
medthc<-list()
madthc<-list()
ntr<-as.numeric(table(razza))
a<-cumsum(ntr)
a<-c(0,a)
nth<-as.numeric(table(razzathresh))
b<-cumsum(nth)
b<-c(0,b)

for(c in 1:ciclosel){
  outthcj<-NULL
  medc<-NULL
  madc<-NULL

  for(l in 1:(length(a)-1)){
    vec<-apply((proxmatrixtestTh[[c]][(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
    outthcj<-c(outthcj,sum(ntr)/vec)
    medc<-c(medc,median(sum(ntr)/vec))
    madc<-c(madc,mad(sum(ntr)/vec))
  } 
  outthc[[c]]<-outthcj
  medthc[[c]]<-medc
  madthc[[c]]<-madc
}

proxmatrixtestS<-list()
a<-length(razza)
for(c in 1:ciclosel){
  print(paste("prox matrix S n.",c,"on 14"))
  proxmatrixtestS[[c]]<-cbind(proxmatrixtestT[[c]][(1:a),],proxmatrixtestO[[c]][(1:a),-(1:a)])
}

a<-cumsum(table(razza))
a<-c(0,a)
rawout<-list()
for(c in 1:ciclosel){
  rawoutc<-NULL
  print(paste("raw out matrix n.",c,"on 14"))
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
  for(k in 1:length(medthc[[c]])){
    med<-rbind(med,rep(medthc[[c]][k],d))
    mad<-rbind(mad,rep(madthc[[c]][k],d))
  }
  sout[[c]]<-(rawout[[c]]-med)/(mad)
}

soutematrixc<-list()
tabc<-list()
for(c in 1:ciclosel){
  soutematrixc[[c]]<-rbind(c(razzatargetc,rep(9,nrazze[esclusa])),sout[[c]])
  soutematrixc[[c]]<-soutematrixc[[c]][,order(soutematrixc[[c]][1,])]
  tabc[[c]]<-table(soutematrixc[[c]][1,])
  soutematrixc[[c]]<-apply(soutematrixc[[c]][2:(dim(soutematrixc[[c]])[1]),],2,min)
}

#valutazione performance del sistema
ss1<-list()
real<-c(rep("T",sum(unlist(lapply(lidtt,length))[-esclusa])),rep("O",nrazze[esclusa]))
for(c in 1:ciclosel){
  decided<-decision(soutematrixc[[c]],optpar[[c]]$minimum)
  confmat<-table(real,decided)
  sens<-confmat[2,2]/sum(confmat[2,])
  spec<-confmat[1,1]/sum(confmat[1,])
  ss1[[c]]<-1-(sens*spec)/(sens+spec) 
}

# rappresentazione grafica, ciclosel=14
index1<-cumsum(tabc[[14]])

plot(soutematrixc[[14]][1:index1[3]],col='darkorange2',xlim=c(0,328),ylim=c(-1,60),ylab='outlyingness value',main='Distribution of standardized outlier
     complete training
     -unique class, Real data-')
points(soutematrixc[[14]][(index1[3]+1):index1[4]],col='grey45')
legend("topright", c('classes outlier ','novelty'), pch=1, title='classes',
       col=c('darkorange2','grey45'))
abline(h=optpar[[14]]$minimum)
