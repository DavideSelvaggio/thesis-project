rm(list=ls())
library(randomForest)

######################################################outr####################################
generap<-function(nr,Fst,nsnp,seme){
  set.seed(seme)
  p<-runif(nsnp,min=0.1,max=0.5)
  pmatr<-matrix(NA,nsnp,nr)
  for (i in 1:nsnp){
    for (r in 1:nr){
      pmatr[i,r]<-rbeta(1,shape1=p[i]*(1-Fst)/(Fst),shape2=(1-p[i])*(1-Fst)/(Fst))
    }
  }
  pmatr
}

generasnp<-function(nrazze,pmatr,seme){
  set.seed(seme)
  nr<-length(nrazze)
  snp<-list()
  
  
  nsnp<-nrow(pmatr)
  for (r in 1:nr){
    snpr<-NULL
    for (i in 1:nsnp) snpr<-cbind(snpr,rbinom(nrazze[r],size=2,prob=pmatr[i,r]))
    snp[[r]]<-snpr
  }
  snp
}


bindlist<-function(x){
  y<-x[[1]]
  for (i in 2:length(x)){
    y<-rbind(y,x[[i]])
  }
  y
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


################################# passo 1)  ##############################################################
semep<-8764836                           #### parametro in input
semesnp<-475648					#### per generare repliche fissato pmatr
semeiniz<-34863

nr<-4
nrazze<-c(250,500,1000,1500)
esclusa<-1                           #### parametro in input
trfract<-.75                         #### parametro in input
Fst<-.05						#### parametro in input
nsnp<-500						#### parametro in input
lperm<-list()
lidtr<-list()

for (w in 1:nr){
  if (w == esclusa) {lperm[[w]]<-NULL
  lidtr[[w]]<-NULL}
  else{
    lperm[[w]]<-sample(1:nrazze[w],nrazze[w])
    lidtr[[w]]<-lperm[[w]][1:round(nrazze[w]*trfract)]
  }
}

pmatr<-generap(nr,Fst,nsnp,semep)
flist<-generasnp(nrazze,pmatr,semesnp)

trainlist<-list()
testlist<-list()
esclusalist<-list()

train<-list()
test<-list()
for (w in 1:nr){
  if (w == esclusa) {train[[w]]<-NULL
  test[[w]]<-flist[[w]]}
  else {
    train[[w]]<-flist[[w]][lidtr[[w]],]
    test[[w]]<-flist[[w]][-lidtr[[w]],]
  }
}

trainlist[[1]]<-train
testlist[[1]]<-test

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
listidlist1<-list()
listidlist2<-list()
listidlist3<-list()

semi<-42   #### parametro in input
fr<-c(.50,.75)								#### parametro in input

############### training random forest  ########################

tprimolist<-lapply(trainlist,qtprimo,v=(1:nr),ciclo=c(esclusa,esclusaOut))
rlist<-lapply(trainlist,qr,v=(1:nr),ciclo=esclusaOut)

  num<-unlist(lapply(tprimolist[[1]],nrow))
  rnum<-nrow(rlist[[1]][[1]])
  
    set.seed(semi)
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
    
    trnselj<-data.frame(trainstar[[1]])
    ttnselj<-data.frame(trainthresh[[1]])
    tsnselj<-data.frame(testtarget[[1]])
    outselj<-data.frame(rlist[[1]][[1]])
    
    trainstarj<-data.frame(lapply(trnselj,factor,levels=c("0","1","2")))
    trainthreshj<-data.frame(lapply(ttnselj,factor,levels=c("0","1","2")))
    testtargetj<-data.frame(lapply(tsnselj,factor,levels=c("0","1","2")))
    testoutj<-data.frame(lapply(outselj[ridlist,],factor,levels=c("0","1","2")))
    
    razzaj<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist1,length))))
    razzajthresh<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist2,length))))
    razzajtarget<-as.factor(rep((1:nr)[-c(esclusa,esclusaOut)],unlist(lapply(idlist3,length))))
    
    outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE)
    proxlisthj<-predict(outRFj,newdata=rbind(trainstarj,trainthreshj),proximity=TRUE)$proximity
    proxlistj<-predict(outRFj,newdata=rbind(trainstarj,testtargetj),proximity=TRUE)$proximity
    proxlisttj<-predict(outRFj,newdata=rbind(trainstarj,testoutj),proximity=TRUE)$proximity
    
  
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
  
#### inizio calcolo misure di outlyingness #### 
rawoutth<-list()
outth<-list()
medth<-list()
madth<-list()
  
  rawoutthj<-NULL
  razzaj<-razzajlist[[1]]
  ntr<-as.numeric(table(razzaj))
  a<-cumsum(ntr)
  a<-c(0,a)
  razzajthresh<-razzajthreshlist[[1]]
  nth<-as.numeric(table(razzajthresh))
  b<-cumsum(nth)
  b<-c(0,b)
  idlist<-listidlist2[[1]]
  
  for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlisth[[1]][(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
    rawoutthj<-c(rawoutthj,sum(ntr)/vec)
    
  }
  
  rawoutth[[1]]<-rawoutthj
  medth[[1]]<-as.numeric(by(rawoutth[[1]],razzajthresh,median))
  madth[[1]]<-as.numeric(by(rawoutth[[1]],razzajthresh,mad))
  medthrep<-rep(medth[[1]],times=unlist(lapply(idlist,length)))
  madthrep<-rep(madth[[1]],times=unlist(lapply(idlist,length)))
  outth[[1]]<-(rawoutth[[1]]-medthrep)/madthrep

#creo test* = test_target + test_out
  proxlistts<-list()

    
    razzaj<-razzajlist[[1]]
    a<-sum(table(razzaj))
    
    proxlistts[[1]]<-cbind(proxlist[[1]][(1:a),],proxlistt[[1]][(1:a),-(1:a)])
    



#calcolo degli outliers per il test set
rawoute<-list()
  
  razzaj<-razzajlist[[1]]
  matrix<-NULL
  a<-cumsum(table(razzaj))
  a<-c(0,a)
  
  for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlistts[[1]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxlistts[[1]])])^2,2,sum)
    matrix<-rbind(matrix,a[length(a)]/vec)
    
  }
  
  rawoute[[1]]<-matrix


soute<-list()

  
  d<-dim(rawoute[[1]])[2]
  med<-NULL
  mad<-NULL
  for(c in 1:length(medth[[1]])){
    med<-rbind(med,rep(medth[[1]][c],d))
    mad<-rbind(mad,rep(madth[[1]][c],d))
  }
  soute[[1]]<-(rawoute[[1]]-med)/(mad)
  

ind<-table(razzajthreshlist[[1]])
end<-sum(ind)
end2<-dim(soute[[1]])[2]
plot(density(soute[[1]][1,1:ind[1]]),col='red',ylim=c(0,.5),xlim=c(-3,50))
lines(density(soute[[1]][1,(ind[1]+1):end]),col='blue')
lines(density(soute[[1]][1,(end+1):end2]),col='orange')
lines(density(soute[[1]][2,(ind[1]+1):end]),col='green')
lines(density(soute[[1]][2,1:ind[1]]),col='violet')
lines(density(soute[[1]][2,(end+1):end2]),col='yellow')


plot(density(soute[[3]][2,189:(188+281)]),col='red',ylim=c(0,.6),xlim=c(-3,12),
     main='Distribution of standardized outlier
     - different loops')
lines(density(soute[[1]][1,1:281]),col='blue')
legend("topright", c('loop 1','loop 2'), title='class 3',lty=c(1,1),
       col=c('red','blue'))

plot(density(outth[[1]][1:ind[1]]),col='orange',ylim=c(0,.6),xlim=c(-3,12),
     main='Distribution of standardized outlier
     - different sets')
lines(density(soute[[1]][1,1:ind[1]]),col='blue')
legend("topright", c('training set 2','test set'), title='class 2',lty=c(1,1),
       col=c('orange','blue'))

plot(density(soute[[1]][2,(ind[1]+1):end]),col='red',ylim=c(0,.45),xlim=c(-3,50),
     main='Distribution of class outlier 
     and novelty outlier
     -Fts score = 0.05-')
lines(density(soute[[1]][2,1:ind[1]]),col='grey')
legend("topright", c('class outlier ','novelty (class 3)'), title='class 2',lty=c(1,1),
       col=c('red','grey','black'))

## decision rule

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

parmat<-list()
optpar<-list()

  parmat[[1]]<-matrix(NA,numiniz,dimso+1)
  for (u in 1:numiniz){
    print(paste("c=", 1, "u=", u))
    outmin<-optim(inizpar[u,],funobb,soute=soute[[1]],cl=cl[[1]])
    if (outmin$convergence==0) parmat[[1]][u,]<-c(outmin$par,outmin$value)
  }
  optpar[[1]]<-parmat[[1]][which.min(parmat[[1]][,dimso+1]),]


plot(density(soute[[1]][1,(1:93)]),col='red',ylim=c(0,.45),xlim=c(-3,45),
     main='Distribution of class outlier
     and novelty outlier 2
     -Fts score = 0.04-')
lines(density(soute[[2]][1,(188+375):dim(soute[[2]])[2]]),col='grey')
lines(density(soute[[2]][2,(188+375):dim(soute[[2]])[2]]),col='black')
legend("topright", c('class outlier ','novelty (class 3)','novelty (class 3)'), title='class 2',lty=c(1,1),
       col=c('red','grey','black'))
abline(v=4.8114788)

plot(soute[[1]][1,end:end2],col='grey',main='Distribution of class outlier
     and novelty outlier
     -Fts score = 0.05-',ylab='outlyingness value',xlim=c(-1,300),ylim=c(-3,70))
#points((soute[[1]][2,(188+375):dim(soute[[2]])[2]]),col='grey')
points((soute[[1]][1,1:ind[1]]),col='red')
legend("topright", c('class outlier ','novelty'), title='class 2', pch=1,
       col=c('red','grey'))
abline(h=optpar[[1]][1])


##grafico 2

plot(soute[[1]][1,1:ind[1]],soute[[1]][2,1:ind[1]],col='blue',main='Distribution of standardized outlier
     between classes
detail 2',xlab='outlyingness value class 3',ylab='outlyingness value class 2',xlim=c(3,200),ylim=c(0,7) )
points(soute[[1]][1,(ind[1]+1):end],soute[[1]][2,(ind[1]+1):end],col='red')
points(soute[[1]][1,(end+1):end2],soute[[1]][2,(end+1):end2],col='grey')
abline(h=optpar[[1]][1])
abline(v=optpar[[1]][2])
legend("topright", c('class 2 ','class 3','novelty'), title='classes', pch=1,
       col=c('red','blue','grey'))


### TRAINING TOTALE
rz<-(1:nr)[-esclusaOut]
vv<-(1:nr)[-c(esclusa,esclusaOut)]

trnsel<-data.frame(bindlist(trainlist[[1]][rz]))
tsnsel<-data.frame(bindlist(testlist[[1]][vv])) 
outsel<-data.frame(testlist[[1]][[esclusa]]) 

testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))

razza<-as.factor(rep(vv,times=unlist(lapply(lidtr,length))[-c(esclusa,esclusaOut)]))
numtarget<-(nrazze-unlist(lapply(lidtr,length)))[-c(esclusa,esclusaOut)]
razzatarget<-as.factor(rep(vv,times=numtarget))
outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
proxmatrixtestT<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
proxmatrixtestO<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity


proxmatrixtestS<-NULL
a<-sum(table(razza))
proxmatrixtestS<-cbind(proxmatrixtestT[(1:a),],proxmatrixtestO[(1:a),-(1:a)])

#calcolo degli outliers per il test set
rawout<-NULL
a<-cumsum(table(razza))
a<-c(0,a)

for(l in 1:(length(a)-1)){
  
  vec<-apply((proxmatrixtestS[(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxmatrixtestS)])^2,2,sum)
  rawout<-rbind(rawout,a[length(a)]/vec)
  
}


sout<-NULL

d<-dim(rawout)[2]
med<-NULL
mad<-NULL
for(c in 1:length(medth[[1]])){
  med<-rbind(med,rep(medth[[1]][c],d))
  mad<-rbind(mad,rep(madth[[1]][c],d))
}
sout<-(rawout-med)/(mad)

decided<-decision(sout,optpar[[1]][1:2])
real<-c(rep("T",sum(numtarget)),rep("O",(dim(sout)[2]-sum(numtarget))))
  
confmat<-table(real,decided)
sens<-confmat[2,2]/sum(confmat[2,])
spec<-confmat[1,1]/sum(confmat[1,])
ss1<-1-(sens*spec)/(sens+spec)  

##rappresentazione grafica

index1<-cumsum(c(numtarget,(dim(sout)[2]-sum(numtarget))))
index2<-cumsum(as.numeric(table(decided)))
plot(sout[1,(index1[1]+1):index1[2]],col='blue',ylim=c(-5,1300),ylab='outlyingness value',main='Distribution of standardized outlier
     complete training')
points(sout[1,(index1[2]+1):index1[3]],col='grey')
points(sout[1,1:index1[1]],col='red')
legend("topright", c('class 2 ','class 3','novelty'), title='class 2', pch=1,
       col=c('red','blue','grey'))
abline(h=optpar[[1]][1])

##detail 1
plot(sout[1,(index1[1]+1):index1[2]],col='blue',ylim=c(-3,250),xlim=c(0,100),ylab='outlyingness value',main='Distribution of standardized outlier
     complete training -Detail 1-')
points(sout[1,(index1[2]+1):index1[3]],col='grey')
points(sout[1,1:index1[1]],col='red')
legend("topright", c('class 2 ','class 3','novelty'), title='class 2', pch=1,
       col=c('red','blue','grey'))
abline(h=optpar[[1]][1])

