## Adattamento dei risultati riscontrati nell script1_differenze.. ad un problema completo di simulazione.

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

## decision rule

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

################################# passo 1)  ##############################################################
semep<-c(8764836,4887636,8647368,6836487)                         #### parametro in input
semesnp<-475648					#### per generare repliche fissato pmatr
semeiniz<-34863

nr<-4
nrazze<-c(450,450,450,450)
trfract<-c(0.60,0.80)                         #### parametro in input
Fst<-c(0.015, 0.035, 0.05, 0.08)		#0.02			#### parametro in input
nsnp<-500						#### parametro in input
pmatr<-NULL
ss1<-list()
CompuTime<-list()
threshold<-list()
##########################################################
# for(i in 1:nr){                                        #
#   pmatr<-cbind(pmatr,generap(1,Fst[i],nsnp,semep[1]))  #
# }                                                      #
# flist<-generasnp(nrazze,pmatr,semesnp)                 #
##########################################################
for(rf in 1:nr){
  print(paste('rf n.',rf,'on 4'))
#############################################################  
      pmatr<-generap(nr,Fst[rf],nsnp,semep[rf])             #
      flist<-generasnp(nrazze,pmatr,semesnp)                # 
#############################################################  
esclusa<-rf 
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

trainlist<-list()
threshlist<-list()
testlist<-list()
esclusalist<-list()

train<-list()
thresh<-list()
test<-list()
for (w in 1:nr){
  if (w == esclusa) {train[[w]]<-NULL
  thresh[[w]]<-NULL
  test[[w]]<-flist[[w]]}
  else {
    train[[w]]<-flist[[w]][lidtr[[w]],]
    thresh[[w]]<-flist[[w]][lidth[[w]],]
    test[[w]]<-flist[[w]][lidtt[[w]],]
  }
}

trainlist[[1]]<-train
threshlist[[1]]<-thresh
testlist[[1]]<-test

######################################### fine passo 1) e inizio passo 2) ##############################################################


#lnr<-lapply(lidtr,length)
v<-(1:nr)[-esclusa]
k<-length(v) # nr classi presenti nel training

############### ciclo per i da 1 a k   ########################
#ptrain<-list()
#ptarget<-list()
#cltarget<-list()
proxlist<-list()
#proxlistr<-list()
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
fr<-c(.50,.75)								#### parametro in input
ciclo<-0
############### ciclo per j da 1 a m   ########################
for(i in v){
  ciclo<-ciclo+1
  print(paste('razza esclusa=',i))
  tprimolist<-lapply(trainlist,qtprimo,v=v,ciclo=ciclo) #da sistemare questa lista, noi abbiamo una sola lista che raggruppa tutti i cromosomi.
  rlist<-lapply(trainlist,qr,v=v,ciclo=ciclo)
  
  num<-unlist(lapply(tprimolist[[1]],nrow))
  rnum<-nrow(rlist[[1]][[1]])
  for (j in 1:m){
    set.seed(semi[ciclo,j])
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
    #ogg<-list()
    #ogg[[1]]<-trainlist[[1]][v]
    #trainstar<-lapply(ogg,star,idlist=idlist)
    #testtarget<-lapply(ogg,starmeno,idlist=idlist)
    trainstar<-lapply(tprimolist,star,idlist=idlist1)
    trainthresh<-lapply(tprimolist,star,idlist=idlist2)
    testtarget<-lapply(tprimolist,star,idlist=idlist3)
    
    # su questo lavorerà la RF per stimare le prob a posteriori
    trnselj<-data.frame(trainstar[[1]])
    ttnselj<-data.frame(trainthresh[[1]])
    tsnselj<-data.frame(testtarget[[1]])
    outselj<-data.frame(rlist[[1]][[1]])
    
    testoutj<-data.frame(lapply(outselj[ridlist,],factor,levels=c("0","1","2")))
    trainstarj<-data.frame(lapply(trnselj,factor,levels=c("0","1","2")))
    trainthreshj<-data.frame(lapply(ttnselj,factor,levels=c("0","1","2")))
    testtargetj<-data.frame(lapply(tsnselj,factor,levels=c("0","1","2")))
    
    razzaj<-as.factor(rep(v[-ciclo],unlist(lapply(idlist1,length))))
    #numtarget<-num-unlist(lapply(idlist,length))
    razzajthresh<-as.factor(rep(v[-ciclo],unlist(lapply(idlist2,length))))
    razzajtarget<-as.factor(rep(v[-ciclo],unlist(lapply(idlist3,length))))
    razzajtargetl<-(rep(v[-ciclo],unlist(lapply(idlist3,length))))
    #outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE,oob.prox=TRUE)
    outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE)
    #proxlistrj<- predict(outRFj,newdata=trainstarj,proximity=TRUE)$proximity
    #proxlistrj<-outRFj$proximity
    #ptrain[[j]]<-predict(outRFj,type="prob")
    #ptarget[[j]]<-predict(outRFj,newdata=testtargetj,type="prob")
    #cltarget[[j]]<-predict(outRFj,newdata=testtargetj,type="class")
    #pout[[j]]<-predict(outRFj,newdata=testoutj,type="prob")
    proxlisthj<-predict(outRFj,newdata=rbind(trainstarj,trainthreshj),proximity=TRUE)$proximity
    proxlistj<-predict(outRFj,newdata=rbind(trainstarj,testtargetj),proximity=TRUE)$proximity
    proxlisttj<-predict(outRFj,newdata=rbind(trainstarj,testoutj),proximity=TRUE)$proximity
    
  } # fine ciclo in j per le repliche
  
  listidlist1[[ciclo]]<-idlist1
  listidlist2[[ciclo]]<-idlist2
  razzajlist[[ciclo]]<-razzaj
  razzajtargetlist[[ciclo]]<-razzajtargetl
  razzajthreshlist[[ciclo]]<-razzajthresh
  proxlist[[ciclo]]<-proxlistj
  proxlisth[[ciclo]]<-proxlisthj
  #proxlistr[[ciclo]]<-proxlistrj
  proxlistt[[ciclo]]<-proxlisttj
  rridlist[[ciclo]]<-length(ridlist)
  
}

#ho verificato se le varie matrici di prossimità calcolate per training set, test set e unione dei due insiemi 
#hanno una diagonale unitaria, e dalle mie verifiche sembrerebbe di sì.



#x<-(density(outtrtr[razza==2])$x)
#y<-(density(outtrtr[razza==2])$y)
#alpa<-x[2] - x[1]
#c<-seq(median(x),range(x)[2],by=.01)

#for(i in c){
#  sum(y[x>i]*alpa1)*100
#}

#calcolo degli outlier partendo dalla matrice di prossimità
#NB: UNA VOLTA CHE AGGIUNGI IL CICLO SULLE RAZZE O AUMENTI IL NUMERO DI CICLI DI J DA 1 A M, RICORDA DI INDICIZZARE RAZZAJ

# calcolo degli outliers per il training set
#rawouttr<-list()
#outtr<-list()
#medtr<-list()
#madtr<-list()

#for(j in 1:k){

#  rawouttrj<-NULL
#  razzaj<-razzajlist[[j]]
#  idlist<-listidlist1[[j]]

#  for (col in 1:ncol(proxlistr[[j]])){
#    rawouttrj<-c(rawouttrj,nrow(proxlistr[[j]])/sum(proxlistr[[j]][razzaj==razzaj[col],col]^2))
#  }

#rawouttr[[j]]<-rawouttrj
#medtr[[j]]<-as.numeric(by(rawouttr[[j]],razzaj,median))
#madtr[[j]]<-as.numeric(by(rawouttr[[j]],razzaj,mad))
#medtrrep<-rep(medtr[[j]],times=unlist(lapply(idlist,length)))
#madtrrep<-rep(madtr[[j]],times=unlist(lapply(idlist,length)))
#outtr[[j]]<-(rawouttr[[j]]-medtrrep)/madtrrep

#}

#out.tr<-outlier(proxlistr[[1]],cls=razzajlist[[1]])
#sum(outtr[[1]]==out.tr)

print('calcolo degli outliers per il training set delle soglie')
rawoutth<-list()
outth<-list()
medth<-list()
madth<-list()

for(j in 1:k){
  
  rawoutthj<-NULL
  medthj<-NULL
  madthj<-NULL
  razzaj<-razzajlist[[j]]
  ntr<-as.numeric(table(razzaj))
  a<-cumsum(ntr)
  a<-c(0,a)
  razzajthresh<-razzajthreshlist[[j]]
  nth<-as.numeric(table(razzajthresh))
  b<-cumsum(nth)
  b<-c(0,b)
  idlist<-listidlist2[[j]]
  
  for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlisth[[j]][(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
    rawoutthj<-c(rawoutthj,sum(ntr)/vec)
    medthj<-c(medthj,median(sum(ntr)/vec))
    madthj<-c(madthj,mad(sum(ntr)/vec))
  }
  
  rawoutth[[j]]<-rawoutthj
  medth[[j]]<-medthj
  madth[[j]]<-madthj

}

print('creo test* = test_target + test_out')
proxlistts<-list()
for(j in 1:k){
  
  razzaj<-razzajlist[[j]]
  a<-sum(table(razzaj))
  
  proxlistts[[j]]<-cbind(proxlist[[j]][(1:a),],proxlistt[[j]][(1:a),-(1:a)])
  
}


print('calcolo degli outliers per il test set')
rawoute<-list()
for(j in 1:k){
  
  razzaj<-razzajlist[[j]]
  matrix<-NULL
  a<-cumsum(table(razzaj))
  a<-c(0,a)
  
  for(l in 1:(length(a)-1)){
    
    vec<-apply((proxlistts[[j]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxlistts[[j]])])^2,2,sum)
    matrix<-rbind(matrix,a[length(a)]/vec)
    
  }
  
  rawoute[[j]]<-matrix
  
}

soute<-list()
for(j in 1:k){
  
  d<-dim(rawoute[[j]])[2]
  med<-NULL
  mad<-NULL
  for(c in 1:length(medth[[j]])){
    med<-rbind(med,rep(medth[[j]][c],d))
    mad<-rbind(mad,rep(madth[[j]][c],d))
  }
  soute[[j]]<-(rawoute[[j]]-med)/(mad)
  
}



# ## verifica grafica dei risultati per la razza 3
# # da cambiare il titolo del plot e da aggiungere la legenda
# plot(density(soute[[3]][2,189:(188+281)]),col='red',ylim=c(0,.5),xlim=c(-3,25))
# lines(density(soute[[3]][2,1:188]),col='blue')
# lines(density(soute[[1]][1,1:281]),col='blue')
# lines(density(soute[[1]][2,1:281]),col='green')
# lines(density(soute[[3]][1,189:(188+281)]),col='yellow')
# lines(density(soute[[2]][1,(188+375):dim(soute[[2]])[2]]),col='orange')
# lines(density(soute[[1]][1,(281+375):dim(soute[[1]])[2]]),col='grey')
# lines(density(soute[[3]][1,(281+188):dim(soute[[3]])[2]]),col='black')
# ## le distribuzioni delle misure di outlyingness dei punti del test set relative alle razze conosciute, calcolate con riferimenti a diverse razze combinazioni di razze sconoiute, si sovrappongono.
# ## posso quindi considerarle congiuntamente nella decision rule.
# plot(density(soute[[3]][2,189:(188+281)]),col='red',ylim=c(0,.6),xlim=c(-3,12),
#       main='Distribution of standardized outlier
#      - different loops')
# lines(density(soute[[1]][1,1:281]),col='blue')
# legend("topright", c('loop 1','loop 2'), title='class 3',lty=c(1,1),
#        col=c('red','blue'))
# 
# plot(density(outth[[1]][1:282]),col='orange',ylim=c(0,.6),xlim=c(-3,12),
#      main='Distribution of standardized outlier
#      - different sets')
# lines(density(soute[[1]][1,1:281]),col='blue')
# legend("topright", c('training set 2','test set'), title='class 3',lty=c(1,1),
#        col=c('orange','blue'))
# 
# plot(density(soute[[3]][2,189:(188+281)]),col='red',ylim=c(0,.45),xlim=c(-3,50),
#      main='Distribution of class outlier 
#      and novelty outlier
#      -Fts score = 0.08-')
# lines(density(soute[[1]][1,(281+375):dim(soute[[1]])[2]]),col='grey')
# lines(density(soute[[3]][1,(281+188):dim(soute[[3]])[2]]),col='black')
# legend("topright", c('class outlier ','novelty 1 (class 2)','novelty 2 (class 4)'), title='class 3',lty=c(1,1),
#        col=c('red','grey','black'))

print('accorpamento delle matrici soute')

soutematrix<-NULL
for(j in 1:k){
  soutematrix<-cbind(soutematrix,rbind(c(razzajtargetlist[[j]],rep(9,rridlist[[j]])),soute[[j]]))
}

second.min<-function(x){
  n <- length(x)
  sort(x)[2]
}

soutematrix<-soutematrix[,order(soutematrix[1,])]
tab<-table(soutematrix[1,])
soutematrix<-soutematrix[-1,]
soutematriy<-apply(soutematrix,2,second.min)
soutematrix<-apply(soutematrix,2,min)

### rappresentazione grafica

# plot(soutematrix,soutematriy,col=rep(c('red3','royalblue3','yellowgreen','darkgrey'),tab),
#      main='Distribution of minimum values',xlab='1st minimum value',ylab='2nd minimum value')
# legend("topright", c('class 2','class 3','class 4','novelty'), title='classes',pch=1,
#        col=c('red3','royalblue3','yellowgreen','darkgrey'))




num<-sum(unlist(lapply(razzajtargetlist,length)))
cl<-c(rep("T",num),rep("O",sum(unlist(rridlist))))


#numiniz<-10
#dimso<-2
#inizpar<-matrix(NA,numiniz,dimso)   ### numiniz da dare in input

#for(u in 1:numiniz){
#  inizpar[u,]<-runif(dimso)
#}

#parmat<-NULL
#optpar<-NULL

#  parmat<-matrix(NA,numiniz,dimso+1)
#  for (u in 1:numiniz){
#    print(paste("c=unique", "u=", u))
timeA<-Sys.time()
outmin<-optimize(funobb,soute=soutematrix,cl=cl,interval=range(soutematrix))
#    if (outmin$convergence==0) parmat[u,]<-c(outmin$par,outmin$value)
#  }
#  optpar<-parmat[which.min(parmat[,dimso+1]),]
optpar<-outmin
timeB<-Sys.time()
CompuTime[[rf]]<-(timeB-timeA)
#plot(density(soute[[2]][1,(1:188)]),col='red',ylim=c(0,.45),xlim=c(-3,45),
#     main='Distribution of class outlier
#     and novelty outlier 2
#     -Fts score = 0.08-')
#lines(density(soute[[2]][1,(188+375):dim(soute[[2]])[2]]),col='grey')
#lines(density(soute[[2]][2,(188+375):dim(soute[[2]])[2]]),col='black')
#legend("topright", c('class outlier ','novelty (class 3)','novelty (class 3)'), title='class 2',lty=c(1,1),
#       col=c('red','grey','black'))
##rappresentazione grafica

index<-cumsum(tab)

# plot(density(soutematrix[1:index[1]]),col='green',ylim=c(0,.5),xlim=c(-3,25),main='Distribution of standardized outlier
#      -classes comparison-')
# lines(density(soutematrix[(index[1]+1):index[2]]),col='red')
# lines(density(soutematrix[(index[2]+1):index[3]]),col='blue')
# lines(density(soutematrix[(index[3]+1):index[4]]),col='grey')
# legend("topright", c('class 2','class 3','class 4','novelty'), title='classes',lty=c(1,1),
#        col=c('green','red','blue','grey'))
# abline(v=optpar$minimum)
# 
# plot((soutematrix[(index[3]+1):index[4]]),col='grey',ylim=c(-1,25),main='Distribution of class outlier
#      and novelty outlier
#      -unique class distribution-',ylab='outlyingness value')
# points((soutematrix[1:index[3]]),col='orange')
# legend("topright", c('classes outlier ','novelty'), pch=1, title='classes',
#        col=c('orange','grey'))
# abline(h=optpar$minimum)


# training completo

trnsel<-data.frame(bindlist(trainlist[[1]]))
thnsel<-data.frame(bindlist(threshlist[[1]]))
tsnsel<-data.frame(bindlist(testlist[[1]][v])) 
outsel<-data.frame(testlist[[1]][[esclusa]]) 

testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
threshtarget<-data.frame(lapply(thnsel,factor,levels=c("0","1","2")))
testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))

razza<-as.factor(rep(v,times=unlist(lapply(lidtr,length))[-esclusa]))
razzathresh<-as.factor(rep(v,times=unlist(lapply(lidth,length))[-esclusa]))
razzatarget<-as.factor(rep(v,times=unlist(lapply(lidtt,length))[-esclusa]))
razzatargetc<-rep(v,times=unlist(lapply(lidtt,length))[-esclusa])

outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
proxmatrixtestTh<-predict(outRF,newdata=rbind(traintarget,threshtarget),proximity=TRUE)$proximity
proxmatrixtestT<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
proxmatrixtestO<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity


#calcolo delle med e mad relativo al campione completo
outthc<-list()
medthc<-list()
madthc<-list()

outthcj<-NULL
medc<-NULL
madc<-NULL
ntr<-as.numeric(table(razza))
a<-cumsum(ntr)
a<-c(0,a)
nth<-as.numeric(table(razzathresh))
b<-cumsum(nth)
b<-c(0,b)

for(l in 1:(length(a)-1)){
  
  vec<-apply((proxmatrixtestTh[(a[l]+1):a[l+1],sum(ntr,b[l],1):sum(ntr,b[l+1])])^2,2,sum)
  outthcj<-c(outthcj,sum(ntr)/vec)
  medc<-c(medc,median(sum(ntr)/vec))
  madc<-c(madc,mad(sum(ntr)/vec))
  
}

outthc[[1]]<-outthcj
medthc[[1]]<-medc
madthc[[1]]<-madc


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
for(c in 1:length(medthc[[1]])){
  med<-rbind(med,rep(medthc[[1]][c],d))
  mad<-rbind(mad,rep(madthc[[1]][c],d))
}
sout<-(rawout-med)/(mad)

soutematrixc<-rbind(c(razzatargetc,rep(9,nrazze[esclusa])),sout)

soutematrixc<-soutematrixc[,order(soutematrixc[1,])]
tab<-table(soutematrixc[1,])
soutematrixc<-apply(soutematrixc[2:(dim(soutematrixc)[1]),],2,min)

decided<-decision(soutematrixc,optpar$minimum)
real<-c(rep("T",sum(unlist(lapply(lidtt,length))[-esclusa])),rep("O",nrazze[esclusa]))

confmat<-table(real,decided)
sens<-confmat[2,2]/sum(confmat[2,])
spec<-confmat[1,1]/sum(confmat[1,])
ss1[[rf]]<-1-(sens*spec)/(sens+spec)
threshold[[rf]]<-optpar$minimum

##rappresentazione grafica
# 
# colors1<-decided
# colors1[colors1=='T']<-'darkorange2'
# colors1[colors1=='O']<-'grey45'
# colors2<-c(razzatargetc,rep('grey45',dim(testout)[1]))
# colors2[colors2==1]<-'red3'
# colors2[colors2==2]<-'royalblue3'
# colors2[colors2==3]<-'lawngreen'
# colors2[colors2==4]<-'yellow2'
# index1<-cumsum(tab)
# main1<-paste('Correct class values
# escluded class',rf)
# main2<-paste('Decided values
# escluded class',rf)
# plot(soutematrixc,col=colors2,ylab='outlyingness value',main=main1)
# abline(h=optpar$minimum)
# plot(soutematrixc,col=colors1,ylab='outlyingness value',main=main2)
# abline(h=optpar$minimum)
}


