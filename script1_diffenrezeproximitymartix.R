#Questa prima del progetto serve ad indagare la dipendenza della matrice di prossimità ad 
#alcuni parametri del modello, quali grandezza del trainig set e valore del Fixation Index.
#
#Dovendo studiare un comportamento generale di una random forest, l'indagine sarà condotta 
#su dati simulati.I dati vengono simulati secondo il metodo descritto da Alkes L Price, 
#Nick J Patterson, Robert M Plenge, Michael E Weinblatt, Nancy A Shadick & David Reich nel loro
#paper Principal components analysis corrects for stratification in genome-wide association studies.

#NB il seguente codice è stato scritto in modo da funzionare per matrici, il seguente codice
#è un test necessario a verificare l'effettiva dipendenza dell'indice di prossimità alla grandezza
# del training set.

rm(list=ls())
library(randomForest)

#######funzioni utili######

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
  snp<-NULL

  nsnp<-nrow(pmatr)
  for (r in 1:nr){
    snpr<-NULL
    for (i in 1:nsnp) snpr<-cbind(snpr,rbinom(nrazze[r],size=2,prob=pmatr[i,r]))
    snp<-rbind(snp,snpr)
  }
  snp
}

#bindlist<-function(x){
#  y<-x[[1]]
#  for (i in 2:length(x)){
#    y<-rbind(y,x[[i]])
#  }
#  y
#}


#star<-function(xjj,idlist){ #da qui viene fuori una matrice che contiene le righe selezionate da idlist in ogni razza, un blocco sotto l'altro
#  g<-NULL
#  for (i in 1:length(xjj)){
#    gi<-xjj[[i]]
#    g<-rbind(g,gi[idlist[[i]],])
#  }
#  g
#}

#starmeno<-function(xjj,idlist){ #da qui viene fuori una matrice che contiene le righe NON selezionate da idlist in ogni razza, un blocco sotto l'altro
#  g<-NULL
#  for (i in 1:length(xjj)){
#    gi<-xjj[[i]]
#    g<-rbind(g,gi[-idlist[[i]],])
#  }
#  g
#}


################################# passo 1)  ##############################################################
RF<- 4                                      #numero di random forest che si vogliono trainare
semep<-seq(from=1,by=42,length.out=RF)                           #### parametro in input
semesnp<-seq(from=1231,by=42,length.out=RF)					#### per generare repliche fissato pmatr
semeiniz<-seq(from=511,by=42,length.out=RF)



nr<-2
nrazzem<-matrix(c(50,50,150,150,450,450,900,900),4,2,byrow=T)
trfract<-c(.25,.50,.60,.75)                        #### parametro in input
Fst<-0.02				#### parametro in input
nsnp<-500					#### parametro in input

y.plot<-NULL
x.plot <- NULL
y.outlier<-NULL

par(mfrow=c(2,2))

outendlist<-list()
ntree<-matrix(NA,RF,RF)

for(nrz in 1:RF){

  nrazze<-nrazzem[nrz,]
  outlist<-list()
  
  for(rf in 1:RF){
  
lidtr<-NULL
lperm<-NULL
c<-0
nid<-0
set.seed(semeiniz[rf])

for (w in 1:nr){
  
  c<-c+length(lperm)
  lperm<-sample(1:nrazze[w],nrazze[w])
  lperm<-lperm+c                             #traslazione degli indici campionati
  lidtr<-c(lidtr,(lperm[1:round(nrazze[w]*trfract[rf])]))
  nid[w]<-length(lperm[1:round(nrazze[w]*trfract[rf])])
}

pmatr<-generap(nr,Fst,nsnp,semep[rf])
flist<-generasnp(nrazze,pmatr,semesnp[rf])

set.seed(semeiniz[rf])

train<-NULL
test<-NULL

    train<-flist[lidtr,]
    test<-flist[-lidtr,]


#trainlist[[1]]<-train
#testlist[[1]]<-test

######################################### fine passo 1) e inizio passo 2) ##############################################################


#lnr<-lapply(lidtr,length)
v<-(1:nr)
#k<-length(v) # nr classi presenti nel training


#ptrain<-list()
#ptarget<-list()
#cltarget<-list()
                                              
#semi<-257  #### parametro in input


num<-nrow(train)

## NB: salto la parte successiva perchè non è essenziale alla costruzione di una RF, ma è parte
# del procedimento descritto da Benso.

#  set.seed(semi)
#  idlist<-list()
#  for(jj in 1:length(num)){
#   idlist[[jj]]<-sample(1:num[jj],num[jj])
#      }
#  ogg<-list()
#  ogg[[1]]<-trainlist[[1]][v]
#  trainstar<-lapply(ogg,star,idlist=idlist)
#  testtarget<-lapply(ogg,starmeno,idlist=idlist)		
  
  #R1<-prcomp(t(train))
  #R1<-R1$x[,1:2]
  #plot(R1)
  
  # su questo lavorerà la RF per stimare le prob a posteriori
  trnselj<-data.frame(train)
  tsnselj<-data.frame(test)
  #outselj<-data.frame(rlist[[1]][[1]])
  
  #testoutj<-data.frame(lapply(outselj[ridlist,],factor,levels=c("0","1","2")))
  trainstarj<-data.frame(apply(trnselj,2,factor,levels=c("0","1","2")))
  testtargetj<-data.frame(apply(tsnselj,2,factor,levels=c("0","1","2")))
  
  razzaj<-as.factor(rep(v,times=nid))
  #numtarget<-num-unlist(lapply(idlist,length))
  #razzajtarget<-as.factor(rep(v,times=numtarget))
#outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE,oob.prox=TRUE)
outRFj<-randomForest(x=trainstarj,y=razzaj,proximity=TRUE)
  #ptrain<-predict(outRFj,type="prob")
  #ptarget<-predict(outRFj,newdata=testtargetj,type="prob")
  #cltarget<-predict(outRFj,newdata=testtargetj,type="class")
  #pout[[j]]<-predict(outRFj,newdata=testoutj,type="prob")

ntree[nrz,rf]<-mean(treesize(outRFj,terminal=FALSE))
#prox<-predict(outRFj,newdata=rbind(trainstarj,testtargetj),type="prob",proximity=TRUE)
proxtrain<-outRFj$proximity # in questa non c'è l'1 di una unità con se stessa. ma come no??
#proxtrain<-prox$proximity[1:nrow(trainstarj),1:nrow(trainstarj)]

## 1° metodo per il calcolo dei pmed
pimed <- NULL

for(h in 1:length(nid)){
  
  if(h==1){
    
    pimed[h] <- sum(sum(proxtrain[1:nid[h],1:nid[h]]))/(nid[h])^2
  
    }else{ 
        
      pimed[h] <- sum(sum(proxtrain[(sum(nid[1:(h-1)])+1):sum(nid[1:h]),(sum(nid[1:(h-1)])+1):sum(nid[1:h])]))/(nid[h])^2
    
       }
        
}  

#print('pimed value:')
#print(pimed)

#si potrebbe evitare il ciclo for usando un po' di algebra.

#print('algebric method:')

Z<-matrix(0,sum(nid),nr)

#for(i in 1:nr){
#  if(i==1){Z[1:nid[i],i]<-1}
#  else{
#    Z[(sum(nid[1:(i-1)])+1):sum(nid[1:i]),i]<-1
#  }
#}

for(w in 1:nr){
  Z[razzaj==w,w]<-1
}
  


algebric.pimed<-(diag(t(Z)%*%proxtrain%*%Z)/(nid^2))

if(sum(round(algebric.pimed - pimed) != rep(0,length(pimed)))) {
  print('Warning: something went wrong')
  print('Ciclo:')
  print(nrz)
  print('Pimed:')
  print(pimed)
  print('Algebric Pimed:')
  print(algebric.pimed)
  }


y.plot[rf] <- mean(pimed) #come indice di sintesi uso una media aritmetica
x.plot[rf] <- sum(nid)

outl <- outlier(outRFj,cls=razzaj)
outlist[[rf]]<-outl


outmed<-NULL
for(i in 1:nr){
  outmed[i]<-median(outl[razzaj==i])
}  #solo nell'ultima impostazione utilizzo il valore mediano, anzichè il valore medio. I risultati
# si stabilizzano intorno allo 0. 

y.outlier[rf] <-  mean(outmed) #ho utlizzato qua una sintesi delle misure di outline standardizzate

#cerco di ricavare i valori non standardizzati di outline
proxtrainsq<-proxtrain^2
rawout<-NULL
rawoutdif<-NULL
for(p in 1:nr){
rawout<-c(rawout,apply(proxtrainsq[razzaj==p,razzaj==p],2,sum))
rawoutdif<-c(rawout,apply(proxtrainsq[razzaj!=p,razzaj==p],2,sum))
}

rawout<-sum(nrazze)/rawout
rawoutdif<-sum(nrazze)/rawoutdif

}

plot(x.plot,y.plot,type='b',ylim=c(min(y.outlier),1.1),col='red',pch=17,
     main=paste('Sample Size',sum(nrazze)),xlab='training set size',ylab='Index score')
points(x.plot,y.outlier,type='b',col='blue',pch=17)
abline(a=1,b=0,col='green')
legend("topright", c('Proximity', 'Outlier', 'Max'),lty=c(1,1),
       col=c('red','blue','green'))

outendlist[[nrz]]<-outlist

}

par(mfrow=c(1,1))

plot(ntree[1,],ylim=range(ntree),col='blue', xlab='rf', ylab='average tree size',
     type='b',pch=19)
points(ntree[2,],col='red',type='b',pch=19)
points(ntree[3,],col='green',type='b',pch=19)
points(ntree[4,],col='yellow',type='b',pch=19)

legend("topleft", c('1','2','3','4'), title='nrz',lty=c(1,1),
       col=c('blue','red','green','yellow'))

#check dell'ipotesi di simmetria degli outliers tramite il test dei segni per grandi campioni

for(i in 1:4){
  
  for(j in 1:4){
    
    x<-outendlist[[i]][[j]]
    boxplot(x)
    x.sel <- x[x!=0]
    segno.pos <- x.sel>0
    z <- (sum(segno.pos)-length(segno.pos)/2)/sqrt(length(segno.pos)/4)
    print(c(i,j))
    print(z)
    print(pnorm(z))
    if(pnorm(z)<0.05) print('warning')
    
  }
}

# non credo sia necessario, gli outliers standardizzati sono simmetrici per costruzione, 
#considerando che sono costruiti a parire dalla mediana.

