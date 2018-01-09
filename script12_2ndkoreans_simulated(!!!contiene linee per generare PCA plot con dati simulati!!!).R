rm(list=ls())
library(randomForest)

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

cbindlist<-function(x){
  y<-x[[1]]
  for (i in 2:length(x)){
    y<-cbind(y,x[[i]])
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

sel<-function(x,nsel) x[1:nsel]


################################# passo 1)  ##############################################################
semep<-c(8764836,4887636,8647368,6836487)                           #### parametro in input
semesnp<-475648					#### per generare repliche fissato pmatr
semeiniz<-34863
nr<-4
nrazze<-c(450,450,450,450)                         #### parametro in input
trfract<-0.75                         #### parametro in input
Fst<-c(0.015, 0.035, 0.05, 0.08)		#0.02			#### parametro in input
nsnp<-500					                   	#### parametro in input
pmatr<-NULL
ss1<-list()
##########################################################
# for(i in 1:nr){                                        #
#   pmatr<-cbind(pmatr,generap(1,Fst[i],nsnp,semep[1]))  #
# }                                                      #
# flist<-generasnp(nrazze,pmatr,semesnp)                 #
##########################################################
for(rf in 1:nr){
  print(paste('rf n.',rf,'on 4'))
#############################################################  
  pmatr<-generap(nr,Fst[rf],nsnp,semep[rf])                 #
  flist<-generasnp(nrazze,pmatr,semesnp)                    # 
############################################################# 
  esclusa<-rf
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
  
  lnr<-lapply(lidtr,length)
  v<-(1:4)[-esclusa]
  k<-3 # nr classi presenti nel training
  

######## RANDOM FOREST  ##########

proxtrainlist<-list()
proxtargetlist<-list()
proxoutlist<-list()

  trnsel<-data.frame(bindlist(trainlist[[1]]))
  tsnsel<-data.frame(bindlist(testlist[[1]][v]))
  outsel<-data.frame(testlist[[1]][[esclusa]])

  testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
  traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
  testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))
  
  razza<-as.factor(rep(v,times=unlist(lapply(lidtr,length))[-esclusa]))
  razzac<-rep(v,times=unlist(lapply(lidtr,length))[-esclusa])
  if(esclusa==1){
    numtarget<-(nrazze-unlist(lapply(lidtr,length)))[-esclusa]
  }
  razzatarget<-as.factor(rep(v,times=numtarget))
  razzatargetc<-rep(v,times=numtarget)
  outRF<-randomForest(x=traintarget,y=razza,proximity=TRUE,oob.prox=TRUE)
  
  proxtrainlist[[1]]<-predict(outRF,proximity=TRUE)$proximity
  proxtargetlist[[1]]<-predict(outRF,newdata=rbind(traintarget,testtarget),proximity=TRUE)$proximity
  proxoutlist[[1]]<-predict(outRF,newdata=rbind(traintarget,testout),proximity=TRUE)$proximity
  


######## CALCOLO DELLE SOGLIE  ##########
ntr<-table(razza)
a<-cumsum(ntr)
a<-c(0,a)
pimed<-list()

  pimedc<-NULL
  for(l in 1:(length(a)-1)){
    vec<-apply((proxtrainlist[[1]][(a[l]+1):a[l+1],(a[l]+1):a[l+1]]),2,sum)
    pimedc<-c(pimedc,sum(vec)/ntr[l]^2)
  }
  pimed[[1]]<-pimedc


proxmatrixtestS<-list()
b<-length(razza)


  proxmatrixtestS[[1]]<-cbind(proxtargetlist[[1]][(1:b),],proxoutlist[[1]][(1:b),-(1:b)])


Wdi<-list()

  wdic<-NULL

  for(l in 1:(length(a)-1)){
    vec<-apply(proxmatrixtestS[[1]][(a[l]+1):a[l+1],(a[length(a)]+1):ncol(proxmatrixtestS[[1]])],2,sum)
    wdic<-rbind(wdic,vec/(ntr[l]))
  }
  Wdi[[1]]<-(wdic)

####### APPLICAZIONE DELLE SOGLIE #######

decided<-list()
alpha<-1                             ### parametro in input

  decidedj<-NULL
  decidedj<-Wdi[[1]]<alpha*pimed[[1]]
  decidedj<-apply(decidedj,2,prod)
  decidedj[decidedj==1]<-'O'
  decidedj[decidedj==0]<-'T'
  decided[[1]]<-decidedj



###### VALUTAZIONE DEL SISTEMA ####

real<-c(rep('T',length(razzatarget)),rep('O',dim(testout)[1]))

  confmat<-table(real,decided[[1]])
  sens<-confmat[2,2]/sum(confmat[2,])
  spec<-confmat[1,1]/sum(confmat[1,])
  ss1[[rf]]<-1-(sens*spec)/(sens+spec) 


## rappresentazione grafica tramite PCA
  
      dAtA<-rbind(trnsel,tsnsel,outsel)
      pca.dAtA<-prcomp(dAtA)
      col<-c(razzac,razzatargetc,rep('grey45',dim(outsel)[1]))
      col[col==1]<-'red3'
      col[col==2]<-'yellow2'
      col[col==3]<-'royalblue3'
      col[col==4]<-'lawngreen'
      names<-c('class 1','class 2','class 3','class 4')
      colors<-c('red3','yellow2','royalblue3','lawngreen')
      plot(pca.dAtA$x[,1:2],col=col,main=paste('Principal component analysis plot (a)
    -Fst=',Fst[esclusa],'-Simulated Data-'))
      legend("topleft", c(names[-esclusa],names[esclusa]), title='classes',pch=1,
           col=c(colors[-esclusa],'grey45'))
    
      ## rappresentazione grafica tramite prime due componenti principali
        dAtA<-trnsel
        pca.dAtA<-prcomp(dAtA)
        pca.dAtA<-predict(pca.dAtA,newdata=rbind(dAtA,outsel))
        col<-c(razzac,rep('grey45',dim(outsel)[1]))
        col[col==1]<-'red3'
        col[col==2]<-'yellow2'
        col[col==3]<-'royalblue3'
        col[col==4]<-'lawngreen'
        plot(pca.dAtA[,1:2],col=col,main=paste('Principal component analysis plot (b)
      -Fst= ',Fst[esclusa],'- Simulated Data-'))
        legend("topleft", c(names[-esclusa],names[esclusa]), title='classes',pch=1,
               col=c(colors[-esclusa],'grey45'))
  
  
}

##### RAPPRESENTAZIONE GRAFICA ####

# colors1<-decided[[14]]
# colors1[colors1=='T']<-'darkorange2'
# colors1[colors1=='O']<-'grey45'
# colors2<-c(razzatarget,rep('grey45',dim(testout)[1]))
# colors2[colors2==1]<-'red3'
# colors2[colors2==3]<-'yellowgreen'
# colors2[colors2==2]<-'royalblue3'
# 
# par(mfrow=c(2,3))
# plot(Wdi[[14]][1,],Wdi[[14]][2,],col=colors2,ylab = 'proximity to class 3',
#      xlab = 'proximity to class 1',main = 'Proximity Distribution')
# legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
#        col=c('red3','royalblue3','yellowgreen','darkgrey'))
# 
# plot(Wdi[[14]][1,],Wdi[[14]][3,],col=colors2,ylab = 'proximity to class 4',
#      xlab = 'proximity to class 1',main = 'Proximity Distribution')
# legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
#        col=c('red3','royalblue3','yellowgreen','darkgrey'))
# 
# plot(Wdi[[14]][2,],Wdi[[14]][3,],col=colors2,ylab = 'proximity to class 4',
#      xlab = 'proximity to class 3',main = 'Proximity Distribution')
# legend("topright", c('breed 1','breed 3','breed 4','novelty'), title='classes',pch=1,
#        col=c('red3','royalblue3','yellowgreen','darkgrey'))
# 
# plot(Wdi[[14]][1,],Wdi[[14]][2,],col=colors1,ylab = 'proximity to class 3',
#      xlab = 'proximity to class 1',main = 'Proximity Distribution')
# legend("topright", c('known breeds','novelty'), title='classes',pch=1,
#        col=c('darkorange2','darkgrey'))
# 
# plot(Wdi[[14]][1,],Wdi[[14]][3,],col=colors1,ylab = 'proximity to class 4',
#      xlab = 'proximity to class 1',main = 'Proximity Distribution')
# legend("topright", c('known breeds','novelty'), title='classes',pch=1,
#        col=c('darkorange2','darkgrey'))
# 
# plot(Wdi[[14]][2,],Wdi[[14]][3,],col=colors1,ylab = 'proximity to class 4',
#      xlab = 'proximity to class 3',main = 'Proximity Distribution')
# legend("topright", c('known breeds','novelty'), title='classes',pch=1,
#        col=c('darkorange2','darkgrey'))