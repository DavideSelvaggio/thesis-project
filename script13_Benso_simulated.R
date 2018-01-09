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


regola<-function(x,par){
  xord<-sort(x,decreasing=TRUE)
  max1<-xord[1]
  max2<-xord[2]
  diff<-max1-max2
  outRegola<-NA
  if(max1<par[1])  outRegola<-"O"
  if(max1>par[2]) outRegola<-"T"
  if (max1>=par[1] & max1<=par[2] & diff>par[3]) outRegola<-"T"
  if (max1>=par[1] & max1<=par[2] & diff<=par[3] & max2>par[1]) outRegola<-"U" 
  if (max1>=par[1] & max1<=par[2] & diff<=par[3] & max2<=par[1]) outRegola<-"O" 
  outRegola
}

fobb<-function(par,posterior,cl){
  ss1<-1
  if( sum((par>0)*(par<1)) ==3 & par[1]<par[2]){
    outRegola<-apply(posterior,1,regola,par=par)
    if (length(unique(outRegola))!=1) {
      confmat<-table(cl,outRegola)
      sens<-confmat[2,2]/sum(confmat[2,])
      spec<-confmat[1,1]/sum(confmat[1,])
      ss1<-1-(sens*spec)/(sens+spec)
    }
  }
  ss1
}


################################# passo 1)  ##############################################################
semep<-c(8764836,4887636,8647368,6836487)                          #### parametro in input
semesnp<-475648					#### per generare repliche fissato pmatr
semeiniz<-34863
nr<-4
nrazze<-c(450,450,450,450)                         #### parametro in input
trfract<-0.75                         #### parametro in input
Fst<-c(0.015, 0.035, 0.05, 0.08)		#0.02			#### parametro in input
nsnp<-500					                   	#### parametro in input
pmatr<-NULL
BensoOut<-list()
ss1<-list()
CompuTime<-list()
##########################################################
# for(i in 1:nr){                                        #
#   pmatr<-cbind(pmatr,generap(1,Fst[i],nsnp,semep[1]))  #
# }                                                      #
# flist<-generasnp(nrazze,pmatr,semesnp)                 #
##########################################################
for(rf in 1:nr){
  print(paste('rf n.',rf,'on 4'))
##############################################################  
  pmatr<-generap(nr,Fst[rf],nsnp,semep[rf])                  #
  flist<-generasnp(nrazze,pmatr,semesnp)                     # 
############################################################## 
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

############### ciclo per i da 1 a k   ########################
ptrain<-list()
ptarget<-list()
pout<-list()
m<-10                                                   #### parametro in input
semi<-matrix(seq(from=1,by=3476,length.out=(k*m)),k,m)   #### parametro in input
fr<-0.5									#### parametro in input
ciclo<-0									###parametro in input
for (i in v){
  ciclo<-ciclo+1
  print(paste("razza esclusa=", i))
  ptraini<-list()
  ptargeti<-list()
  pouti<-list()
  tprimolist<-lapply(trainlist,qtprimo,v=v,ciclo=ciclo)
  rlist<-lapply(trainlist,qr,v=v,ciclo=ciclo)
  
  ############### ciclo per j da 1 a m   ########################
  num<-unlist(lapply(tprimolist[[1]],nrow))
  rnum<-nrow(rlist[[1]][[1]])
  for (j in 1:m){
    set.seed(semi[ciclo,j])
    idlist<-list()
    for(jj in 1:length(tprimolist[[1]])){
      permjj<-sample(1:num[jj],num[jj])
      idlist[[jj]]<-permjj[1:round(num[jj]*fr)]
    }
    rperm<-sample(1:rnum,rnum)
    ridlist<-rperm[1:round(rnum*fr)]
    trainstar<-lapply(tprimolist,star,idlist=idlist)
    testtarget<-lapply(tprimolist,starmeno,idlist=idlist)	

    # ciclo in nsel
    ptrainij<-list()
    ptargetij<-list()
    poutij<-list()

    trnselj<-data.frame(trainstar[[1]])
    tsnselj<-data.frame(testtarget[[1]])
    outselj<-data.frame(rlist[[1]][[1]])
 
      testoutj<-data.frame(lapply(outselj[ridlist,],factor,levels=c("0","1","2")))
      trainstarj<-data.frame(lapply(trnselj,factor,levels=c("0","1","2")))
      testtargetj<-data.frame(lapply(rbind(tsnselj),factor,levels=c("0","1","2")))
      razzaj<-as.factor(rep(v[-ciclo],times=unlist(lapply(idlist,length))))
      numtarget<-num-unlist(lapply(idlist,length))
      razzajtarget<-as.factor(rep(v[-ciclo],times=numtarget))
      outRFj<-randomForest(x=trainstarj,y=razzaj)
      
      ptrainij[[1]]<-predict(outRFj,type="prob")
      ptargetij[[1]]<-predict(outRFj,newdata=testtargetj,type="prob")
      poutij[[1]]<-predict(outRFj,newdata=testoutj,type="prob")
      
    ptraini[[j]]<-ptrainij
    ptargeti[[j]]<-ptargetij
    pouti[[j]]<-poutij
    
  } # fine ciclo in j per le repliche
  
  ptrain[[ciclo]]<-ptraini
  ptarget[[ciclo]]<-ptargeti
  pout[[ciclo]]<-pouti
  
} # fine ciclo in i sulle razze




#################################
#SETTAGGIO THRESHOLD
#################################

#########################  functions: regola di decisione e funzione obiettivo ####################


pout1<-list()
ptarget1<-list()
posterior<-list()


  pout1c<-NULL
  ptarget1c<-NULL
  for(i in 1:3){	
    for (j in 1:m){
      pout1c<-rbind(pout1c,pout[[i]][[j]][[1]])
      ptarget1c<-rbind(ptarget1c,ptarget[[i]][[j]][[1]])
    }
  }
  pout1[[1]]<-pout1c
  ptarget1[[1]]<-ptarget1c
  posterior[[1]]<-rbind(ptarget1c,pout1c)
  


cl<-c(rep("T",nrow(ptarget1[[1]])),rep("O",nrow(pout1[[1]])))

numiniz<-10
inizpar<-matrix(NA,numiniz,3)   ### numiniz da dare in input
u<-1
while (u<=numiniz){
  iniz<-runif(3)
  if(iniz[1]<iniz[2]) {
    inizpar[u,]<-iniz
    u<-u+1
  }
}

parmat<-list()
optpar<-list()

  parmat[[1]]<-matrix(NA,numiniz,4)
  TimeA<-Sys.time()
  for (u in 1:numiniz){
    print(paste("u=", u))
    outmin<-optim(inizpar[u,],fobb,posterior=posterior[[1]],cl=cl)
    if (outmin$convergence==0) parmat[[1]][u,]<-c(outmin$par,outmin$value)
  }
  optpar[[1]]<-parmat[[1]][which.min(parmat[[1]][,4]),1:3]
  TimeB<-Sys.time()
  CompuTime[[rf]]<-TimeB-TimeA
  print(CompuTime[[rf]])
########################### FILTRAGGIO SUL TRAINING  #################################

ptrainlist<-list()
ptargetlist<-list()
poutlist<-list()
outregolalist<-list()

trnsel<-data.frame(bindlist(trainlist[[1]]))
tsnsel<-data.frame(bindlist(testlist[[1]][v]))
outsel<-data.frame(testlist[[1]][[esclusa]])
  
  testout<-data.frame(lapply(outsel,factor,levels=c("0","1","2")))
  traintarget<-data.frame(lapply(trnsel,factor,levels=c("0","1","2")))
  testtarget<-data.frame(lapply(tsnsel,factor,levels=c("0","1","2")))
  
  razza<-as.factor(rep(v,times=unlist(lapply(lidtr,length))[-esclusa]))
  
  if(esclusa != 4){
  numtargetf<-(nrazze-unlist(lapply(lidtr,length)))[-esclusa]
  }
  
  razzatarget<-as.factor(rep(v,times=numtargetf))
  outRF<-randomForest(x=traintarget,y=razza)
  
  ptrainlist[[1]]<-predict(outRF,type="prob")
  ptargetlist[[1]]<-predict(outRF,newdata=testtarget,type="prob")
  poutlist[[1]]<-predict(outRF,newdata=testout,type="prob")
  
  posterior1<-rbind(ptargetlist[[1]],poutlist[[1]])
  outregolalist[[1]]<-apply(posterior1,1,regola,par=optpar[[1]])

BensoOut[[rf]]<-outregolalist[[1]]
################################################################################
cl<-c(rep("T",nrow(testtarget)),rep("O",nrow(testout)))

  confmat<-table(cl,outregolalist[[1]])
  sens<-confmat[2,2]/sum(confmat[2,])
  spec<-confmat[1,1]/sum(confmat[1,])
  ss1[[rf]]<-1-(sens*spec)/(sens+spec)
  
}
