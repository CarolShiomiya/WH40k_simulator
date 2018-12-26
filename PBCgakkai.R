#モーターの性能評価
par(mfcol=c(2,1))
sim2(A="D6",WS=4,Str =8 ,Tgh = 4,Saving = 3,AP= 2, D="D3",Dreroll = 1 ,Subtitle = "PBM VS marine")
sim3(A="D6",WS=4,Attack=8,Toughness=4,Saving=3,AP=2,D="D3",Wound=10,Dreroll = 1,"PBM VS marine")

#エントロピーキャノンVSプレーグスピッター
par(mfcol=c(2,1))
EvK<-sim2(A=1,WS=4,Str =8 ,Tgh = 8,Saving = 5,AP= 0, D="D6" ,Subtitle = "Entropy VS Knight")
SvK<-sim2(A="D6",WS=4,Str =7 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "Spitter VS Knight")
print(sum(EvK>SvK)/length(EvK))
print(sum(EvK==SvK)/length(EvK))
print(sum(EvK<SvK)/length(EvK))

par(mfcol=c(2,1))
EvM<-sim2(A=1,WS=4,Str =8 ,Tgh = 4,Saving = 3,AP= 4, D="D6" ,Subtitle = "Entropy VS marine")
SvM<-sim2(A="D6",WS=4,Str =7 ,Tgh = 4,Saving = 3,AP= 1, D=1,Dreroll = 1 ,Subtitle = "Spitter VS marine")
print(sum(EvM>SvM)/length(EvM))
print(sum(EvM==SvM)/length(EvM))
print(sum(EvM<SvM)/length(EvM))
type<-c(sum(EvM>SvM)/length(EvM),sum(EvM<SvM)/length(EvM))
which.max(type)

sapply(EvM,FUN=function(x){min(x,2)})

par(mfcol=c(2,1))
EvM_howmany<-sim3(A=1,WS=4,Attack=8,Toughness=4,Saving=3,AP=4,D="D6",Wound=10,Dreroll = 0,"Entropy VS marine")
SvM_howmany<-sim3(A="D6",WS=4,Attack=8,Toughness=4,Saving=3,AP=1,D=1,Wound=10,Dreroll = 1,"Spitter VS marine")

#ヘビースラッガーVSロットレイルヴォレイガン
par(mfcol=c(2,1))
sim2(A=4,WS=4,Str =5 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "heavystubber VS Knight")
sim2(A=3,WS=4,Str =6 ,Tgh = 8,Saving = 3,AP= 2, D=1 ,Subtitle = "volley VS Knight")

sim2(A=4,WS=4,Str =5 ,Tgh = 4,Saving = 3,AP= 1, D=1 ,Subtitle = "heavystubber VS marine")
sim2(A=3,WS=4,Str =6 ,Tgh = 4,Saving = 3,AP= 2, D=1 ,Subtitle = "volley VS marine")

sim3(A=4,WS=4,Attack=5,Toughness=4,Saving=3,AP=1,D=1,Wound=10,Dreroll = 0,"heavystubber VS marine")
sim3(A=3,WS=4,Attack=6,Toughness=4,Saving=3,AP=2,D=1,Wound=10,Dreroll = 0,"volley VS marine")


#相手を変えながら、エントロピーキャノンとスピッターを比較する
library(lattice)
#install.packages("latticeExtra")
library(latticeExtra)

list<-list()
for (save in 2:7){ #全てのAPについて試す
  vec<-c()
  for (tgh in 3:8){ #全ての耐久について試す
    EvM<-sim2(A=1,WS=4,Str =8 ,Tgh = tgh,Saving = save,AP= 4, D="D6" ,Subtitle = "Entropy VS marine")
    SvM<-sim2(A="D6",WS=4,Str =7 ,Tgh = tgh,Saving = save,AP= 1, D=1,Dreroll = 1 ,Subtitle = "Spitter VS marine")
    type<-c(sum(EvM>SvM)/length(EvM),sum(EvM<SvM)/length(EvM))
    print(type)
    if (max(type)-min(type)<0.01){tmp<-1.5}else{tmp<-which.max(type)}
    vec<-c(vec,tmp)
    
  }
  list[[save-1]]<-vec}

#二次元ヒートマップを出力
saving <- seq(2,7,by=1)
toughness <- seq(3,8,by=1)
d <- expand.grid(saving=saving,toughness=toughness)
for( i in 1:length(d$saving)){
  d$z[i]<-list[[(d$saving[i])-1]][d$toughness[i]-2]}
levelplot(z~saving*toughness, data=d, asp=1,colorkey=list(at=c(1,1.51,2), labels=list(at=c(1,1.5,2), labels=c("スピッター優位","同等","エントロピーキャノン優位"))),main="それぞれの敵に与えるダメージが多いのは？")



#信頼区間の検証
rate<-c()
for (i in 1:10000){
  ransu<-rnorm(50000)
  ransu2<-rnorm(50000)
  
  rate<-c(rate,sum(ransu>ransu2)/50000)
}
hist(rate)
