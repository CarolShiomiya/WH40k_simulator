#相手を変えながら、パワーソードとチェーンソードを比較する
library(lattice)
#install.packages("latticeExtra")
library(latticeExtra)

list<-list()
for (save in 2:7){ #全てのAPについて試す
  vec<-c()
  for (tgh in 3:8){ #全ての耐久について試す
    PS<-sim2(A=2,WS=3,Str =4 ,Tgh = tgh,Saving = save,AP= 3, D=1 ,Subtitle = "PowerSword")
    CS<-sim2(A=3,WS=3,Str =4 ,Tgh = tgh,Saving = save,AP= 0, D=1,Subtitle = "ChainSword")
    type<-c(sum(PS>CS)/length(PS),sum(PS<CS)/length(PS))
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
levelplot(z~saving*toughness, data=d, asp=1,colorkey=list(at=c(1,1.51,2), labels=list(at=c(1,1.5,2), labels=c("PowerSword優位","同等","ChainSword優位"))),main="それぞれの敵に与えるダメージが多いのは？")


#対ナイト
par(mfcol=c(2,3))
sim3(A=10,WS=4,Attack=9,Toughness=8,Saving=3,AP=3,D="D6",Wound=24,Dreroll = 0,SpecialSave = 5,Accum = TRUE,"Lascannon VS Knight")
sim3(A=40,WS=4,Attack=5,Toughness=8,Saving=3,AP=3,D="D3",Wound=24,Dreroll = 0,SpecialSave = 5,Accum = TRUE,"Grav-cannon VS Knight")
sim3(A=10,WS=4,Attack=8,Toughness=8,Saving=3,AP=4,D="D6",Wound=24,Dreroll = 0,SpecialSave = 5,Accum = TRUE,Melta=TRUE,"Melta VS Knight")
par(mfcol=c(2,1))
sim2(A=40,WS=4,Str=5,Tgh=8,Saving=3,AP=3,D="D3",Subtitle="",Dreroll=0,SpecialSave=5)
sim2(A=80,WS=4,Str=5,Tgh=8,Saving=3,AP=3,D="D3",Subtitle="",Dreroll=0,SpecialSave=5)
howmany.to.kill(A=10,WS=4,Attack = 9,Toughness = 8,Saving = 3,AP=3,D="D6",Wound = 5,SpecialSave = 5)
