
#相手を変えながら、パワーソードとチェーンソードを比較する
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
