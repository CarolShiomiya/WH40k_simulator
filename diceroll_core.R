###攻撃で何回ダメージが通るか実験します。
#hitroll Aの数だけロールして、WSで命中
hitroll<-function(A,WS){
  if (A=="D6"){
    A<-ceiling(runif(1)*6)
  }
  if (A=="D3"){
    A<-ceiling(runif(1)*3)
  }
  
  hitcheck<-ceiling(runif(A)*6)>=WS
  #print(hitcheck)
  return(sum(hitcheck))
}

#atk vs toughness, returns required-die
AvT<-function(strength,toughness){
  if(strength>=toughness*2){
    require<-2
  }
  else if(strength>toughness){
    require<-3
  }
  else if(strength==toughness){
    require<-4
  }
  else if(strength*2<=toughness){
    require<-6
  }
  else if(strength<toughness){
    require<-5
  }
  return(require)
}

#damageroll hitsの数だけhitしたとして、攻撃者のStrengthと被攻撃者のToughnessに応じたダメージロールを行う
damageroll<-function(hits,Strength,Toughness){
  require<-AvT(Strength,Toughness)
  damagecheck<-ceiling(runif(hits)*6)>=require
  return(sum(damagecheck))
}

#AProll　damegesの数だけdamageロールが通ったとして、防御者のSavingと攻撃者のAPに応じたセービングを行い、通ったダメージ回数を返す
AProll<-function(damages,saving,AP){
  savingcheck<-ceiling(runif(damages)*6)>=(saving+AP)
  return(damages-sum(savingcheck))
}
AProll(damageroll(hitroll(A="D6",3),5,2),2,3)

#total sim　A,WS,Attack,Toughness,Saving,APを入力し、
#５万回のシミュレーションを行いダメージ回数ごとの確率を表にする
sim1<-function(A,WS,Str,Toughness,Saving,AP){
  title<-paste("A=",A,", WS=",WS,", Str=",Str,", Tgh=",Toughness,", Saving=",Saving,", AP=",AP)
  APs<-c()
  for (i in 1:50000){
    APs[i]<-AProll(damageroll(hitroll(A,WS),Str,Toughness),Saving,AP)
  }
  plot(table(APs)/500,main =title,xlab="Damage(times)",ylab="Probability(%)")
  return(table(APs)/500)
}


#実行
sim1(A="D6",WS=4,Str=5,Toughness=4,Saving=4,AP=1)

#ダメージ量のバージョン
sim2<-function(A,WS,Str,Tgh,Saving,AP=0,D=1,Subtitle=""){
  title<-paste("A=",A,", WS=",WS,", Str=",Str,", Tgh=",Tgh,", Saving=",Saving,", AP=",AP)
  APs<-c()
  for (i in 1:50000){
    APs[i]<-AProll(damageroll(hitroll(A,WS),Str,Tgh),Saving,AP)
  }
  Damageamount<-c()
  for (i in 1:50000){
    if(D=="D6"){
      Damageamount[i]<-sum(ceiling(runif(APs[i])*6))
    }
    if(D=="D3"){
      Damageamount[i]<-sum(ceiling(runif(APs[i])*3))
    }
    if(is.numeric(D)){
    Damageamount[i]<-APs[i]*D
    }
  }
  plot(table(Damageamount)/500,main =paste(title,Subtitle),xlab="Damage(amount)",ylab="Probability(%)")
  return(table(Damageamount)/500)
}

#実行
sim2(A=1,WS=4,Str =8 ,Tgh = 8,Saving = 5,AP= 0, D="D6" ,Subtitle = "Entropy VS Knight")
sim2(A="D6",WS=4,Str =7 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "Spitter VS Knight")

#ダメージ回数を先に決めてから、それに達するまでにかかる回数のシミュレーション
howmany.to.kill<-function(A,WS,Attack,Toughness,Saving,AP,D,Wound){
  chikuseki_damage<-0
  i <- 0
  repeat {
    i<-i+1
    if (D=="D3"){D=ceiling(runif(1)*3)}
    if (D=="D6"){D=ceiling(runif(1)*6)}
    
    chikuseki_damage<-chikuseki_damage+D*AProll(damageroll(hitroll(A,WS),Attack,Toughness),Saving,AP)
    
    if(chikuseki_damage >= Wound) break
    
  }
  return(i)
}


#実行
howmany.to.kill("D6",3,5,4,2,3,2,10)

#50000回のシミュレーション
sim3<-function(A,WS,Attack,Toughness,Saving,AP,D,Wound,Subtitle=""){
  times<-c()
  for (j in 1:50000){
    times[j]<-howmany.to.kill(A,WS,Attack,Toughness,Saving,AP,D,Wound)
  }
  plot(table(times)/500,main=paste("How many times are required to kill them?",Subtitle),xlab="Attacks(times)",ylab="Probability(%)")
  return(table(times)/500)
}

#実行
sim3(A="D6",WS=1,Attack=7,Toughness=4,Saving=3,AP=1,D=1,Wound=10,"Spitter VS Intersessor")

