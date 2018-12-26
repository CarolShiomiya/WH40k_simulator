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
#Drerollには、0:リロールなし（デフォルト）、1:1のみリロール、"any":失敗を全てリロール
damageroll<-function(hits,Strength,Toughness,Dreroll=0){
  require<-AvT(Strength,Toughness)
  damagedies<-ceiling(runif(hits)*6)
  damagecheck<-damagedies>=require
  #reroll
  if (Dreroll==0){
    return(sum(damagecheck))
  }
  else if (Dreroll==1){
    rerolls<-sum(damagedies==1)
    reroll_damagedies<-ceiling(runif(rerolls)*6)
    reroll_damagecheck<-reroll_damagedies>=require
    return(sum(damagecheck)+sum(reroll_damagecheck))
  }
  else if (Dreroll=="any"){
    rerolls<-sum(!damagecheck)
    reroll_damagedies<-ceiling(runif(rerolls)*6)
    reroll_damagecheck<-reroll_damagedies>=require
    return(sum(damagecheck)+sum(reroll_damagecheck))
  }
  
}

#AProll　damegesの数だけdamageロールが通ったとして、防御者のSavingと攻撃者のAPに応じたセービングを行い、通ったダメージ回数を返す
AProll<-function(damages,saving,AP,SpecialSave=7){
  savingcheck<-ceiling(runif(damages)*6)>=min((saving+AP),SpecialSave)
  return(damages-sum(savingcheck))
}
AProll(damageroll(hitroll(A="D6",3),5,2),2,3)

#total sim　A,WS,Attack,Toughness,Saving,APを入力し、
#5万回のシミュレーションを行いダメージ回数ごとの確率を表にする
sim1<-function(A,WS,Str,Toughness,Saving,AP,Dreroll=0,SpecialSave=7){
  title<-paste("A=",A,", WS=",WS,", Str=",Str,", Tgh=",Toughness,", Save=",Saving,",SPsave=",SpecialSave,"AP=",AP)
  APs<-c()
  for (i in 1:50000){
    APs[i]<-AProll(damageroll(hitroll(A,WS),Str,Toughness,Dreroll),Saving,AP,SpecialSave)
  }
  plot(table(APs)/500,main =title,xlab="Damage(times)",ylab="Probability(%)")
  print(table(APs)/500)
  return(APs)
}


#実行
sim1(A="D6",WS=4,Str=5,Toughness=4,Saving=3,AP=2,SpecialSave = 4)
sim1(A="D6",WS=4,Str=5,Toughness=4,Saving=3,AP=2,SpecialSave = 7)

#ダメージ量のバージョン
sim2<-function(A,WS,Str,Tgh,Saving,AP=0,D=1,Subtitle="",Dreroll=0,SpecialSave=7){
  title<-paste("A=",A,", WS=",WS,", Str=",Str,", Tgh=",Tgh,", Save=",Saving,",SPsave=",SpecialSave,", AP=",AP)
  APs<-c()
  for (i in 1:50000){
    APs[i]<-AProll(damageroll(hitroll(A,WS),Str,Tgh,Dreroll),Saving,AP,SpecialSave)
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
  print(table(Damageamount)/500)
  return(Damageamount)
}

#実行
sim2(A=1,WS=4,Str =8 ,Tgh = 8,Saving = 5,AP= 0, D="D6" ,Subtitle = "Entropy VS Knight",SpecialSave = 4)
sim2(A="D6",WS=4,Str =7 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "Spitter VS Knight")

#ダメージ回数を先に決めてから、それに達するまでにかかる回数のシミュレーション
howmany.to.kill<-function(A,WS,Attack,Toughness,Saving,AP,D,Wound,Dreroll=0,SpecialSave=7,Melta=FALSE){
  chikuseki_damage<-0
  i <- 0
  repeat {
    i<-i+1
    APs<-AProll(damageroll(hitroll(A,WS),Attack,Toughness,Dreroll),Saving,AP,SpecialSave)
    if (D=="D3"){
      j<-0
      while(j<APs){
        D2=ceiling(runif(1)*3)
        chikuseki_damage<-chikuseki_damage+D2
        j<-j+1}
      }
    if (D=="D6"){
      j<-0
      while(j<APs){
        if(Melta){D2<-max(ceiling(runif(2)*6))}else{D2<-ceiling(runif(1)*6)}
        print(D2)
        chikuseki_damage<-chikuseki_damage+D2
        print(chikuseki_damage)
        j<-j+1}
      }
    if(is.numeric(D)==TRUE){
      chikuseki_damage<-chikuseki_damage+D*AProll(damageroll(hitroll(A,WS),Attack,Toughness,Dreroll),Saving,AP,SpecialSave)
    }  
    
    if(chikuseki_damage >= Wound) break
  }
  return(i)
}


#実行
howmany.to.kill("D6",3,5,4,2,3,2,10,Dreroll = 0,SpecialSave = 4)

#50000回のシミュレーション
sim3<-function(A,WS,Attack,Toughness,Saving,AP,D,Wound,Subtitle="",Dreroll=0,SpecialSave=7,Accum=FALSE,Melta=FALSE){
  times<-c()
  for (j in 1:50000){
    times[j]<-howmany.to.kill(A,WS,Attack,Toughness,Saving,AP,D,Wound,Dreroll,SpecialSave,Melta)
  }
  plot(table(times)/500,main=paste("How many times are required to kill them?",Subtitle),xlab="Attacks(times)",ylab="Probability(%)")
  if (Accum){
    plot(cumsum(table(times)/500),main=paste("How many times are required to kill them?",Subtitle),xlab="Attacks(times or less)",ylab="Probability(%)")
  }
  print(table(times)/500)
  return(times)
}

#実行
par(mfcol=c(2,3))
sim3(A="D6",WS=1,Attack=7,Toughness=4,Saving=3,AP=1,D=1,Wound=24,"Spitter VS Intersessor")
sim3
