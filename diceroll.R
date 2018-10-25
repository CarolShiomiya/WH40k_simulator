###攻撃で何回ダメージが通るか実験します。
#hitroll Aの数だけロールして、WSで命中
hitroll<-function(A,WS){
  hitcheck<-ceiling(runif(A)*6)>=WS
  #print(hitcheck)
  return(sum(hitcheck))
}
#atk vs toughness, returns required-die
AvT<-function(attack,toughness){
  if(attack>=toughness*2){
    require<-2
  }
  else if(attack>toughness){
    require<-3
  }
  else if(attack==toughness){
    require<-4
  }
  else if(attack*2<=toughness){
    require<-6
  }
  else if(attack<toughness){
    require<-5
  }
  return(require)
}

#damageroll hitsの数だけhitしたとして、攻撃者のAttackと被攻撃者のToughnessに応じたダメージロールを行う
damageroll<-function(hits,Attack,Toughness){
  require<-AvT(Attack,Toughness)
  damagecheck<-ceiling(runif(hits)*6)>=require
  return(sum(damagecheck))
}
damageroll(hitroll(6,4),5,5)

#AProll　damegesの数だけhitしたとして、防御者のSavingと攻撃者のAPに応じたセービングを行い、通ったダメージ回数を返す
AProll<-function(damages,saving,AP){
  savingcheck<-ceiling(runif(damages)*6)>=(saving+AP)
  return(damages-sum(savingcheck))
}
AProll(damageroll(hitroll(6,4),5,5),2,3)

#total sim　A,WS,Attack,Toughness,Saving,APを入力し、
#５万回のシミュレーションを行いダメージ回数ごとの確率を表にする
sim<-function(A,WS,Attack,Toughness,Saving,AP){
  title<-paste("A=",A,", WS=",WS,", Attack=",Attack,", Toughness=",Toughness,", Saving=",Saving,", AP=",AP)
  APs<-c()
  for (i in 1:50000){
    APs[i]<-AProll(damageroll(hitroll(A,WS),Attack,Toughness),Saving,AP)
  }
  plot(table(APs)/500,main =title,xlab="Damage(times)",ylab="Probability(%)")
  return(table(APs)/500)
  }

sim(A=12,WS=4,Attack=5,Toughness=4,Saving=4,AP=1)

#partial sims
hits<-c()
for (i in 1:10000){
  hits[i]<-hitroll(10,3)
  }
plot(table(hits)/100)

damages<-c()
for (i in 1:10000){
  damages[i]<-damageroll(hitroll(6,4),5,5)
}
plot(table(damages)/100)

APs<-c()
for (i in 1:10000){
  APs[i]<-AProll(damageroll(hitroll(6,4),5,5),2,3)
}
plot(table(APs)/100)


#ダメージ回数を先に決めてから、それに達するまでにかかる回数のシミュレーション
howmany.to.kill<-function(A,WS,Attack,Toughness,Saving,AP,Wound){
  chikuseki_damage<-0
  i <- 0
  repeat {
    i<-i+1
    chikuseki_damage<-chikuseki_damage+AProll(damageroll(hitroll(A,WS),Attack,Toughness),Saving,AP)
    
    if(chikuseki_damage >= Wound) break
  
  }
  return(i)
}

sim2<-function(A,WS,Attack,Toughness,Saving,AP,Wound){
  times<-c()
  for (j in 1:50000){
    times[j]<-howmany.to.kill(A,WS,Attack,Toughness,Saving,AP,Wound)
  }
  plot(table(times)/500,main="How many times do U need to kill them?",xlab="Attacks(times)",ylab="Probability(%)")
  return(table(times)/500)
}
sim2(2,3,1,1,4,2,4)
