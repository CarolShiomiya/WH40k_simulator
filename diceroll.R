WS BS S T W A Save

#hitroll
hitroll<-function(A,WS){
  hitcheck<-ceiling(runif(A)*6)>=WS
  #print(hitcheck)
  return(sum(hitcheck))
}
#atk vs toughness
AvT<-function()

#damageroll
damageroll<-function(hits,Attack,Toughness){

}

hits<-c()
for (i in 1:10000){
  hits[i]<-hitroll(10,3)
  }
plot(table(hits)/100)

