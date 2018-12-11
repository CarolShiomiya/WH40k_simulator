#モーターの性能評価
sim2(A="D6",WS=4,Str =8 ,Tgh = 4,Saving = 3,AP= 2, D="D3",Dreroll = 1 ,Subtitle = "PBM VS marine")
sim3(A="D6",WS=4,Attack=8,Toughness=4,Saving=3,AP=2,D="D3",Wound=10,Dreroll = 1,"PBM VS marine")

#エントロピーキャノンVSプレーグスピッター
sim2(A=1,WS=4,Str =8 ,Tgh = 8,Saving = 5,AP= 0, D="D6" ,Subtitle = "Entropy VS Knight")
sim2(A="D6",WS=4,Str =7 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "Spitter VS Knight")

sim2(A=1,WS=4,Str =8 ,Tgh = 4,Saving = 3,AP= 4, D="D6" ,Subtitle = "Entropy VS marine")
sim2(A="D6",WS=4,Str =7 ,Tgh = 4,Saving = 3,AP= 1, D=1,Dreroll = 1 ,Subtitle = "Spitter VS marine")

sim3(A=1,WS=4,Attack=8,Toughness=4,Saving=3,AP=4,D="D6",Wound=10,Dreroll = 0,"Entropy VS marine")
sim3(A="D6",WS=4,Attack=8,Toughness=4,Saving=3,AP=1,D=1,Wound=10,Dreroll = 1,"Spitter VS marine")


#ヘビースラッガーVSロットレイルヴォレイガン

sim2(A=4,WS=4,Str =5 ,Tgh = 8,Saving = 3,AP= 1, D=1 ,Subtitle = "heavystubber VS Knight")
sim2(A=3,WS=4,Str =6 ,Tgh = 8,Saving = 3,AP= 2, D=1 ,Subtitle = "volley VS Knight")

sim2(A=4,WS=4,Str =5 ,Tgh = 4,Saving = 3,AP= 1, D=1 ,Subtitle = "heavystubber VS marine")
sim2(A=3,WS=4,Str =6 ,Tgh = 4,Saving = 3,AP= 2, D=1 ,Subtitle = "volley VS marine")

sim3(A=4,WS=4,Attack=5,Toughness=4,Saving=3,AP=1,D=1,Wound=10,Dreroll = 0,"heavystubber VS marine")
sim3(A=3,WS=4,Attack=6,Toughness=4,Saving=3,AP=2,D=1,Wound=10,Dreroll = 0,"volley VS marine")

