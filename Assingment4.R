#q1
plot(1:10,1:10,type = "n")
for(i in 1:10){
  lines(c(i,i),c(1,20))
}
for(j in 1:20){
  lines(c(1,10),c(j,j))
}

#q2
plot(1:10,1:10)
for(i in 1:10){
  for(j in 1:20 )
  {
    points(i,j)
  }
}

#q3
plot(1:10,1:10)
  for(j in 1:20){
    color = if(j %% 2== 0)("blue")
    else{"red"}
    lines(c(1,10),c(i,j),col=color)
  }