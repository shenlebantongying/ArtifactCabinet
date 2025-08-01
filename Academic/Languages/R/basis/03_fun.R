# (...) -> execute and print
(d0<-matrix(c(1,2,3,4,5,6),nrow=2))
(d1<-d0[,c(2,3)])
(d2<-t(d1))

(rbind(d1,d2))
(cbind(d1,d2))

for (v in as.vector(rbind(d1,d2))){
  print(v)
}

plot(as.vector(cbind(d1,d2)))
