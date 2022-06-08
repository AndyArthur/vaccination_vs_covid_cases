cos <- counties('ny')

nb <- cos %>% vect() %>% adjacent("rook", pairs=T) 
v <- centroids(cos %>% vect())

p1 <- v[nb[,1], ]
p2 <- v[nb[,2], ]

par(mai=c(0,0,0,0))
plot(cos %>% vect(), col="gray", border="blue")
lines(p1, p2, col="red", lwd=2)
points(p1)
points(p2)
