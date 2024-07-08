# C) Do multidimensional scaling and interpret the results. 

icecream_df<-read.csv('D:\\CHRIST\\Boot camp\\DATA\\icecream.csv',header=TRUE)
dim(icecream_df)

names(icecream_df) 

ice<-subset(icecream_df,select = -c(Brand)) 
distance_matrix<-dist(ice) 

mds_result<-cmdscale(distance_matrix,k=2) 

plot(mds_result[,1],mds_result[,2],pch=16,xlab="Dimension1",ylab="Dimension2",main="MDS plot") 
