plot2d <- function(res, dim1=1, dim2=2) {
  e1 <- as.vector(res$ecoords[,dim1])
  e2 <- as.vector(res$ecoords[,dim2])
  avge1 <- mean(abs(e1))
  avge2 <- mean(abs(e2))
  avepl1 <- mean(abs(e1)+abs(e2))
  lbl <- res$lbl
  auto_mca_table <- data.frame(cbind(e1,e2,lbl))
  auto_mca_table$e1 <- as.numeric(auto_mca_table$e1)
  auto_mca_table$e2 <- as.numeric(auto_mca_table$e2)
  auto_mca_table$lbl <- as.factor(auto_mca_table$lbl)
  auto_mca_table <- auto_mca_table %>%filter(abs(e1)+abs(e2)>avepl1)
  averageccc = avepl1
  avexx=c(averageccc,0,-averageccc,0,averageccc)
  aveyy=c(0,-averageccc,0,averageccc,0)
  averagesquaree=round(as.data.frame(cbind(avexx,aveyy)),2)

  ggplot()->interpretive_plane1
  interpretive_plane1+geom_path(data=averagesquaree,aes(x=avexx,y=aveyy),color='red')->interpretive_plane1
  auto_mca_table$c=abs(auto_mca_table$e1)+abs(auto_mca_table$e2)
  for (i in 1:nrow(auto_mca_table)){

    thexx=c(auto_mca_table$c[i],0,-auto_mca_table$c[i],0,auto_mca_table$c[i])
    theyy=c(0,-auto_mca_table$c[i],0,auto_mca_table$c[i],0)
    ccc=cbind(thexx,theyy)
    ccc=as.data.frame(ccc)

    interpretive_plane1 +geom_path(data=ccc,aes(x=thexx,y=theyy),show.legend = TRUE,linewidth=0.1)->interpretive_plane1

  }
  interpretive_plane1+geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+coord_fixed()+
    geom_point(data=auto_mca_table,aes(x = e1, y = e2),shape = "circle", colour = "black")->interpretive_plane1

  interpretive_plane1+geom_label_repel(data=auto_mca_table,
                                       mapping=aes(x=e1,y=e2,label = paste(lbl)), # data point size
                                       size = 3.5,
                                       max.overlaps = Inf,
                                       point.padding = 0.7,
                                       min.segment.length = 0.9,
                                       box.padding = 1.2

  )+
    labs(x = paste("Interpretive axis",dim1),y=paste("Interpretive axis",dim2))->interpretive_plane1

  interpretive_plane1
}



