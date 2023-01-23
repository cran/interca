interca <- function(data = data, numaxes = 10){
  out <- list()
  results <- MCA(data,ncp = numaxes,graph = FALSE)#running multiple correspondence analysis with FactoMineR package
  coords <- results$var$coord#points' coordinates for first factorial axis
  ctr<- results$var$contrib#points' contribution for first factorial axis
  cor <- results$var$cos2#points' contribution for first factorial axis
  lbl <- rownames(results$var$coord)
  signs <- apply(coords,2,sign)
  ecoords <- matrix(0,nrow(coords),ncol(coords))
  rownames(ecoords)=lbl
  colnames(ecoords)=colnames(results$var$coord)
  for (i in 1:numaxes) {
    ecoords[,i] <- sign(coords[,i])*ctr[,i]*results$eig[i,1]
  }
  out$coords <- coords
  out$ecoords <- ecoords
  out$ctr <- ctr
  out$cor <- cor
  out$lbl <- lbl
  fviz_eig(results, addlabels=TRUE, hjust = -0.3,
           linecolor ="red") + theme_minimal()->plot


  out$plot<-plot
  class(out) <- "automca"
  out
}

