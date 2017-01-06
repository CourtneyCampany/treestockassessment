
##merge both leaf mass datasets and determine mean mass per tree
##build function to makes this general

##read data
alpine_leaf <- read.csv("data/alpine_leaves.csv")
alpine_leaf2 <- read.csv("data/alpine_leaves_lumped.csv") #leaves that could not be weighed seperate

alpine_leaf$leafmass <- rowMeans(alpine_leaf[, c("leaf1", "leaf2", "leaf3")], na.rm=TRUE)

leafmeans <- alpine_leaf[,c("batch_id", "tree", "leafmass")]



leafmass_func <- function(dfr1, dfr2) {
  
  agg_dat1 <- dfr1[dfr1$tree == "all",]
  
  agg_dat2 <- dfr1[!dfr1$tree == "all",]
  ##insert warning message that agg_dat2$leaf_numb should ==3
  ##then remove leafnumb from
  if(agg_dat2$leaf_numb != 3) stop("there is any error in tree/batch seperation")
  print("seperated lumped datasets into individual trees or whole batches successfully")
  
  dfr2$leafmass <- rowMeans(df2[, c("leaf1", "leaf2", "leaf3")], na.rm=TRUE)
  leafmeans <- dfr2[,c("batch_id", "tree", "meanleafmass")]
  
  ##add the lumped data back to new means dfr, keep tree# id
  
  
  
}