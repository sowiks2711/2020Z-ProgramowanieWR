library(dplyr)
library(ggplot2)
library(wesanderson)
library(tidyr)
library(lattice)
library(latticeExtra)

data(msleep, package="ggplot2")
#sleep total per vore // box plot 

msleep %>% filter(!is.na(vore)) %>% ggplot(aes(x=vore, y=sleep_total)) +
  geom_boxplot(colour="black") +
  geom_point(color="blue") +
  theme_bw() +
  labs(x="Vore", y="Hours of sleep per day", title="Distribution of time of sleep per vore")

bwplot(
  sleep_total~vore,
  msleep,
  xlab="Vore",
  ylab="Hours of sleep per day",
  main="Distribution of time of sleep per vore",
  par.settings=list(
    box.umbrella = list(col = "black"), 
    box.rectangle=list(col= rep(c("black", "black"),2)))
  ) +
dotplot(sleep_total~vore, msleep, col="blue")

#sleep awake // part of rectangle
orderedNames <- msleep %>% arrange(sleep_total) %>% mutate(name=factor(name,name)) %>% filter(conservation=="domesticated") %>% pull(name) %>% as.factor()
plotData <- msleep %>% filter(conservation=="domesticated") %>% gather("state", "time", sleep_total, awake)
plotData  %>% mutate(
    state=factor(ifelse(state=="sleep_total", "asleep", "awake"),c("awake","asleep")),
    name=factor(name,orderedNames)
  ) %>% 
  select(name, time, state) %>% 
  ggplot(aes(x=name, y=time, fill=state)) +
  geom_bar(stat="identity") +
  labs(x="Specie",
       y="Duration of state",
       title="Sleep awake ratio for domesticated animals") +
  theme_minimal()


######################################################################

nestList <- function(seedList, leafList, level) {
  if(level > 0) {
    lapply(seedList, function(i){
      res <- nestList(i, leafList, level-1)
      res
    })
  } else {
    leafList
  }
}

createNestedList <- function(
  outermostCount,
  nestedCount,
  nesting,
  innerMostCount) {
  res <- 1:outermostCount %>% as.list()
  lapply(res, function(i){
     nestList(1:nestedCount, 1:innerMostCount, nesting)
  })
}

for(i in 1:50) {
  for(j in 1:5) {
    for(k in 1:5) {
      for(l in 1:50) {
        size <- object.size(createNestedList(i,j,k,l))
        dataRow <- data.frame(
          Outermost=i,
          Nested=j,
          Nestings=k,
          Innermost=l,
          Size=size %>% as.numeric()
        )
        sizeData <- rbind(sizeData, dataRow)
      }
    }
  }
}

sizeData %>% mutate(Elements=Outermost*Innermost*Nested, SizeByElems=(Size/(Elements))) -> processedData

head(sizeData)
#Size 
sizeData %>% ggplot(aes(x=Outermost, y=Innermost, fill=Size)) +
  geom_raster() +
  facet_grid(Nested~Nestings, labeller = label_both) +
  scale_fill_gradientn(colours=wes_palette("Zissou1", 100, type = "continuous")) +
  labs(title="Memory usage in nested list structure",
       fill="Size in bytes")

levelplot(Size~Innermost*Outermost|Nestings*Nested, sizeData)

#nestings and innermost
processedData %>% ggplot(aes(x=Elements, y=Size, col=factor(Innermost))) +
  geom_point() +
  facet_wrap(~Nestings, labeller = label_both) +
  labs(y="Size in bytes",
       col="Nr of elements in the most nested lists")


library(randomcoloR)
palette <- rainbow(50)
xyplot(
  Size~Elements|Nestings,
  processedData,
  groups=Innermost,
  col=palette,
  pch=19,
  key = list(space="right",
             points=list(col=palette, lty=c(3,2), lwd=6),
             text=list(as.character(1:50))
            )
  )
  

#mean size heatmap
processedData %>% group_by(Nested, Nestings) %>% summarise(mSizeByElems=mean(SizeByElems)) %>% 
  ggplot(aes(x=Nestings, y=Nested, fill=mSizeByElems)) +
  geom_tile() +
  scale_fill_gradientn(colours=wes_palette("Zissou1", 100, type = "continuous")) +
  labs(fill="Mean size\nper element\n[bytes/elem]",
       title="Memory eficiency for lists with different levels of nesting")

groupedSizeData <- processedData %>% group_by(Nested, Nestings) %>% summarise(mSizeByElems=mean(SizeByElems))

library(grid)
levelplot(mSizeByElems~Nested*Nestings, groupedSizeData,
          main="Memory eficiency for lists with different levels of nesting")

grid.text("Mean size\nper element\n[bytes/elem]", y=unit(0.93, "npc"),
          x=unit(0.95, "npc"),
          gp=gpar(col="black", fontsize=8))
