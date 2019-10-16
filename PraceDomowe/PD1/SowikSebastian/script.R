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

boxplot(sleep_total~vore,
        msleep,
        xlab="Vore",
        ylab="Hours of sleep per day",
        main="Distribution of time of sleep per vore"
        )
stripchart(sleep_total~vore, vertical = TRUE, data = msleep, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue')

#sleep awake // part of rectangle
orderedNames <- msleep %>% arrange(sleep_total) %>% mutate(name=factor(name,name)) %>% filter(conservation=="domesticated") %>% pull(name) %>% as.factor()
plotData <- msleep %>% filter(conservation=="domesticated") %>% 
  gather("state", "time", sleep_total, awake)  %>% mutate(
    state=factor(ifelse(state=="sleep_total", "asleep", "awake"),c("awake","asleep")),
    name=factor(name,orderedNames)
  ) 
plotData %>% 
  select(name, time, state) %>% 
  ggplot(aes(x=name, y=time, fill=state)) +
  geom_bar(stat="identity") +
  labs(x="Specie",
       y="Duration of state",
       title="Sleep awake ratio for domesticated animals") +
  theme_minimal()


barchart(time ~ name,
         groups=factor(plotData[["state"]], c("asleep", "awake")),
         data=plotData,
         stack=TRUE,
         xlab="Specie",
         ylab="Duration of state",
         auto.key=list(space="right", title="State"),
         main="Sleep awake ratio for domesticated animals")
graphicsData <- plotData %>% select(name, state, time) %>% spread(name, time) %>% select(-state) %>% arrange(Horse) %>%as.matrix.data.frame()
rownames(graphicsData) <- c("awake", "asleep")
barplot(graphicsData,
        legend=c("awake", "asleep"),
         xlab="Specie",
         ylab="Duration of state",
         main="Sleep awake ratio for domesticated animals")
        
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

levelplot(Size~Innermost*Outermost|Nestings*Nested,
          sizeData,
          auto.key=list(space="right",title="Size in bytes"))

contourDataList = list()
contourDataList[[1]]
for(i in 1:5) {
  for(j in 1:5) {
    data <- sizeData %>% filter(Nested==1 & Nestings==3) %>% 
            select(-Nested, -Nestings) %>% spread(Outermost, Size) %>% data.matrix()
    contourDataList <- append(contourDataList, list(data))
  }
}


source("./Filled.contour3.R")
plot.new()
par(mfrow=c(5,5), mar=rep(1,4)) 
for(i in 1:25) {
  filled.contour3(y=seq(0,1,length.out=ncol(tmp3)), x=seq(0,1,length.out=nrow(tmp3)), z=contourDataList[[i]] )
}

#nestings and innermost
processedData %>% ggplot(aes(x=Elements, y=Size, col=factor(Innermost))) +
  geom_point() +
  facet_wrap(~Nestings, labeller = label_both) +
  labs(y="Size in bytes",
       col="Nr of elements in the most nested lists",
       title="Size of nested lists"
       )


library(randomcoloR)
palette <- rainbow(50)
xyplot(
  Size~Elements|Nestings,
  processedData,
  groups=Innermost,
  col=palette,
  pch=19,
  key = list(space="right",
             columns=3,
             title="Nr of elements \nin the most nested lists",
             points=list(col=palette, lty=c(3,2), lwd=3),
             text=list(as.character(1:50))
            ),
  ylab="Size in bytes"
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
          main="Memory eficiency for lists with different levels of nesting",
          auto.key=list(space="right",title="Mean size\nper element\n[bytes/elem]"))

grid.text("Mean size\nper element\n[bytes/elem]", y=unit(0.93, "npc"),
          x=unit(0.95, "npc"),
          gp=gpar(col="black", fontsize=8))
