library(dplyr )
library(ggplot2)
library(tidyr)
# 1. Porównaj szybkość działania pętli for i funkcji lapply dla:
Sys.time()
# a) generowania listy 1000 data.frames o 5 kolumnach i 500 wierszach
system.time(
  lapply(1:10000, function(i) {lapply(1:5, function(i){
    1:500
  })} %>% as.data.frame())
)[['elapsed']] -> lapplyMeasures


forAppendMeasures <- system.time({
    res <- list()
    for(i in 1:10000) {
      list <- list()
      for(j in 1:5) {
       list[[j]] = 1:500
      }
      res[[i]] <- list %>% as.data.frame()
    }
  })[['elapsed']]

forReplaceMeasures <- system.time(
  {
    res <- list(1:1000)
    for(j in 1:1000) {
      res[[j]] <- replicate(5,list(1:500)) %>% as.data.frame()
    }
  })[['elapsed']]

# b) generowanie listy 1000 wektorow klasy integer o długości 100


lapplyMeasures <- c(
  system.time(
  lapply(1:1000, function(i){
    1:500
  }))[['elapsed']],
  lapplyMeasures
)

forAppendMeasures <- c(
  system.time(
  { 
    res <- list()
    for(j in 1:1000) {
       list[[j]] = 1:100
    }
  })[['elapsed']],
  forAppendMeasures
)

forReplaceMeasures <- c(
  system.time(
  { 
    res <- list(1:1000)
    for(j in 1:1000) {
      res[[j]] <- 1:100
    }
  })[['elapsed']],
  forReplaceMeasures
)

# c) generowanie listy 1000 wektorow klasy numeric o długości 100
lapplyMeasures <- c(
  system.time(
  lapply(1:1000, function(i){
    0.2*1:100
  }))[['elapsed']],
  lapplyMeasures
)

forAppendMeasures <- c(
  system.time(
  { 
    res <- list()
    for(j in 1:1000) {
       list[[j]] = 0.1*1:500
    }
  })[['elapsed']],
  forAppendMeasures
)

forReplaceMeasures <- c(
  system.time(
  { 
    res <- list(1:1000)
    for(j in 1:1000) {
      res[[j]] <- 0.1*1:500
    }
  })[['elapsed']],
  forReplaceMeasures
)
data = data.frame(Name = c("Data.Frames", "Integers", "Numerics"),
                  ForAppendTime = forAppendMeasures,
                  ForReplaceTime = forReplaceMeasures,
                  LApply = lapplyMeasures )
data %>% gather("MethodName", "Time", -Name) %>%
  ggplot(aes(x=MethodName, y=Time, fill=MethodName)) +
  facet_wrap(~Name, scales = "free") +
  geom_col() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=45, vjust=0.5),
    axis.title = element_blank()
  )
  
# Dla petli for przygotuj dwa warianty: dodawanie elementów do istniejących listy
# i nadpisywanie elementów zadeklarowanej listy
# Wyniki przedstaw na wykresie.


# 2. Porównaj szybkość działania instrukcji warunkowej if i funkcji ifelse

system.time(
{
  j = 0
  for( i in 1:100000) {
    if(i %% 2 == 0) {
      j <- j + i
    } else {
      j <- j - i
    }
  }
  j
})

system.time(
{
  j = 0
  for( i in 1:100000) {
    j <- ifelse(i %% 2 == 0, j+i, j-i) 
  }
  j
})
# 3. Sprawdz rozmiar listy o od 1 do 50 elementów zawierającej od 1 do 5 zagniezdzonych 
# od 1 do 5 razy list zawierajacych od 1 do 50 elementow. Wyniki przedstaw na wykresie.
glue::glue("How are {nest}")
sizeData <- data.frame(
  Nesting=c(1),
  NestedCount=c(1),
  InnermostCount=c(1),
  OutermosrCount=c(1)
  )
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

sizeData = data.frame()
names(sizeData)=c("Outermost", "Nested", "Nestings", "Innermost")

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

head(sizeData)

sizeData %>% ggplot(aes(x=Outermost, y=Innermost)) +
  geom_tile(aes(fill=(Size/(Outermost*Innermost*Nested)))) +
  facet_wrap(Nested~Nestings) +
  theme(
    strip.text.x = element_blank()
  )
  theme_classic()


  
sizeData %>% mutate(Elements=Outermost*Innermost*Nested, SizeByElems=(Size/(Elements))) -> processedData
  
processeddata %>% ggplot(aes(x=elements, y=size, group=nestings, col=factor(nestings))) +
  geom_point()
  
processedData %>% ggplot(aes(x=Elements, y=Size, group=Nestings, col=factor(Nestings))) +
  geom_point() +
  facet_wrap(~Nestings)
  

processedData %>% ggplot(aes(x=Elements, y=Size, group=Nestings, col=factor(Nested))) +
  geom_point() +
  facet_wrap(~Nestings)

processedData %>% ggplot(aes(x=Elements, y=Size, group=Nestings, col=factor(Innermost))) +
  geom_point() +
  facet_wrap(~Nestings)


sizeData %>% mutate(Count=Outermost*Nested) %>%  ggplot(aes(y=Count, x=Innermost)) +
  scale_y_log10(  ) +
  geom_tile(aes(fill=(Size))) +
  facet_wrap(~Nestings) +
  theme_classic()
# 4. Porównaj rozmiary obiektów kwadratowych macierzy rzadkich (wymiary od 4 do 400) 
# zawierających elementy klasy 
# a) integer
# b) numeric 
# c) macierzy z pakietu slam 
# d) macierzy z pakietu Matrix.  