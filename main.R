file_wines <- 'all_wines.csv'
if (file.exists(file_wines)) {
  wines <- read.csv(file_wines)
  print('Loaded all wines')
}

barve <- function() {
  counts <- table(wines$Colour)
  barplot(counts,
          main = 'Colours',
          col = c('yellow', 'red', 'pink'))
}

regije <- function() {
  counts <- table(wines$Region)
  barplot(counts, main = 'Regije')
}

serving <- function() {
  counts <- table(wines$ServingTemperature)
  barplot(counts, main = 'Temperature')
}

serving_comb <- function() {
  counts <- table(wines$Colour, wines$ServingTemperature)
  barplot(
    counts,
    main = 'Serving Temperature vs Colour',
    legend = rownames(counts),
    col = c('yellow', 'red', 'pink')
  )
}

year <- function() {
  counts <- table(wines$Year)
  barplot(counts, main = 'Years')
}

bodies <- function() {
  counts <- table(wines$Body)
  barplot(counts, main = 'Telo')
}

sorts_top10 <- function() {
  counts <- sort(table(wines$Sort),
                 decreasing = TRUE)[1:10]
  barplot(counts, main = 'Sorte')
}

sugar <- function() {
  counts <- table(wines$SugarLevel)
  barplot(counts, main = 'Tipi')
}

alcohol <- function() {
  counts <- table(wines$Alcohol)
  barplot(counts, main = 'Alko')
}

winery_prod <- function() {

  fivemakers <- sort(table(wines$Winery), decreasing = TRUE)[1:5]
  counts <- table(wines$Colour, wines$Winery)
  fivecounts <- counts[,names(fivemakers)]
  
  op <- par(mar=c(1,7,4,2))
  barplot(counts,
          main = 'Production',
          legend = rownames(counts),
          col = c('yellow', 'red', 'pink'),
          cex.names=0.3,
          las = 1,
          horiz = T
  )
  
  op <- par(mar=c(10,2,1,1))
  barplot(fivecounts,
          main = 'Top five',
          legend = rownames(counts),
          col = c('yellow', 'red', 'pink'),
          las = 2
  )
  rm(op)
}
