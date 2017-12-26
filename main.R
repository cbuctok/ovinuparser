file_wines <- 'all_wines.csv'
if (file.exists(file_wines)) {
  wines <- read.csv(file_wines)
  print('Loaded all wines')
}

barve <- function() {
  counts <- table(wines$Colour)
  barplot(counts, main = 'Barve')
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
    main = 'Temperature vs Colour',
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