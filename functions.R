library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(car)

wload <- function() {
  temp <- tempfile()
  archive <- 'wine.zip'
  file_wines <- 'all_wines.csv'
  if (file.exists(archive)) {
    wines <- read.csv(unz(archive, file_wines))
    print('Loaded all wines')
  } else {
    stop('NO FILE FOUND!')
  }
  unlink(temp)
  rm(file_wines)
  wines
}

wines <- wload()

selektor <- function(kolona = 'Maturation', poisk = 'amfora') {
  q <- sprintf('SELECT X, %s
                FROM wines
                WHERE %s LIKE "%%%s%%"', kolona, kolona, poisk)
  sqldf(q)
}

mean_price_var <- function(variety = 'merlot') {
  mean(unlist(subset(wines, wines$Variety == variety)['Price']))
}

find_repeating <- function(table = wines$Variety) {
  for (i in table) {
    kk <- gregexpr('\\b(\\S+?)\\1\\S*\\b', i, perl = T)[[1]][1]
    if (kk > 0) {
      print(i)
    }
  }
}
