library(xml2)
library(curl)
library(rvest)
library(parallel)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = 'FORK')
stopCluster(cl)

setwd("~/git/ovinu/ovinu")
WEBPAGE = 'https://www.ovinu.si'

get_html <- function(url = 'https://www.ovinu.si/vinar/166') {
  loc <- substring(url_parse(url)['path'], 2)
  if (!file.exists(loc)) {
    download.file(url, destfile = loc, quiet = TRUE)
  }
  loc
}

parse_winery <- function(url = '/vinar/166') {
  df <- data.frame(
    FullName = character(),
    Region = character(),
    YearSince = character(),
    Ha = character(),
    YearlyProd = character(),
    IsDegustation = character(),
    IsStore = character(),
    Wines = character(),
    Card = character()
  )
  
  winepage <- read_html(get_html(sprintf('%s%s', WEBPAGE, url)))
  
  fullname <- winepage %>%
    html_node('h2') %>%
    html_text()
  
  props <- winepage %>%
    html_nodes('dt') %>%
    html_text()
  
  winery <- winepage %>%
    html_nodes('dd') %>%
    html_text()
  
  card <- winepage %>%
    html_nodes('.left') %>%
    html_text()
  
  wines <- winepage %>%
    html_nodes('.title')
  
  de <- data.frame(fullname,
                   winery[1],
                   winery[2],
                   winery[3],
                   winery[4],
                   winery[5],
                   winery[6],
                   length(wines),
                   card)
  
  colnames(de) <- colnames(df)
  
  df <- rbind(df, de)
  df
}

parse_wineries <- function(url = 'https://www.ovinu.si/vinarji') {
  df <- data.frame(Winery = character(), URL = character())
  
  links_makers_page <- read_html(get_html(url))
  
  links_makers <- links_makers_page %>%
    html_nodes('.even')
  links_makers <- c(links_makers, links_makers_page %>%
                      html_nodes('.odd'))
  
  for (i in seq_along(links_makers)) {
    de <-
      data.frame((links_makers[[i]] %>% html_text), (xml_attr(
        links_makers[[i]] %>% html_node('a'), attr = 'href'
      )))
    colnames(de) <- colnames(df)
    df <- rbind(df, de)
  }
  
  df <- unique(df)
  df
}

parse_wines <- function(url = '/vinar/166') {
  df <- data.frame(Wine = character(), URL = character())
  
  winepage <- read_html(get_html(sprintf('%s%s', WEBPAGE, url)))
  
  wines <- winepage %>%
    html_nodes('.title')
  
  for (i in seq_along(wines)) {
    de <- data.frame((wines[[i]] %>% html_text()),
                     (xml_attr(wines[[i]], attr = 'href')))
    colnames(de) <- colnames(df)
    df <- rbind(df, de)
  }
  
  df <- unique(df)
  df
}

parse_wine <- function(url = '/vino/1302') {
  df <- data.frame(
    FullName = character(),
    Years = character(),
    Price = character(),
    Description = character(),
    Winery = character(),
    Region = character(),
    Sort = character(),
    Year = character(),
    Style = character(),
    Colour = character(),
    Body = character(),
    SugarLevel = character(),
    Alcohol = character(),
    ServingTemperature = character(),
    Production = character(),
    Maturation = character()
  )
  
  winepage <- read_html(get_html(sprintf('%s%s', WEBPAGE, url)))
  
  fullname <- winepage %>%
    html_node('h2') %>%
    html_text()
  
  description <- winepage %>%
    html_node('.ocena_opis') %>%
    html_text()
  
  years <- winepage %>%
    html_node('.ocena_popiti') %>%
    html_text()
  
  price <- winepage %>%
    html_node('.cena') %>%
    html_text()
  price <- strsplit(price, ':')[[1]][2]
  
  props <- winepage %>%
    html_nodes('dd') %>%
    html_text()
  
  de <- data.frame(
    fullname,
    years,
    price,
    description,
    props[1],
    props[2],
    props[3],
    props[4],
    props[5],
    props[6],
    props[7],
    props[8],
    props[9],
    props[10],
    props[11],
    props[12]
  )
  
  colnames(de) <- colnames(df)
  
  df <- rbind(df, de)
  df
}

prepare_csv <- function() {
  print('Start!')
  csv_wineries <- data.frame()
  csv_wines <- data.frame()
  wineries <- parse_wineries()
  all_wineries <- nrow(wineries)
  for (i in seq_len(nrow(wineries))) {
    nowat <-sprintf('i=%d, %f%%',
                    i,
                    ((i/all_wineries)*100))
    print(wineries[i, 'URL'])
    url_winery <- wineries[i, 'URL']
    winery <- parse_winery(url = url_winery)
    csv_wineries <- rbind(csv_wineries, winery)
    wines <- parse_wines(url = url_winery)
    all_wines <- nrow(wines)
    for (k in seq_len(nrow(wines))) {
      print(sprintf('%s k=%d %f%%',
                    nowat,
                    k,
                    ((k/all_wines)*100)))
      print(wines[k, 2])
      if (nrow(wines) != 0) {
        wine <- parse_wine(wines[k, 2])
        csv_wines <- rbind(csv_wines, wine)
      }
    }
  }
  write.csv(csv_wineries, 'all_wineries.csv')
  write.csv(csv_wines, 'all_wines.csv')
}
