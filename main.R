library(rvest)
# library(RPostgreSQL)

WEBPAGE="https://www.ovinu.si"

parse_winery <- function(url="/vinar/166") {
  
  df <- data.frame(FullName=character(),
                   Region=character(),
                   YearSince=character(),
                   Ha=character(),
                   YearlyProd=character(),
                   IsDegustation=character(),
                   IsStore=character(),
                   Wines=character(),
                   Card=character())
  
  winepage <- read_html(sprintf('%s%s', WEBPAGE, url))

  fullname <- winepage %>%
    html_node("h2") %>%
    html_text()

  props <- winepage %>%
    html_nodes("dt") %>%
    html_text()

  winery <- winepage %>%
    html_nodes("dd") %>%
    html_text()

  card <- winepage %>%
    html_nodes(".left") %>%
    html_text()
  
  wines <- winepage %>%
    html_nodes(".title")
  
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
#  write.csv(df, "winery.csv")
  df
}

parse_wineries <- function(url="https://www.ovinu.si/vinarji") {
  
  df <- data.frame(Winery=character(), URL=character())
  
  links_makers_page <- read_html(url)
  
  links_makers <- links_makers_page %>%
    html_nodes(".even")
  links_makers <- c(links_makers, links_makers_page %>%
                      html_nodes(".odd"))
  
  for (i in 1:length(links_makers)){
   de <- data.frame((links_makers[[i]] %>% html_text), (xml_attr(links_makers[[i]] %>% html_node("a"), attr = "href")))
   colnames(de) <- colnames(df)
   df <- rbind(df, de)
  }
  
  df <- unique(df)
#  write.csv(df, "wineries.csv")
  df
}

parse_wines <- function(url="/vinar/166"){
  
  df <- data.frame(Wine=character(), URL=character())
  
  winepage <- read_html(sprintf('%s%s', WEBPAGE, url))
  
  wines <- winepage %>%
    html_nodes(".title")
  
  for (i in 1:length(wines)) {
  de <- data.frame((wines[[i]] %>% html_text()),
                   (xml_attr(wines[[i]], attr = "href")))
  colnames(de) <- colnames(df)
  df <- rbind(df, de)
  }
  
  df <- unique(df)
#  write.csv(df, "wines.csv")
  df
}

parse_wine <- function(url="/vino/1302") {
  
  df <- data.frame(FullName=character(),
                   Years=character(),
                   Price=character(),
                   Description=character(),
                   Winery=character(),
                   Region=character(),
                   Sort=character(),
                   Year=character(),
                   Style=character(),
                   Colour=character(),
                   Body=character(),
                   SugarLevel=character(),
                   Alcohol=character(),
                   ServingTemperature=character(),
                   Production=character(),
                   Maturation=character())
  
  winepage <- read_html(sprintf('%s%s', WEBPAGE, url))
  
  fullname <- winepage %>%
    html_node("h2") %>%
    html_text()
  
  description <- winepage %>%
    html_node(".ocena_opis") %>%
    html_text()
  
  years <- winepage %>%
    html_node(".ocena_popiti") %>%
    html_text()
  
  price <- winepage %>%
    html_node(".cena") %>%
    html_text()
  price <- strsplit(price, ":")[[1]][2]
  
  props <- winepage %>%
    html_nodes("dd") %>%
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
    props[12])
  
  colnames(de) <- colnames(df)
  
  df <- rbind(df, de)
  
#  write.csv(df, "wine.csv")
  
  df
}

main <- function() {
  print("Start!")
  csv_wineries <- data.frame()
  csv_wines <- data.frame()
  wineries <- parse_wineries()
  
  for (i in 1:nrow(wineries)) {
    print(sprintf("i=%d",i))
    print(wineries[i,"URL"])
    url_winery <- wineries[i,"URL"]
    winery <- parse_winery(url = url_winery)
    csv_wineries <- rbind(csv_wineries, winery)
    wines <- parse_wines(url = url_winery)
    

    for (k in seq(1, nrow(wines), 1)){
      print(sprintf("k=%d",k))
      print(wines[k,2])
      wine <- parse_wine(wines[k,2])
      csv_wines <- rbind(csv_wines, wine)
      }
    }
  write.csv(csv_wineries, "all_wineries.csv")
  write.csv(csv_wines, "all_wines.csv")
}