library(rvest)

url <- "https://www.ovinu.si/vinar/%d"
winery_catalog <- list()
for (i in 1:166){
  curr_url <- sprintf(url, i)
  winepage <- read_html(url)

  name <- winepage %>%
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

  winery <- c(name, winery, card, length(wines))

  names(winery) = c("Ime", props, "Vizitka", "Vina")

  winery_catalog[[i]] <- winery
}

write.csv(winery_catalog, file = "MyData.csv")