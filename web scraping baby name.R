library(wordcloud)
library(irlba)
library(tm)
library(stopwords)
library(plyr)
# library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(dplyr)
# library(tidyr)
library(ggplot2)
library(RCurl)
library(XML)
library(plyr)
library(reshape2)
options(error = recover)
library(foreach)
library(doParallel)
library(tibble)
library(textmineR)
library(Rcrawler)
library(polite)


# site1

part1 <- "https://nomi.auguri.it/nomi-femminili-con-iniziale-"
part3 <- ".htm"

i <- "l"
get_names = function(i){
  url <- paste0(part1, i, part3)
  session <- bow(url, force = TRUE, delay = 5)
  result <- scrape(session, query=list(page = i))

  node_i <- paste0("#lista_nomi_categoria ", i)

  lnks <- result %>%
    html_nodes(node_i)

  if (length(lnks) == 0) {
    node_i <- paste0("#lista_nomi_categoria :nth-child(1)")
    lnks <- result %>%
      html_nodes(node_i)
  }

  lnks %>% length()

  link_table <- tibble(name = lnks %>% html_text(),
                       link = i) %>%
    distinct() %>%
    dplyr::mutate(sesso = "F")
  return(link_table)
}

list_name <- letters %>%
  purrr::map_df(.x = ., .f = get_names)

save.image("list1_F.RData")

# site1

part1 <- "https://nomi.auguri.it/nomi-maschili-con-iniziale-b.htm"

part1 <- "https://nomi.auguri.it/nomi-maschili-con-iniziale-"
part3 <- ".htm"

i <- "l"
get_names = function(i){
  url <- paste0(part1, i, part3)
  session <- bow(url, force = TRUE, delay = 5)
  result <- scrape(session, query=list(page = i))

  node_i <- paste0("#lista_nomi_categoria ", i)

  lnks <- result %>%
    html_nodes(node_i)

  if (length(lnks) == 0) {
    node_i <- paste0("#lista_nomi_categoria :nth-child(1)")
    lnks <- result %>%
      html_nodes(node_i)
  }

  lnks %>% length()

  link_table <- tibble(name = lnks %>% html_text(),
                       link = i) %>%
    distinct()
  return(link_table)
}

list_name <- letters %>%
  purrr::map_df(.x = ., .f = get_names)

save.image("C:/Users/laceto/Desktop/myname/list1.RData")

# site2

part1 <- "https://www.nomix.it/nomi-italiani-lettera-"
part3 <- ".php"

i <- "A"
get_names = function(i){
  print(i)
  url <- paste0(part1, i, part3)
  session <- bow(url, force = TRUE, delay = 5)
  result <- scrape(session, query=list(page = i))

  node_i <- ".pure-u-md-1-2:nth-child(1) td"

  lnks <- result %>%
    html_nodes(node_i)

  link_table_M <- tibble(name = lnks %>% html_text(),
                       link = i) %>%
    dplyr::distinct() %>%
    dplyr::mutate(sesso = "M")

  node_i <- ".pure-u-md-1-2+ .pure-u-md-1-2 td"

  lnks <- result %>%
    html_nodes(node_i)

  link_table_F <- tibble(name = lnks %>% html_text(),
                         link = i) %>%
    dplyr::distinct() %>%
    dplyr::mutate(sesso = "F")

  link_table <- bind_rows(link_table_F, link_table_M)

  return(link_table)
}

list_name <- LETTERS %>%
  purrr::map_df(.x = ., .f = get_names)
save.image("C:/Users/laceto/Desktop/myname/list2.RData")

# site3

url <- "https://digilander.libero.it/gioer/nomimaschili.html"
session <- bow(url, force = TRUE, delay = 5)
result <- scrape(session)
rawToChar(result)
node_i <- "p~ table a"

nomi_maschili <- read_html(url) %>%
  html_nodes("td+ td a")%>% html_text()

nomi_maschili <- nomi_maschili[-c(1:8)]

url <- "https://digilander.libero.it/gioer/nomifemminili.html"
session <- bow(url, force = TRUE, delay = 5)
result <- scrape(session)
rawToChar(result)
node_i <- "p~ table a"

nomi_femminili <- read_html(url) %>%
  html_nodes("td+ td a")%>% html_text()

nomi_femminili <- nomi_femminili[-c(1:8)]


list_name <- tibble(sesso = "M",
       nome = nomi_maschili) %>%
  bind_rows(
  tibble(sesso = "F",
            nome = nomi_femminili))
save.image("C:/Users/laceto/Desktop/myname/list3.RData")

load("list1.RData")

list_name_male <- list_name %>%
  dplyr::mutate(sesso = "M") %>%
  dplyr::rename(gender = sesso)
list_name_male

rm(list=setdiff(ls(), "list_name_male"))

load("list1_F.RData")

list_name_female <- list_name %>%
  dplyr::rename(gender = sesso)

list_name_all <- bind_rows(list_name_male, 
                           list_name_female)

rm(list=setdiff(ls(), "list_name_all"))

load("list2.RData")

list_name_all <- list_name %>%
  dplyr::rename(gender = sesso) %>%
  dplyr::bind_rows(., list_name_all)

rm(list=setdiff(ls(), "list_name_all"))

load("list3.RData")

list_name_all <- list_name %>%
  dplyr::rename(name = nome,
                gender = sesso) %>%
  dplyr::bind_rows(., list_name_all)

list_name_all <- list_name_all %>%
  dplyr::mutate(name = trimws(name),
                gender = trimws(gender))

rm(list=setdiff(ls(), "list_name_all"))

male_name <- readLines("nomi maschili.txt") %>% paste(., collapse = ",") %>%
  stringr::str_split(., ",", simplify = F) %>% unlist
female_name <- readLines("nomi femminili.txt") %>% paste(., collapse = ",") %>%
  stringr::str_split(., ",", simplify = F) %>% unlist

list_name_all <- tibble(name = male_name,
                        gender = "M") %>%
  dplyr::bind_rows(tibble(name = female_name,
                          gender = "F")) %>%
  bind_rows(list_name_all)

rm(list=setdiff(ls(), "list_name_all"))

saveRDS(list_name_all, file = "my_data.rds")
