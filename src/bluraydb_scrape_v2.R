# blu ray scraper v2.0

## Load libraries ----
library(dplyr)
library(magrittr)
library(rvest)
library(stringr)
library(xml2)


## URLs ----

### Base
con_base <- "https://www.blu-ray.com/community/collection.php?u=483309&categoryid="

### Media types
media <- c("4K" = "7&4k=1", "blu_ray" = "7", "dvd" = "21")



## Get HTML data ----

### Blu-ray example
con_base <- paste0(con_base, media[["blu_ray"]], "&page=")
i <- 0

url_data   <- list()
title_data <- list()
img_data   <- list()


repeat {
    con <- paste0(con_base, i)

    urls <- xml2::read_html(con) %>%
        rvest::html_nodes(".hoverlink") %>%
        rvest::html_attr("href")

    titles <- xml2::read_html(con) %>%
        rvest::html_nodes(".hoverlink") %>%
        rvest::html_attr("title")

    img <- xml2::read_html(con) %>%
        rvest::html_nodes(".hoverlink .cover") %>%
        rvest::html_attr("src")

    ## Break out
    if (length(titles) == 0) {
        message("Finished!")
        break()
    } else {
        message("Scraping page ", i + 1, " ...")
        message("   -- ", con)
        url_data[[i + 1]]   <- urls
        title_data[[i + 1]] <- titles
        img_data[[i + 1]]   <- urls
        i <- i + 1
    }

}

url_data   <- url_data %>% unlist()
title_data <- title_data %>% unlist()
img_data   <- img_data %>% unlist()


