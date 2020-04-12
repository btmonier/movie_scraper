#!/usr/bin/env Rscript

#--------------------------------------------------------------------
# Script Name:   bluraydb_movie_scraper.R
# Description:   Scrape blu-ray.com for movie metadata
# Author:        Brandon Monier
# Created:       2020-04-11 at 22:04:54
# Last Modified: 2020-04-12 at 00:25:00
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Detailed Purpose:
#    The main purpose of this Rscript is to scrape through all
#    pages of my blu-ray.com profile in order to obtain movie
#    information.
#
# Note:
#    This will extract information related the film that is
#    contained on related media.
#--------------------------------------------------------------------

# === Preamble ======================================================

## Load libraries ----
library(magrittr)
library(rvest)
library(xml2)


## Variables ----

### Base
con_base <- "https://www.blu-ray.com/community/collection.php?u=483309&action=hybrid&page="


## WIP test ----

### Get movie URL data...
j <- 0
con <- paste0(con_base, j)

urls <- xml2::read_html(con) %>%
    rvest::html_nodes("td.middle a") %>%
    rvest::html_attr("href")

movies <- xml2::read_html(con) %>%
    rvest::html_nodes("a.alphaborder.hoverlink") %>%
    rvest::html_attr("href")

### TODO: find a way to convert a vector to a list with given
###       elements in vector as list elements...


