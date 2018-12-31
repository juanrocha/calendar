# library(prophet)
library(tidyverse)
library(lubridate)
library(emo)

df <- data_frame(
    ds = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days"),
    y = seq(1, 365, by = 1)
)
## They don't have the holidays for Colombia
# add_country_holidays(df, "Colombia")

library(RSelenium)
library(XML)

## Initialize remote driver
d <- rsDriver() # should open a chrome
remDr <- d[["client"]]
remDr$navigate("https://publicholidays.co/es/2019-dates/")
Sys.sleep(3)

tbl <- remDr$findElement(using = "tag name", value = "table")

tbl$getElementText()

## another suggestion from stakoverflow:

doc <- htmlParse(remDr$getPageSource()[[1]])
tbl <- readHTMLTable(doc)

remDr$navigate("https://www.calendar-12.com/moon_phases/2019")

doc <- htmlParse(remDr$getPageSource()[[1]])
tbl2 <- readHTMLTable(doc)

remDr$close()


## entry 10 is an error
class(tbl[[1]])
tbl <- tbl[[1]]
tbl <- tbl[-10,]

tbl$date <- c(
    "2019-01-01",
    "2019-01-07",
    "2019-03-25",
    "2019-04-18",
    "2019-04-19",
    "2019-05-01",
    "2019-06-03",
    "2019-06-24",
    "2019-07-01",
    "2019-07-01",
    "2019-07-20",
    "2019-08-07",
    "2019-08-19",
    "2019-10-14",
    "2019-11-04",
    "2019-11-11",
    "2019-12-08",
    "2019-12-25"
)

df <- df %>%
    mutate(
        day = day(ds),
        week_day = wday(ds),
        week = epiweek(ds),
        month = months(ds),
        year_day = yday(ds))

df %>%
    select(-y) %>%
    mutate(x = 0.9, y = 0.9) %>%
    filter(month == "January") %>%
    ggplot(aes(x,y)) +
    geom_text(aes(label = day), size = 4, color = 'gray') +
    lims(x = c(0,1), y = c(0,1)) +
    labs(x = "", y = "", title = "Enero 2019") +
    facet_grid(week ~ week_day) +
    theme_linedraw(base_size = 12) +
    theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank(),
        strip.background = element_rect(fill = "gray50")
    )
