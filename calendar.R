# library(prophet)
library(tidyverse)
library(lubridate)
# library(emo)
library(emojifont)

df <- data_frame(
    date = seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="days"),
    y = seq(1, 365, by = 1)
)
## prophet: They don't have the holidays for Colombia
# add_country_holidays(df, "Colombia")

## Webscrap the holidays and lunar phases:
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
tbl <- readHTMLTable(doc) ## tabla de festivos

remDr$navigate("https://www.calendar-12.com/moon_phases/2019")

doc <- htmlParse(remDr$getPageSource()[[1]])
tbl2 <- readHTMLTable(doc) ## dos tablas con fases lunares

remDr$close()


## entry 8 is an error
class(tbl[[1]])
tbl <- tbl[[1]]
tbl <- tbl[-8,]

## Add manually dates for holidays.
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

tbl <- tbl %>%
    mutate(date = as.Date(date)) %>%
    select(date, festivos = "Días festivos") %>%
    mutate(festivos = as.character(festivos))

## Add carnaval
tbl <- tbl  %>%
    bind_rows(data_frame(
    date = as.Date(c("2019-03-02", "2019-03-03","2019-03-04","2019-03-05")),
    festivos = rep("Carnaval",4)))

tbl <- tbl[-10,] # delete sagrado corazon because it's the same date as others, no need to mark the same red day twice.

tbl2 <- tbl2[[2]] %>%
    select(1,2) %>%
    rename(lunar_phase = V1, date = V2) %>%
    mutate(date = str_sub(date, start = 1L, end = -3),
        date = str_c(date, "2019"),
        date = mdy(date),
        ## the following is to change the name of the phase to match the name of the emojis
        lunar_phase = str_to_lower(lunar_phase),
        lunar_phase = str_replace(lunar_phase, " ", "_"),
        lunar_phase = str_remove(lunar_phase, "_moon" ),
        lunar_phase = str_c(lunar_phase, "_moon"))


df <- df %>%
    mutate(
        day = day(date),
        week_day = wday(date),
        week = epiweek(date),
        month = months(date),
        year_day = yday(date)) %>%
    select(-y)

df <- left_join(df, tbl) %>%
    left_join(tbl2)

## some last adjustments:

week_days <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")

df <- df %>%
    mutate(day_names = week_days[week_day]) %>%
    mutate(month = as_factor(month))

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

levels(df$month) <- meses

df <- df %>%
    mutate(day_names = as_factor(day_names),
        day_names = fct_reorder(day_names, week_day))


df <- df %>% mutate(
    day_type = ifelse(week_day == 1 | week_day == 7, "weekend", "weekday")
)

%>% mutate(day_type = if(!is.na(festivos))"festivo") %>% select(8:10)

load.emojifont("OpenSansEmoji.ttf")

df %>%
    mutate(x = 0.9, y = 0.9) %>%
    filter(month == "Enero") %>%
    ggplot(aes(x,y)) +
    geom_text(aes(label = day, color = day_type),
        size = 4, show.legend = FALSE) +
    geom_text(aes(label = festivos), color = "purple",
        x = 0., y = 0.7, size = 2, hjust = "left") +
    # geom_text(
    #     data = filter(df, month == "Enero", !is.na(lunar_phase)) ,
    #     aes(label = emoji(lunar_phase)), x = 0.1, y = 0.9, size = 5,
    #     family = "OpenSansEmoji"
    #     ) +
    geom_emoji(
        data = filter(df, month == "Enero", !is.na(lunar_phase)) ,
        aes(alias = lunar_phase ), x = 0.1, y = 0.9, size = 5
        #family = "OpenSansEmoji"
        ) +
    scale_color_manual(values = c("black", "red")) +
    lims(x = c(0,1), y = c(0,1)) +
    labs(x = "", y = "", title = "Enero 2019") +
    facet_grid(week ~ day_names, switch = "y") +
    theme_light(base_size = 12) +
    theme(
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(), panel.grid = element_blank(),
        #strip.background = element_rect(fill = "gray50"),
        panel.spacing = unit(1, "mm"),
        strip.text.y = element_text(angle = 180)
    )


ggplot(data = data_frame(x=0.5, y = 0.5, lab = "moon"), aes(x,y)) +
geom_text(aes(label = emoji(lab)), family = "EmojiOne")
