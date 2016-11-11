library("dplyr")
library("data.table")
library("ggplot2")
library("Ckmeans.1d.dp")

# -----

source("summer_olympics/helpers.R")

## Load the datasets.

hosts <- fread("summer_olympics/data/list_of_hosts.csv",
               sep = ",", encoding = "UTF-8", header = FALSE,
               col.names = c("year", "city", "country"))

# Read the datasets as data.table objects.
datasets <- list.files(path = "summer_olympics/data", 
                       pattern = "^[0-9]*_Summer_Olympics_medal_table\\.csv")

dt <- lapply(datasets, function(x) fread(paste0("summer_olympics/data/", x), 
                                         sep = ",", encoding = "UTF-8"))
names(dt) <- gsub("([0-9]*).*", "\\1", datasets)

# Check the structure of the files.
str(dt)

# -----

## Clean data and reshape it.

# Change "NOC" & "Nation" to "nation" to be consistent in all files.
dt <- lapply(dt, function(x) setnames(x, 1, "nation"))

# Recode "Russian Empire", "Soviet Union" and "Unified Team" as "Russia".
dt <- lapply(dt, function(x) {
  setkey(x, nation)
  x[c("Russian Empire", "Soviet Union", "Unified Team"), nation := "Russia"]
})

# Keep only the total of medals won.
dt_total <- lapply(dt, function(x) x[, .(nation, Total)])

# Rename "Total" to "total _xxxx" where xxxx is the year.
dt_total <- lapply(names(dt_total), function(x) {
  setnames(dt_total[[x]], "Total", paste("total", x, sep = "_"))
})

# Create one data frame from the list.
dt_total_wide <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "nation"), 
                        dt_total)

# Reshape data to long format (usually the best format for ggplot).
dt_total_long <- melt(dt_total_wide, id.vars = "nation", 
                      variable.name = "year", value.name = "medals")

# Set "nation" as a key (for really fast subsetting).
setkey(dt_total_long, nation)

# Get the years for the labels of the plots.
years <- gsub("([0-9]*).*", "\\1", datasets)

# -----

## Some basic plots to start: number of medals awarded over time, 
## number of countries that won a least one medal, growth rate...

# Plot the number of medals awarded.
# pdf("./summer_olympics/plots/medals_awarded.pdf", width = 16, height = 9)
dt_total_long[, .(medals = sum(medals, na.rm = TRUE)), by = year] %>%
  setkey(year) %>%
  ggplot(aes(x = year, y = medals)) +
  geom_line(color = "grey50", size = 0.8, group = 1) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 1000, 250), limits = c(0, 1125),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals awarded over time") +
  theme_simple()
# dev.off()

# Plot the number of countries that won at least one medal.
# pdf("./summer_olympics/plots/countries_won_medals.pdf",
#     width = 16, height = 9)
dt_total_long[! is.na(medals), .(number_of_countries = .N),
              by = year][order(year)] %>%
  setkey(year) %>%
  ggplot(aes(x = year, y = number_of_countries)) +
  geom_line(color = "grey50", size = 0.8, group = 1) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 75, 25), limits = c(0, 100),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of countries that won at least one medal") +
  theme_simple()
# dev.off()

# Same plots with the growth rate.
dt_growth <- dt_total_long[, .(medals = sum(medals, na.rm = TRUE)),
                                 by = year][, .(year, growth_medals = medals /
                                                  shift(medals) - 1)]

dt_growth <- merge(dt_growth,
                   dt_total_long[! is.na(medals), .(nb_cnts = .N),
                      by = year][order(year)][, .(year,growth_cnts = nb_cnts /
                                                    shift(nb_cnts) - 1)],
                   by = "year")

# Reshape data to long format and plot it.
gg <- melt(dt_growth, id.vars = "year", value.name = "growth") %>%
  ggplot(aes(x = year, y = growth, group = variable, color = variable)) +
    geom_line(size = 0.8, na.rm = TRUE) +
    scale_x_discrete(labels = years) +
    scale_y_continuous(breaks = seq(-0.5, 1.0, 0.5), limits = c(-0.65, 1.25),
                       expand = c(0, 0)) +
    scale_colour_discrete(labels = c("medals", "countries")) +
    xlab("") + ylab("") +
    ggtitle("Growth rate") +
    theme_simple() +
    theme(legend.position = "none")
# pdf("./summer_olympics/plots/growth_rate_countries_medals.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "Medals & countries awarded",
                     fontsize = 18, col = "grey40")
# dev.off()

# -----

## Time series plots.

gg <- ggplot(data = dt_total_long,
             aes(x = year, y = medals, group = nation)) +
  geom_line(color = "grey50", size = 0.4, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 250), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won") +
  theme_simple()
# pdf("./summer_olympics/plots/ts_all.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "All nations", fontsize = 18, col = "grey40")
# dev.off()

# A function to highlight a given country.
highlight_country <- function(country,
                              color = "#3333cc",
                              base_size = 18,
                              text_size = 20) {
  ggplot(data = dt_total_long[! .(country)], 
         aes(x = year, y = medals, group = nation)) +
    geom_line(color = "grey50", size = 0.4, na.rm = TRUE) +
    geom_line(data = dt_total_long[.(country)], aes(x = year, y = medals),
              color = color, size = 1.2, na.rm = TRUE) +
    scale_x_discrete(labels = years) +
    scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 250),
                       expand = c(0, 0)) +
    xlab("") + ylab("") +
    ggtitle("Number of medals won") +
    theme_simple(base_size = base_size, text_size = text_size) +
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0, size = 24, color = "grey40"))
}

# Highlight the United States.
annotation_1 <- paste0("In 1980, the US and 64 other nations\n",
                       "boycotted the Summer Olympics\n",
                       "which where celebrated in Moscow, Russia")
annotation_2 <- paste0("The 1904 Summer Olympics were \n",
                       "celebrated in St. Louis, Missouri")
annotation_3 <- paste0("The 1932 Summer Olympics were \n",
                       "hosted in Los Angeles, California")
annotation_4 <- paste0("The 1984 Summer Olympics were \n",
                       "held in Los Angeles, California")
annotation_5 <- paste0("The 1996 Summer Olympics\n",
                       "took place in Atlanta, Georgia")

gg <- highlight_country(country = "United States", color = "#3333cc") +
  # 1980: Boycott.
  geom_vline(xintercept = 19, color = "grey50", linetype = "dashed") +
  annotate("text", label = annotation_1, x = 16, y = 215, color = "grey50") +
  # 1904: St. Louis.
  annotate("text", label = annotation_2, x = 5.8, y = 212, color = "grey50") +
  geom_segment(aes(x = 3.1, y = 238, xend = 5, yend = 221), 
               color = "grey70", linetype = "longdash", size = 0.25) +
  # 1932: Los Angeles.
  annotate("text", label = annotation_3, x = 11.8, y = 137, color = "grey50") +
  geom_segment(aes(x = 9.1, y = 103, xend = 10.8, yend = 127), 
               color = "grey70", linetype = "longdash", size = 0.25) +
  # 1984: Los Angeles.
  annotate("text", label = annotation_4, x = 22.8, y = 187, color = "grey50") +
  geom_segment(aes(x = 20.1, y = 174, xend = 20.7, yend = 186),
               color = "grey70", linetype = "longdash", size = 0.25)  +
  # 1996: Atlanta.
  annotate("text", label = annotation_5, x = 24.8, y = 137, color = "grey50") +
  geom_segment(aes(x = 23.2, y = 101, xend = 24.8, yend = 127),
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/ts_us.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "The United States", fontsize = 18, col = "grey40")
# dev.off()

# Highlight Russia.
annotation_1 <- paste0("In 1984, the USSR and 15 other nations\n",
                       "boycotted the Summer Olympics\n",
                       "which where celebrated in Los Angeles, California")
annotation_2 <- "The 1980 Summer Olympics were \nheld in Moscow, Russia"

gg <- highlight_country(country = "Russia", color = "#ff0000") +
  # 1984: Boycott.
  geom_vline(xintercept = 20, color = "grey50", linetype = "dashed") +
  annotate("text", label = annotation_1, x = 23.5, y = 225, color = "grey50") +
  # 1980: Moscow.
  annotate("text", label = annotation_2, x = 15, y = 212, color = "grey50") +
  geom_segment(aes(x = 18.9, y = 195, xend = 17.1, yend = 208),
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/ts_russia.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "Russia", fontsize = 18, col = "grey40")
# dev.off()

# Highlight GB.
annotation_1 <- paste0("The 1908 Summer Olympics were\n",
                       "held in London, United Kingdom")
annotation_2 <- paste0("The 1948 Summer Olympics were\n",
                       "celebrated in London, United Kingdom")
annotation_3 <- paste0("The 2012 Summer Olympics also\n",
                       "took place in London, United Kingdom")

gg <- highlight_country(country = "Great Britain", color = "#3333cc") +
  # 1908: London.
  annotate("text", label = annotation_1, x = 6.5, y = 162, color = "grey50") +
  geom_segment(aes(x = 4.1, y = 146, xend = 5.5, yend = 153), 
               color = "grey70", linetype = "longdash", size = 0.25) +
  # 1948: London.
  annotate("text", label = annotation_2, x = 14.5, y = 137, color = "grey50") +
  geom_segment(aes(x = 11, y = 24.5, xend = 14.5, yend = 128), 
               color = "grey70", linetype = "longdash", size = 0.25) +
  # 2012: London.
  annotate("text", label = annotation_3, x = 25, y = 137, color = "grey50") +
  geom_segment(aes(x = 27, y = 67, xend = 25.3, yend = 129), 
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/ts_gb.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "Great Britain", fontsize = 18, col = "grey40")
# dev.off()

# Highlight China.
annotation_1 <- "The 2008 Summer Olympics were \nheld in Beijing, China"

gg <- highlight_country(country = "China", color = "#ff0000") +
  # 2008: Beijing.
  annotate("text", label = annotation_1, x = 24, y = 162, color = "grey50") +
  geom_segment(aes(x = 25.9, y = 102, xend = 24.3, yend = 153), 
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/ts_china.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "China", fontsize = 18, col = "grey40")
# dev.off()

# Highlight Romania.
annotation_1 <- paste0("The 1984 Summer Olympics were\n",
                       "held in Los Angeles, California and\n",
                       "boycotted by the USSR and 15 other nations")

gg <- highlight_country(country = "Romania", color = "#ffcc00") +
  # 1984: Boycott.
  annotate("text", label = annotation_1, x = 14, y = 137, color = "grey50") +
  geom_segment(aes(x = 19.9, y = 53, xend = 14, yend = 125), 
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/ts_romania.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "Romania", fontsize = 18, col = "grey40")
# dev.off()

# Highlight both the United States and Russia at the same time.
gg <- ggplot(data = dt_total_long[! c("United States", "Russia")],
             aes(x = year, y = medals, group = nation)) +
  geom_line(color = "grey50", size = 0.4, na.rm = TRUE) +
  geom_line(data = dt_total_long[.("United States")], aes(x = year, y = medals),
            color = "#3333cc", size = 1.2, na.rm = TRUE) +
  geom_line(data = dt_total_long[.("Russia")], aes(x = year, y = medals),
            color = "#ff0000", size = 1.2, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 250),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won") +
  theme_simple()
# pdf("./summer_olympics/plots/ts_us_russia.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, "The United States and Russia", fontsize = 18, 
                     col = "grey40")
# dev.off()

# Group the countries by continent.
continents <- fread("summer_olympics/data/continents.txt",
                    sep = ";", encoding = "UTF-8", header = TRUE)

# Find the countries not present in "continents" that we have to rename.
dt_continents <- copy(dt_total_wide)
wrong_names <- dt_continents[! .(continents[, country]), .(nation)] %>%
  unique()

# "Independent Olympic Athletes", "Independent Olympic Participants" &
# "Mixed team" are recoded as NA. For the others we use the names that exist
# in "continents".
wrong_names[, nation_rec := c("Australia", "Czech Republic", "Jamaica",
                              "Sri Lanka", "Taiwan", "Czech Republic",
                              "Germany", "United Kingdom", NA, NA, NA, "Aruba",
                              "China", "Serbia", "Egypt",
                              "United States of America", "Puerto Rico",
                              "Germany", "Serbia")]

# Rename these countries.
dt_continents[wrong_names[, nation], nation := wrong_names[, nation_rec]]
setkey(dt_continents, nation)

# Remove NA.
dt_continents <- dt_continents[! is.na(dt_continents[, nation]), ]

# Get the continent for each nation.
dt_continents <- merge(dt_continents, continents,
                      by.x = "nation", by.y = "country", all.x = TRUE)

# Reshape data to long format (usually the best format for ggplot).
dt_continents <- melt(dt_continents, id.vars = c("nation", "continent"),
                      variable.name = "year", value.name = "medals")

# Remove the variable "nation" which is now useless.
dt_continents[, nation := NULL]

# Get the total of medals won by continent.
dt_continents[, medals := sum(medals, na.rm = TRUE), by = .(year, continent)]
dt_continents <- unique(dt_continents)

# Plot the continents (and the countries).
# Africa: brown -> #b79f00
# Asia: green -> #00ba38
# Europe: blue -> #619cff
# North America: yellow -> #f4c300
# South America: red -> #f8766d
# Oceania: purple -> #c77cff
cols <- c("Africa" = "#b79f00", "Asia" = "#00ba38", "Europe" = "#619cff",
          "North America" = "#f4c300", "South America" = "#f8766d",
          "Oceania" = "#c77cff")

gg <- ggplot(data = dt_continents,
             aes(x = year, y = medals, group = continent, color = continent)) +
  geom_line(data = dt_total_long, aes(x = year, y = medals, group = nation),
            color = "grey50", size = 0.4, alpha = 0.3, na.rm = TRUE) +
  geom_line(size = 1.2, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 500, 250), limits = c(0, 600),
                     expand = c(0, 0)) +
  scale_color_manual(values = cols) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won") +
  theme_simple()
# pdf("./summer_olympics/plots/ts_continents.pdf", width = 16, height = 9)
ggplot_with_subtitle(gg, paste0("Africa, Asia, Europe, North & ",
                                "South America and Oceania"),
                     fontsize = 18, col = "grey40")
# dev.off()

# -----

## Compare the number of medals won with the percentage of medals won.
## (Because the number of medals has increased over time.)

dt_total_wide_per <- copy(dt_total_wide)

# Compute the percentage of medals won for each occasion.
for (col in setdiff(names(dt_total_wide_per), "nation")) {
  dt_total_wide_per[, (col) := (get(col) / sum(get(col), na.rm = TRUE))]
}

# Reshape data to long format (usually the best format for ggplot).
dt_total_long_per <- melt(dt_total_wide_per, id.vars = "nation",
                          variable.name = "year", value.name = "medals")

# Set "nation" as a key (for really fast subsetting).
setkey(dt_total_long_per, nation)

# Plot both the number and the percentage of medals won and
# highlight the United States.
gg1 <- highlight_country(country = "United States", color = "#3333cc",
                         base_size = 14, text_size = 20) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75))

gg2 <- ggplot(data = dt_total_long_per[!.("United States")],
              aes(x = year, y = medals, group = nation)) +
  geom_line(color = "grey50", size = 0.8, na.rm = TRUE) +
  geom_line(data = dt_total_long_per[.("United States")], 
            aes(x = year, y = medals), color = "#3333cc", size = 1.2) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 0.8, 0.2), limits = c(0, 0.95), 
                     expand = c(0, 0), labels = scales::percent) +
  xlab("") + ylab("") +
  ggtitle("Percentage of medals won") +
  theme_simple(base_size = 14, text_size = 20) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75))

# pdf("./summer_olympics/plots/us_number_per.pdf", width = 16, height = 9)
gridExtra::grid.arrange(gg1, gg2, ncol = 2)
# dev.off()

# -----

## Growth rate of the proportion of medals won.

dt_growth_rate <- copy(dt_total_long_per)
dt_growth_rate[! is.na(medals), growth := medals / shift(medals) - 1,
               by = nation]

# Higlight the United States.
annotation_1 <- paste0("In 1980, the US and 64 other nations\n",
                       "boycotted the Summer Olympics\n",
                       "which where celebrated in Moscow, Russia")
annotation_2 <- paste0("The Summer Olympics were held in the US\n",
                       "in four occasions: 1904, 1932, 1984, and 1996")

# pdf("./summer_olympics/plots/us_growth_rate.pdf", width = 16, height = 9)
gg <- ggplot(dt_growth_rate[.("United States"), ], aes(x = year, y = growth)) +
  # 1980: Boycott.
  geom_vline(xintercept = 19, color = "grey50", linetype = "dashed") +
  annotate("text", label = annotation_1, x = 16, y = 3.5, color = "grey50") +
  # 1904: St. Louis.
  geom_segment(aes(x = 3.1, y = 3.85, xend = 11.5, yend = 1.7),
               color = "grey70", linetype = "longdash", size = 0.25) +
  geom_line(color = "grey50", size = 0.8, group = 1, na.rm = TRUE) +
  # 1932: Los Angeles.
  geom_segment(aes(x = 9.1, y = 0.76, xend = 10.5, yend = 1.3), 
               color = "grey70", linetype = "longdash", size = 0.25) +
  # 1984: Los Angeles.
  geom_segment(aes(x = 19.9, y = 0.65, xend = 16, yend = 1.5),
               color = "grey70", linetype = "longdash", size = 0.25)  +
  # 1996: Atlanta.
  geom_segment(aes(x = 22.9, y = -0.09, xend = 14, yend = 1.3),
               color = "grey70", linetype = "longdash", size = 0.25) +
  annotate("text", label = annotation_2, x = 13, y = 1.5, color = "grey50") +
  geom_line(data = dt_growth_rate[nation == "United States" &
                                    year %in% c("total_1976", "total_1984"), ],
            aes(x = year, y = growth),
            color = "grey50", size = 0.8, group = 1, linetype = "dashed") +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(-1, 4, 1), limits = c(-1.5, 4.5), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Growth rate of the proportion of medals won") +
  theme_simple()
ggplot_with_subtitle(gg, "The United States", fontsize = 18, col = "grey40")
# dev.off()

# -----

## Let's compare London 2012 and Rio 2016 more in depth. 

# Merge data of London and Rio together.
london_rio <- merge(dt[["2012"]][, .(Total = sum(Total)), by = nation],
                    dt[["2016"]][, .(Total = sum(Total)), by = nation],
                    by = "nation", all = TRUE)
setnames(london_rio, c("nation", "total_london", "total_rio"))
london_rio <- london_rio[! is.na(london_rio[, nation]), ]

# Reshape data to long format (usually the best format for ggplot).
london_rio_long <- melt(london_rio, id.vars = "nation",
                        value.name = "total")
london_rio_long[variable == "total_london", variable := "London 2012"]
london_rio_long[variable == "total_rio", variable := "Rio 2016"]

# Sort the countries by the number of medals won in Rio.
order_countries <- dplyr::arrange(london_rio, desc(total_rio))[, "nation"]

london_rio_long[, nation := factor(london_rio_long$nation, 
                                   levels = order_countries)]

# Histogram.
# pdf("./summer_olympics/plots/hist_london_rio.pdf", width = 16, height = 9)
ggplot(london_rio_long, aes(x = total)) +
  geom_histogram(binwidth = 3, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  xlab("") + ylab("") +
  facet_grid(~ variable) +
  ggtitle("Number of medals won by country") +
  theme_simple() +
  theme(strip.text = element_text(color = "grey40"),
        strip.background = element_rect(color = "grey80", fill = "grey95"))
# dev.off()

# Dotplot.
# pdf("./summer_olympics/plots/dotplot_london_rio.pdf", width = 16, height = 9)
ggplot(london_rio_long, aes(x = variable, y = total)) +
  geom_jitter(size = 3, color = "#595959", width = 0.25, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 135), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won by country") +
  theme_simple()
# dev.off()

# Top 10 in Rio.
# pdf("./summer_olympics/plots/dotplot_london_rio_top_10.pdf",
#     width = 16, height = 9)
gg <- ggplot(london_rio_long[nation %in% order_countries[1:10], ]) +
  geom_segment(data = london_rio[nation %in% order_countries[1:10], ],
               aes(x = nation, xend = nation,
                   y = london_rio[nation %in% order_countries[1:10],
                                  total_london],
                   yend = london_rio[nation %in% order_countries[1:10],
                                     total_rio])) +
  geom_point(aes(x = nation, y = total, color = variable), size = 10) +
  geom_text(aes(x = nation, y = total, label = total), color = "grey20") +
  scale_x_discrete(limits = order_countries[1:10]) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won by country") +
  coord_flip() +
  theme_simple()
ggplot_with_subtitle(gg, "London 2012 vs Rio 2016 (top 10 in Rio)",
                     fontsize = 18, col = "grey40")
# dev.off()

# -----

## Let's make some maps!

world <- map_data("world")
world <- world[world$region != "Antarctica", ]

# The colors we'll use (a variation of greens).
cols <- c("#bae4b3", "#74c476", "#31a354", "#006d2c", "#1b5030")

# Get the longitude and the latitude of the cities which hosted the games.
geocodes <- paste(hosts$city, hosts$country, sep = ", ") %>%
  ggmap::geocode()
hosts <- cbind(hosts, geocodes)

# Find the countries that should be renamed (to be consistent with the names
# used in world).
wrong_names <- lapply(dt, function(x) {
  setkey(x, nation)
  x[! .(unique(world[, "region"])), .(nation)]
})

wrong_names <- rbindlist(wrong_names) %>%
  unique() %>%
  setorder()

# "Independent Olympic Athletes", "Independent Olympic Participants" &
# "Mixed team" are recoded as NA. For the others we use the names that exist
# in "world".
wrong_names[, nation_rec := c("Australia", "Czech Republic", "Jamaica",
                              "Sri Lanka", "Taiwan", "Czech Republic",
                              "Germany", "UK", "China", NA, NA, NA, "Aruba",
                              "China", "Serbia", "Trinidad", "Egypt", "USA",
                              "Germany", "Serbia")]

# Rename these countries.
dt <- lapply(dt, function(x) {
  x[wrong_names[, nation], nation := wrong_names[, nation_rec]]
  setkey(x, nation)
})

# A function to plot a map for each occasion of the Summer Olympics.
plot_map <- function(x) {
  # Explicitly copy the data.table.
  temp <- copy(dt[[x]])
  
  # Sum the total of medals by country (in case of some countries appeared
  # more than once).
  temp[, Total := sum(Total), by = nation]
  
  # Create the breaks with an univariate kmeans algorithm.
  set.seed(42)
  temp$cluster <- Ckmeans.1d.dp(temp[, Total], k = 5)$cluster
  
  temp <- merge(world, temp[, .(nation, cluster)],
                by.x = "region", by.y = "nation", all.x = TRUE)
  
  # Reorder the rows (required when using ggplot2 to make maps).
  setorderv(temp, c("group", "order"))
  
  ggplot() +
    geom_map(data = temp, map = world, aes(x = long, y = lat, map_id = region,
                                           fill = factor(cluster)),
             color = "grey50", size = 0.1) +
    ggalt::coord_proj("+proj=wintri") +  # A good projection of the world.
    scale_fill_manual(values = cols, na.value = "grey99") +
    annotate("text", label = hosts[x, year],
             x = -160, y = -53, color = "grey99", size = 32) +
    # geom_point(data = hosts[year], aes(x = lon, y = lat),
    #            size = 1, color = "#000000") +
    theme_map()
}

# Create the map for each occasion.
for (year in seq_along(dt)) {
# pdf(paste0("./summer_olympics/plots/maps/map_", hosts[year, year], ".pdf"),
#     width = 15.93, height = 9)
print(plot_map(year))
# dev.off()
}

