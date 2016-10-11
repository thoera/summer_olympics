library("dplyr")
library("data.table")
library("ggplot2")

# -----

source("summer_olympics/helpers.R")

## Load the datasets.

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

# Keep only the total of medals earned.
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

# Plot the number of medals distributed.
dt_total_long[, .(medals = sum(medals, na.rm = TRUE)), by = year] %>%
  setkey(year) %>%
  ggplot(aes(x = year, y = medals)) +
  geom_line(color = "grey50", size = 0.8, group = 1) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 1000, 250), limits = c(0, 1125),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals distributed") +
  theme_simple()

# Plot the number of countries that won medals.
dt_total_long[!is.na(medals), .(number_of_countries = .N), by = year] %>%
  setkey(year) %>%
  ggplot(aes(x = year, y = number_of_countries)) +
  geom_line(color = "grey50", size = 0.8, group = 1) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 75, 25), limits = c(0, 100),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of countries that won medals") +
  theme_simple()

# -----

## Time series plots.

gg <- ggplot(data = dt_total_long,
             aes(x = year, y = medals, group = nation)) +
  geom_line(color = "grey50", size = 0.8, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 250), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won") +
  theme_simple()
# pdf("./summer_olympics/plots/plot_all.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "All nations", fontsize = 18, col = "grey40")
# dev.off()

# A function to highlight a given country.
highlight_plot <- function(country, color, base_size = 18, text_size = 20) {
  ggplot(data = dt_total_long[!.(country)], 
         aes(x = year, y = medals, group = nation)) +
    geom_line(color = "grey50", size = 0.8, na.rm = TRUE) +
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

gg <- highlight_plot(country = "United States", color = "#3333cc") +
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
# pdf("./summer_olympics/plots/plot_us.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "The United States", fontsize = 18, col = "grey40")
# dev.off()

# Highlight Russia.
annotation_1 <- paste0("In 1984, the USSR and 15 other nations\n",
                       "boycotted the Summer Olympics\n",
                       "which where celebrated in Los Angeles, California")
annotation_2 <- "The 1980 Summer Olympics were \nheld in Moscow, Russia"

gg <- highlight_plot(country = "Russia", color = "#ff0000") +
  # 1984: Boycott.
  geom_vline(xintercept = 20, color = "grey50", linetype = "dashed") +
  annotate("text", label = annotation_1, x = 23.5, y = 225, color = "grey50") +
  # 1980: Moscow.
  annotate("text", label = annotation_2, x = 15, y = 212, color = "grey50") +
  geom_segment(aes(x = 18.9, y = 195, xend = 17.1, yend = 208),
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/plot_russia.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "Russia", fontsize = 18, col = "grey40")
# dev.off()

# Highlight GB.
annotation_1 <- paste0("The 1908 Summer Olympics were\n",
                       "held in London, United Kingdom")
annotation_2 <- paste0("The 1948 Summer Olympics were\n",
                       "celebrated in London, United Kingdom")
annotation_3 <- paste0("The 2012 Summer Olympics also\n",
                       "took place in London, United Kingdom")

gg <- highlight_plot(country = "Great Britain", color = "#3333cc") +
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
# pdf("./summer_olympics/plots/plot_gb.pdf", 
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "Great Britain", fontsize = 18, col = "grey40")
# dev.off()

# Highlight China.
annotation_1 <- "The 2008 Summer Olympics were \nheld in Beijing, China"

gg <- highlight_plot(country = "China", color = "#ff0000") +
  # 2008: Beijing.
  annotate("text", label = annotation_1, x = 24, y = 162, color = "grey50") +
  geom_segment(aes(x = 25.9, y = 102, xend = 24.3, yend = 153), 
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/plot_china.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "China", fontsize = 18, col = "grey40")
# dev.off()

# Highlight Romania.
annotation_1 <- paste0("The 1984 Summer Olympics were\n",
                       "held in Los Angeles, California and\n",
                       "boycotted by the USSR and 15 other nations")

gg <- highlight_plot(country = "Romania", color = "#ffcc00") +
  # 1984: Boycott.
  annotate("text", label = annotation_1, x = 14, y = 137, color = "grey50") +
  geom_segment(aes(x = 19.9, y = 53, xend = 14, yend = 125), 
               color = "grey70", linetype = "longdash", size = 0.25)
# pdf("./summer_olympics/plots/plot_romania.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "Romania", fontsize = 18, col = "grey40")
# dev.off()

# Highlight both the United States and Russia.
gg <- ggplot(data = dt_total_long[! c("United States", "Russia")],
             aes(x = year, y = medals, group = nation)) +
  geom_line(color = "grey50", size = 0.8, na.rm = TRUE) +
  geom_line(data = dt_total_long[.("United States")], aes(x = year, y = medals),
            color = "#3333cc", size = 1.2, na.rm = TRUE) +
  geom_line(data = dt_total_long[.("Russia")], aes(x = year, y = medals),
            color = "#ff0000", size = 1.2, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 250),
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won") +
  theme_simple() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0, size = 24, color = "grey40"))
# pdf("./summer_olympics/plots/plot_us_russia.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "The United States and Russia", fontsize = 18, 
                     col = "grey40")
# dev.off()

# -----

## Maps.

world <- map_data("world")

london_2012 <- copy(dt[[27]])  # Explicitly copy the data.table.

# Find which countries have a different name in the two datasets.
london_2012[!.(unique(world$region)), .(nation)]

# Rename those countries.
london_2012[c("Great Britain", "Trinidad and Tobago", "United States"),
            nation := c("UK", "Trinidad", "USA")]
setkey(london_2012, nation)

# Histogram.
gg <- ggplot(london_2012, aes(x = Total)) +
  geom_histogram(binwidth = 3) +
  scale_x_continuous(breaks = seq(0, 100, 25)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won by country") +
  theme_simple()
# pdf("./summer_olympics/plots/hist_london_2012.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "London 2012", fontsize = 18, col = "grey40")
# dev.off()

# Create the breaks.
breaks <- c(0, 5, 20, 40, 80, max(london_2012$Total))
london_2012$total_london_2012 <- cut(london_2012$Total, breaks = breaks, 
                                     labels = FALSE)

# The colors we'll use.
cols <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# Merge with the second dataset.
world <- merge(world, london_2012[, .(nation, total_london_2012)],
               by.x = "region", by.y = "nation", all.x = TRUE)

# Reorder the rows (needed by ggplot if we want a "correct" map).
setorderv(world, c("group", "order"))

# pdf("./summer_olympics/plots/map_london_2012.pdf",
#     width = 16, height = 9)
ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = region, 
               fill = factor(total_london_2012)),
           color = "grey40", size = 0.2) +
  coord_map(projection = "mercator", xlim = c(-180, 180), ylim = c(-60, 90)) +
  scale_fill_manual(values = cols) +
  ggtitle("London 2012") +
  theme_map()
# dev.off()

# -----

rio_2016 <- copy(dt[[28]])  # Explicitly copy the data.table.

# Find which countries have a different name in the two datasets.
rio_2016[!.(unique(world$region)), .(nation)]

# Rename those countries.
rio_2016[c("Great Britain", "Trinidad and Tobago", "United States"),
         nation := c("UK", "Trinidad", "USA")]
setkey(rio_2016, nation)

# Histogram.
gg <- ggplot(rio_2016, aes(x = Total)) +
  geom_histogram(binwidth = 3) +
  scale_x_continuous(breaks = seq(0, 120, 20)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won by country") +
  theme_simple()
# pdf("./summer_olympics/plots/hist_rio_2016.pdf",
#     width = 16, height = 9)
ggplot_with_subtitle(gg, "Rio 2016", fontsize = 18, col = "grey40")
# dev.off()

# Create the breaks.
breaks <- c(0, 5, 20, 40, 80, max(rio_2016$Total))
rio_2016$total_rio_2016 <- cut(rio_2016$Total, breaks = breaks, labels = FALSE)

# Merge with the second dataset.
world <- merge(world, rio_2016[, .(nation, total_rio_2016)],
               by.x = "region", by.y = "nation", all.x = TRUE)

# Reorder the rows (needed by ggplot if we want a "correct" map).
setorderv(world, c("group", "order"))

# pdf("./summer_olympics/plots/map_rio_2016.pdf",
#     width = 16, height = 9)
ggplot() +
  geom_map(data = world, map = world,
           aes(x = long, y = lat, map_id = region, 
               fill = factor(total_rio_2016)),
           color = "grey40", size = 0.2) +
  coord_map(projection = "mercator", xlim = c(-180, 180), ylim = c(-60, 90)) +
  scale_fill_manual(values = cols) +
  ggtitle("Rio 2016") +
  theme_map()
# dev.off()

# A facet plot rather than two histograms.

# A dotplot london 2012 -> rio 2016
        # london_2012  rio_2016
# us      x            y
# uk      x            y
# china   x            y

london_rio <- merge(london_2012[, .(nation, Total)], 
                    rio_2016[, .(nation, Total)], by = "nation", all = TRUE)
setnames(london_rio, c("nation", "total_london", "total_rio"))

london_rio_long <- melt(london_rio, id.vars = "nation", value.name = "total")

london_rio_long[variable == "total_london", variable := "London 2012"]
london_rio_long[variable == "total_rio", variable := "Rio 2016"]

# Histogram.
# pdf("./summer_olympics/plots/hist_london_rio.pdf",
#     width = 16, height = 9)
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
# pdf("./summer_olympics/plots/dotplot_london_rio.pdf",
#     width = 16, height = 9)
ggplot(london_rio_long, aes(x = variable, y = total)) +
  geom_jitter(size = 3, color = "#595959", width = 0.25, na.rm = TRUE) +
  scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 135), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Number of medals won by country") +
  theme_simple()
# dev.off()

setorder(london_rio_long, variable, -total, na.last = TRUE)

# Sort the countries by the number of medals won in Rio.
order_countries <- dplyr::arrange(london_rio, desc(total_rio))[, "nation"]

london_rio_long[, nation := factor(london_rio_long$nation, 
                                   levels = order_countries)]

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

dt_total_wide_per <- copy(dt_total_wide)

for (col in setdiff(names(dt_total_wide), "nation")) {
  dt_total_wide_per[, (col) := (get(col) / sum(get(col), na.rm = TRUE))]
}

# Reshape data to long format (usually the best format for ggplot).
dt_total_long_per <- melt(dt_total_wide_per, id.vars = "nation",
                          variable.name = "year", value.name = "medals")

# Set "nation" as a key (for really fast subsetting).
setkey(dt_total_long_per, nation)

gg1 <- highlight_plot(country = "United States", color = "#3333cc",
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

# pdf("./summer_olympics/plots/plot_us_number_per.pdf",
#     width = 16, height = 9)
gridExtra::grid.arrange(gg1, gg2, ncol = 2)
# dev.off()

# Growth rate of the number of medals.
dt_us_growth_rate <- dt_total_long_per[.("United States"), .(year, medals)]
dt_us_growth_rate[year != "total_1980", growth := medals / shift(medals) - 1]

annotation_1 <- paste0("In 1980, the US and 64 other nations\n",
                       "boycotted the Summer Olympics\n",
                       "which where celebrated in Moscow, Russia")
annotation_2 <- paste0("The Summer Olympics were held in the US\n",
                       "in four occasions: 1904, 1932, 1984, and 1996")

# pdf("./summer_olympics/plots/plot_us_growth_rate.pdf",
#     width = 16, height = 9)
gg <- ggplot(dt_us_growth_rate, aes(x = year, y = growth)) +
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
  geom_line(data = dt_us_growth_rate[year %in% c("total_1976", "total_1984"), ],
            aes(x = year, y = growth),
            color = "grey50", size = 0.8, group = 1, linetype = "dashed") +
  geom_point(color = "grey50", size = 4, na.rm = TRUE) +
  scale_x_discrete(labels = years) +
  scale_y_continuous(breaks = seq(-1, 4, 1), limits = c(-1.5, 4.5), 
                     expand = c(0, 0)) +
  xlab("") + ylab("") +
  ggtitle("Growth rate of the proportion of medals won") +
  theme_simple()
ggplot_with_subtitle(gg, "The United States", fontsize = 18, col = "grey40")
# dev.off()