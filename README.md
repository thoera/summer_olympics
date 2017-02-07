![olympic_rings.png](olympic_rings.png?raw=true)

<br>

Because sport is always a good way to find interesting datasets, I chose to spend some time in the world of the Summer Olympic Games. This "analysis" is a really superficial one and it's more a pretext than anything else to work with the well known `R` graphical package `ggplot2`.

# Find some data

When I don't have a dataset pre-made, my first instinct is to look on [Wikipedia](https://en.wikipedia.org) and, more often than not, it's enough to fulfill my needs. This time again, Wikipedia was a great resource: you can find the medal table for each event from [1896 to 2016](https://en.wikipedia.org/wiki/Category:Summer_Olympics_medal_tables). Great!

At this point some web scraping is needed. My preferred tool in that situation is the `Python` library `Beautiful Soup`. It's well documented and perfectly suited for that kind of task.

For each event, we get a simple csv file which contains five columns: the [National Olympic Committee](https://en.wikipedia.org/wiki/National_Olympic_Committee) (NOC), the number of gold, silver and bronze medals and the total.

To show the structure of the files, I reproduce the first few lines of the 2016 Summer Olympics:

| NOC | Gold | Silver | Bronze | Total |
| :---: | :---: | :---: | :---: | :---: |
| Algeria | 0 | 2 | 0 | 2 |
| Argentina | 3 | 1 | 0 | 4 |
| Armenia | 1 | 3 | 0 | 4 |
| Australia | 8 | 11 | 10 | 29 |
| Austria | 0 | 0 | 1 | 1 |

In the following analysis I mostly - if not only - used the total of medals.
As always a bit of data cleaning is necessary (mainly changing a few names to be consistent in all files) but not that much!

# Start to explore

The first plot we can look at is the number of medals awarded over time.

![medals_awarded.png](/plots/medals_awarded.png?raw=true)

There is a clear tendency to the rise of the number of medals awarded (more than 4x from 1900 to 2016 and x2 since 1956) but - at least graphically - it seems that the [International Olympic Committee](https://en.wikipedia.org/wiki/International_Olympic_Committee) tries to slow down that increase in the recent years.

The number of countries which won at least one medal follows a similar trend.

![countries_won_medals.png](/plots/countries_won_medals.png?raw=true)

There are sudden and visible drops in 1976 and 1980 that can be explained really easily: these two events suffered a boycott.
In 1976, [29 countries, mostly African, boycotted the Games](https://en.wikipedia.org/wiki/1976_Summer_Olympics#Non-participating_National_Olympic_Committees) (in response to the participation of New Zealand) and in 1980,
the event held in Moscow was [boycotted by more than 60 countries](https://en.wikipedia.org/wiki/1980_Summer_Olympics_boycott) following the example of the United States.
(By the way, the Olympic Games of 1984 - held in Los Angeles - were also [boycotted](https://en.wikipedia.org/wiki/1984_Summer_Olympics_boycott) by some countries.)

The growth rate of both the number of medals and the number of countries awarded is another way to depict the same story.

![growth_rate_countries_medals.png](/plots/growth_rate_countries_medals.png?raw=true)

# Highlight the trajectory of some countries

First let's start with all the countries on the same plot.

![ts_all.png](/plots/ts_all.png?raw=true)

Of course with all the countries represented at the same time it's somewhat difficult to discern precise patterns but it gives a good overall view over time.
But for now, let's focus on some specific countries and more precisely on some of the countries which have played a great role in the history of the Summer Olympics.

Since 1896 the United States have always been one of the major players. With that following plot it's especially obvious: they always have been in the top 2 in terms of total of medals won.

![ts_us.png](/plots/ts_us.png?raw=true)

The United States seem to improve *over time.* But is that right? Well, it depends. If we consider only the number of medals won as we did, yes, they do. But if we add to the equation the fact that the number of medals awarded over time also increased, it becomes far from obvious.

What we can do is to look at both the number of medals and the percentage of medals won. While the number of medals won increases, the percentage of medals won tends to decrease...

![us_number_per.png](/plots/us_number_per.png?raw=true)

From the middle of the 20th century to the 90's, the main antagonist of the US was Russia (or more exactly the USSR at that time).

![ts_russia.png](/plots/ts_russia.png?raw=true)

The fierce competition between these two countries is even more clear if we use only one graph to highlight both of them at the same time. The two boycotts of [1980](https://en.wikipedia.org/wiki/1980_Summer_Olympics_boycott) and [1984](https://en.wikipedia.org/wiki/1984_Summer_Olympics_boycott) are distinctly visible.

![ts_us_russia.png](/plots/ts_us_russia.png?raw=true)

Also notable is the emergence of China since the start of the 80's. From nowhere, China has become the second major player in less than 25 years. Even for the largest country in the world that's more than impressive!

![ts_china.png](/plots/ts_china.png?raw=true)

In fact, the rise of China matches more or less the rise of Asia which passed North America as the second most prolific continent (but still a long way from Europe). Africa and South America are also rising but at a much more reasonable pace.

![ts_continents.png](/plots/ts_continents.png?raw=true)

# Small multiple plots

To compare visually several events, an interesting tool popularized by [Edward Tufte](https://en.wikipedia.org/wiki/Edward_Tufte) consists of using a series of similar charts with the same scale. These kind of plots are often called a small multiple, trellis charts or lattice charts.

If we're interested in the evolution of the distribution of the number of medals won by country over time, a good representation could be multiple histograms (one for each event). This gives that kind of plot:

![multi_histo.png](/plots/multi_histo.png?raw=true)

This plot is truly informative. You can perfectly see the evolution over time: more and more countries are winning medals but only a few are able to win *a lot* of them. The main issue with histograms is the choice of the number of bins. For different number of bins, it's possible to have histograms with really different shapes.

Another possible way to visualize a distribution is to use a [violin plot](https://en.wikipedia.org/wiki/Violin_plot). Violin plots are less popular than histograms or boxplots despite the fact they are more informative than the later. Where a boxplot shows summary statistics (median, mean, IQR, etc.), a violin plot shows the *full* distribution of the variable which can be really useful when the data distribution is not unimodal.

Below is an example of small multiple violin plots.

![multi_violin.png](/plots/multi_violin.png?raw=true)

# London 2012 *vs* Rio 2016

We can now focus a bit more and compare only the last two events: London 2012 and Rio 2016.  
The usual histograms to start:

![hist_london_rio.png](/plots/hist_london_rio.png?raw=true)

There is not much to say except maybe the fact that the gap between the most prolific country - the US - and the rest of the nations was bigger in Rio than it was in London. With that small number of points it's also possible to use a simple but powerful type of plot: a [dotplot](https://en.wikipedia.org/wiki/Dot_plot_(statistics)).

![dotplot_london_rio.png](/plots/dotplot_london_rio.png?raw=true)

This graph highlights once again the bigger gap between the US and the rest of the contenders in Rio but it also shows relatively clearly the why. In one hand, the US *improved* significantly from 2012 to 2016 and, in another hand, their closest competitors followed the opposite path: they won *less* medals in Rio.

If we focus only on the countries which were in the top 10 in Rio, we can look precisely how their total of medals won changed from one event to the other.

As we already saw, the US improved quite a lot but they are not the only ones. France and Australia also did pretty well in Rio compared to London and the United Kingdom improved just a little bit but that's quite a notable performance for a country which was the previous host.

On the other side, China and Russia went backwards *a lot.*

![dotplot_london_rio_top_10.png](/plots/dotplot_london_rio_top_10.png?raw=true)

# A bumb chart

The final plot we'll look at is the evolution of the rank of the nations from Barcelona 1992 to Rio 2016. I put the emphasis on both the United Kingdom and Germany to show two really different paths but this plot as a lot more information in it!

![rank_germany_gb.png](/plots/rank_germany_gb.png?raw=true)
