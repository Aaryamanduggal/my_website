---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: 
keywords: ""
slug: ipsum
title: Yield Curve Inversion
---

Every so often, we hear warnings from commentators on the "inverted yield curve" and its predictive power with respect to recessions. An explainer what a [inverted yield curve is can be found here](https://www.reuters.com/article/us-usa-economy-yieldcurve-explainer/explainer-what-is-an-inverted-yield-curve-idUSKBN1O50GA). If you'd rather listen to something, here is a great podcast from [NPR on yield curve indicators](https://www.podbean.com/media/share/dir-4zgj9-6aefd11)

In addition, many articles and commentators think that, e.g., [*Yield curve inversion is viewed as a harbinger of recession*](https://www.bloomberg.com/news/articles/2019-08-14/u-k-yield-curve-inverts-for-first-time-since-financial-crisis). One can always doubt whether inversions are truly a harbinger of recessions, and [use the attached parable on yield curve inversions](https://twitter.com/5_min_macro/status/1161627360946511873).


```{r yield_curve_parable.jpg, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve_parable.jpg"), error = FALSE)
```


In our case we will look at US data and use the [FRED database](https://fred.stlouisfed.org/) to download historical yield curve rates, and plot the yield curves since 1999 to see when the yield curves flatten. If you want to know more, a very nice article that explains the [yield curve is and its inversion can be found here](https://fredblog.stlouisfed.org/2018/10/the-data-behind-the-fear-of-yield-curve-inversions/). At the end of this challenge you should produce this chart

```{r yield_curve_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve_challenge.png"), error = FALSE)
```


First, we will load the yield curve data file that contains data on the yield curve since 1960-01-01

```{r download_historical_yield_curve, warning=FALSE}

yield_curve <- read_csv(here::here("data", "yield_curve.csv"))

glimpse(yield_curve)
```

Our dataframe `yield_curve` has five columns (variables):

- `date`: already a date object
- `series_id`: the FRED database ticker symbol
- `value`: the actual yield on that date
- `maturity`: a short hand for the maturity of the bond
- `duration`: the duration, written out in all its glory!


## Plotting the yield curve

This may seem long but it should be easy to produce the following three plots

### Yields on US rates by duration since 1960

```{r yield_curve_1, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve1.png"), error = FALSE)
```

```{r yield_curve_1,fig.width=20, fig.height=10, fig.fullwidth=TRUE}
yield_curve %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=value,color=duration)) +
  facet_wrap(~duration,scales="free",nr=5)+
  theme_bw()+
  theme(legend.position="none")+
  labs(title="Yields on U.S Treasury rates since 1960",x="",y="%",caption="Source: St. Louis Federal Reserve Economic Database (FRED)")
```
### Monthly yields on US rates by duration since 1999 on a year-by-year basis


```{r yield_curve_2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve2.png"), error = FALSE)
```

```{r yield_curve_2,fig.width=20, fig.height=10, fig.fullwidth=TRUE}
yield_curve_dates <- yield_curve %>% 
  mutate (year = year(date),
          month = lubridate::month(date, label = TRUE),
          week = isoweek(date)) %>% 
  filter(year >1998) 
  

yield_curve_dates %>%
  #Force ggplot to use levels as factor
  ggplot(aes(x=factor(maturity,levels=unique(yield_curve_dates$maturity)),
             y=value,
             #Group by month to make R understand that we are connecting values of each duration by month
             group=month,color=as.factor(year)))+
  #Add the geometric line
  geom_line()+
  facet_wrap(~year,nr=6)+
  theme_bw()+
  labs(title="US Yield Curve",
       x="Maturity",y="Yield(%)",caption="Source: St. Louis Federal Reserve Economic Database (FRED)")+
  theme(legend.position="none")

```


### 3-month and 10-year yields since 1999

```{r yield_curve_3, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve3.png"), error = FALSE)
```


```{r yield_curve_3,fig.width=20, fig.height=10, fig.fullwidth=TRUE}
yield_curve_3_10 <- yield_curve_dates %>% 
  filter(maturity %in% c("3m","10y")) 

yield_curve_3_10 %>% 
  #Color based on factor, rather than alphabetically
  ggplot(aes(x=date,y=value,color=factor(duration,levels=unique(yield_curve_3_10$duration))))+
  #Add the lines
  geom_line()+
  #Theme and labels
  theme_bw()+
    labs(title="Yields on 3-month and 10-year US Treasury rates since 1999",
       x="",y="%",caption="Source: St. Louis Federal Reserve Economic Database (FRED)")+
  theme(legend.title=element_blank())
```

According to [Wikipedia's list of recession in the United States](https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States), since 1999 there have been two recession in the US: between Mar 2001–Nov 2001 and between Dec 2007–June 2009. Does the yield curve seem to flatten before these recessions? Can a yield curve flattening really mean a recession is coming in the US? Since 1999, when did short-term (3 months) yield more than longer term (10 years) debt?



Besides calculating the spread (10year - 3months), there are a few things we need to do to produce our final plot

1. Setup data for US recessions 
1. Superimpose recessions as the grey areas in our plot
1. Plot the spread between 30 years and 3 months as a blue/red ribbon, based on whether the spread is positive (blue) or negative(red)


- For the first, the code below creates a dataframe with all US recessions since 1946

```{r setup_US-recessions, warning=FALSE}

# get US recession dates after 1946 from Wikipedia 
# https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States

recessions <- tibble(
  from = c("1948-11-01", "1953-07-01", "1957-08-01", "1960-04-01", "1969-12-01", "1973-11-01", "1980-01-01","1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01","2020-02-01"),  
  to = c("1949-10-01", "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-30") 
  )  %>% 
  mutate(From = ymd(from), 
         To=ymd(to),
         duration_days = To-From)


glimpse(recessions)
```

```{r yield_curve_3,fig.width=20, fig.height=10, fig.fullwidth=TRUE}
library(scales)
library(ggtext)

yield_curve_3<- yield_curve %>% 
  #Get the 3months data
  filter(maturity %in% c("3m")) %>% 
  #Convert the dataframe to wider form
  pivot_wider(names_from="maturity",values_from="value") %>% 
  rename(three_month="3m")

yield_curve_10 <- yield_curve %>% 
  #Get the 10y data
  filter(maturity %in% c("10y")) %>% 
  #Convert the dataframe to wider form
  pivot_wider(names_from="maturity",values_from="value") %>% 
  rename(ten_year="10y")

#Merge the two datasets and calculate the spread
tidy_yield <- yield_curve_10_3 <- left_join(yield_curve_3,yield_curve_10,by="date") %>% 
  mutate(spread=ten_year-three_month) %>% 
  rename(dates2=date)

tidy_yield %>% 
  #Plot dates
  ggplot(aes(x=dates2)) +
  #Plot the recession lines
  geom_rect(data=recessions,aes(xmin=From,xmax=To,ymin=-Inf,ymax=Inf),fill='darkgray',inherit.aes=FALSE,alpha=0.35)+
  
  #Ensure that x axis is showing years in the increments of 2
  scale_x_date(date_breaks="2 years",labels=date_format("%Y"),limits=as.Date(c('1959-01-01','2021-08-01')))+
  #Add the spread
  geom_line(aes(y=spread),size=0.3)+
  #Color based on whether spread is positive or negative
  geom_ribbon(aes(ymin=0,ymax=pmax(spread,0)),fill="dodgerblue3",alpha=0.3)+
  geom_ribbon(aes(ymin=pmin(0,spread),ymax=0),fill="red",alpha=0.3)+
  #Add the rugs and color based on the sign of spread
  geom_rug(data=subset(tidy_yield,spread>0),color="dodgerblue3",sides="b",alpha=0.3)+
  geom_rug(data=subset(tidy_yield,spread<=0),color="red",sides="b",alpha=0.3)+
  geom_hline(yintercept=0,color="black")+
  #Theme and title
  theme_bw()+
   labs(
    #Use ** for bold
    title = "**Yield Curve Inversion: 10-year minus 3-month U.S Treasury rates**",
    #* for italics and <br> to force writing in new line
    subtitle = "*Difference in % points, monthly averages <br> Shaded areas correspond to recessions*",
    x = "", y = "Difference (10 year-3 month) yield in %",caption="Source: St. Louis Federal Reserve Economic Database (FRED)")+
   theme(
    plot.title = element_markdown(lineheight = 1.1),
    plot.subtitle=element_markdown(lineheight=1.1),
    legend.text = element_markdown(size = 11))

```



