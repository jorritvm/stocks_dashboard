# Stocks_Dashboard
Dashboard to track stock OHLC and portfolio

## Challenges
This is a hobby project to get familiar with
* R shiny & shinydashboard + proper use of reactivity
* RSqlite interface (& DBI)
* dplyr syntax & it's application on sqlite (dbplyr)
* quantmod package (Yahoo OHLC API)
* querying rapidapi using httr & rjson
* plotly for interactive charts

## Todo
* Show portfolio performance using modified Dietz method

## Features & screenshots
The tool can list currencies exchange rates:

<img src="doc/screenshots/forex.png" width="750"> 

The tool also contains profile & OHLC data for desired stocks:

<img src="doc/screenshots/profile.png" width="750"> 

The tool can plot stock OHLC data in a candlestick chart.

<img src="doc/screenshots/candlestick.png" width="750"> 

The tool can benchmark one stocks performance to another over various timeframes.

<img src="doc/screenshots/benchmark.png" width="750"> 

The tool can show your market timing visually.

<img src="doc/screenshots/timings.png" width="750"> 