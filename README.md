# Stocks_Dashboard
Dashboard to track stock OHLC, FX, and portfolio

## Learnings
This is a hobby project to get familiar with
* R shiny & shinydashboard + proper use of reactivity
* RSqlite interface (& DBI)
* dplyr syntax & it's application on sqlite (dbplyr)
* quantmod package (Yahoo OHLC API)
* querying rapidapi using httr & rjson
* plotly for interactive charts
* using env files for secrets

## Todo
* Show portfolio performance using modified Dietz method

## How to install the dashboard
* Clone the repo
* Restore the RENV environment (currently built using R4.2.2)
* If you need a vanilla data db file, run ```scripts/setup_db.R```
* If you need a credentials db file, setup``` scripts/credentials.env``` and run ```scripts/setup_credentials.R```
* Setup host, port and yahoo api key in ```src/config.env```
* Run app.R

More information is to be found in `doc/stock_dashboard_documentation.pptx`

## How to run using docker
* Modify Dockerfile ENV variables (host, port and yahoo api key)
* Build the image
  * ```
    host> docker build -t stocks_dashboard .
    ```
* Run the container in interactive mode to set up the credentials DB
  * ```
    host> docker container run -it -e shiny_user=your_user -e shiny_password=your_password -e cred_db_password=your_cred_db_password --name stocks_dashboard stocks_dashboard /bin/bash
    cont> Rscript ../scripts/setup_credentials.R
    cont> exit
    ```
* Run the container normally 


## Features & screenshots
The tool can list currencies exchange rates:

<img src="doc/screenshots/forex.png?" width="750"> 

The tool also contains profile & OHLC data for desired stocks:

<img src="doc/screenshots/profile.png?" width="750"> 

The tool can plot stock OHLC data in a candlestick chart.

<img src="doc/screenshots/candlestick.png?" width="750"> 

The tool can benchmark one stocks performance to another over various timeframes.

<img src="doc/screenshots/benchmark.png?" width="750"> 

The tool can show your market timing visually.

<img src="doc/screenshots/timings.png?" width="750"> 
  
  
