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

## How to run your own development version
* Clone the repo
* Restore the RENV environment (currently built using R4.2.2)
* If you need a vanilla data db file, run ```scripts/setup_db.R```
* If you need a credentials db file, setup``` scripts/credentials.env``` and run ```scripts/setup_credentials.R```
* Setup host, port and yahoo api key in ```src/config.env```
* chdir into ```src/```
* Run ```app.R```
* Navigate to your app on http://localhost:9999

More information is to be found in `doc/stock_dashboard_documentation.pptx`

## How to run using docker
* This assumes your database and credential store have already been created.
* Modify Dockerfile ENV variables (host, port and yahoo api key)
* Build the image
  * ```
    docker build -t stocks_dashboard .
    ```
* Run the container, bind the desired port and mount the correct volumes
  * ```
    docker container run -d -p 12345:9999 -e cred_db_password=my_cred_db_password -v %cd%\auth:/app/auth -v %cd%\db:/app/db --name stocks_dashboard stocks_dashboard
    ```
* Navigate to your app on http://localhost:12345

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
  
  
