# get a base image
FROM rocker/r-ver:4.2.2

LABEL maintainer="JVM <jorrit.vm@gmail.com>"

# update system dependencies required for shiny
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libsodium-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# create a non root user to run the app
RUN addgroup --system app \
    && adduser --system --ingroup app app

# make sure RENV is installed
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

# set the workdir
WORKDIR /app/src

# restore the RENV as the root user still, but make sure the cache does not end up in the root user home folder
COPY src/renv.lock .

RUN mkdir -p renv/cache
ENV RENV_PATHS_ROOT=./renv/cache

COPY ./src/.Rprofile .Rprofile
COPY ./src/renv/activate.R renv/activate.R

RUN R -e "renv::restore()"

# change the ownership of the renv - doing it now speeds up image rebuilds
RUN chown app:app -R /app/src/renv
RUN chown app:app /app/src/renv.lock
RUN chown app:app /app/src/.Rprofile

# copy the source data 
WORKDIR /app
RUN mkdir -p db  # this will be for volume mounting 
RUN mkdir -p auth # this will be for volume mounting 

WORKDIR /app/src
COPY ./src/app.R .
COPY ./src/www www
COPY ./src/R R

# change the ownership of the source
RUN chown app:app -R /app/auth
RUN chown app:app -R /app/db
RUN chown app:app -R /app/src/R
RUN chown app:app -R /app/src/www
RUN chown app:app /app/src/app.R
RUN chown app:app /app/src
RUN chown app:app /app/db

# change the user executing the CMD
USER app

# instead of using config.env i want to try and set the env variables here:
ENV r_stock_dashboard_host=0.0.0.0
ENV r_stock_dashboard_port=9999
ENV r_stock_dashboard_api_key=c9a70401a7mshb996d7e6cfc299fp17e46fjsn149c0bec651e

EXPOSE 9999
CMD ["R", "-e", "shiny::runApp()"]