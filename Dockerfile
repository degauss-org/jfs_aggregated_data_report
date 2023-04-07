FROM rocker/verse:4.1.2

WORKDIR /app

RUN apt-get update \
  && apt-get install -yqq --no-install-recommends \
  libgdal-dev \
  libgeos-dev \
  libudunits2-dev \
  libproj-dev \
  libv8-dev \
  && apt-get clean

RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('argparser')"

COPY tract_to_neighborhood.rds .
COPY ham_neighborhoods_dep_index_shp.rds .
COPY monthly_data_report.R .
COPY entrypoint.R .

WORKDIR /tmp

ENTRYPOINT ["/app/entrypoint.R"]

