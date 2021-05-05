FROM rocker/verse:4.0.3

# install a newer-ish version of renv, but the specific version we want will be restored from the renv lockfile
RUN R --quiet -e "install.packages('renv', repos = 'https://cran.rstudio.com')"

WORKDIR /app

RUN apt-get update \
  && apt-get install -yqq --no-install-recommends \
  libgdal-dev \
  libgeos-dev \
  libudunits2-dev \
  libproj-dev \
  libssl-dev \
  && apt-get clean

COPY renv.lock .
RUN R --quiet -e "renv::restore(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'))"
#RUN R --quiet -e "renv::restore(repos = c(CRAN = 'https://cran.rstudio.com'))"

COPY tract_to_neighborhood.rds .
COPY ham_neighborhoods_dep_index_shp.rds .
COPY aggregate_data_report.r .

WORKDIR /tmp

ENTRYPOINT ["/app/aggregate_data_report.r"]
