FROM rocker/verse:4.1.2

# install a newer-ish version of renv, but the specific version we want will be restored from the renv lockfile
RUN R --quiet -e "install.packages('renv', repos = 'https://cran.rstudio.com')"
ENV RENV_VERSION 0.15.2
RUN R --quiet -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /app

RUN apt-get update \
  && apt-get install -yqq --no-install-recommends \
  libgdal-dev \
  libgeos-dev \
  libudunits2-dev \
  libproj-dev \
  libv8-dev \
  && apt-get clean

COPY renv.lock .
#RUN R --quiet -e "renv::restore(repos = c(CRAN = 'https://packagemanager.rstudio.com/all/__linux__/focal/latest'))"
RUN R --quiet -e "renv::restore(repos = c(CRAN = 'https://cran.rstudio.com'))"

COPY tract_to_neighborhood.rds .
COPY ham_neighborhoods_dep_index_shp.rds .
COPY weekly_data_report.R .

WORKDIR /tmp

ENTRYPOINT ["/app/weekly_data_report.R"]
