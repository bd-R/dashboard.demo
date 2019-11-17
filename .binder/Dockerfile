FROM rocker/binder:3.6.0
LABEL maintainer='Tomer Gueta'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}



RUN wget https://github.com/tom-gu/dashboard.demo/raw/master/DESCRIPTION && R -e "options(repos = list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2019-11-16/')); devtools::install_deps(); devtools::install_github('tom-gu/dashboard.demo') "

RUN rm DESCRIPTION.1; exit 0
