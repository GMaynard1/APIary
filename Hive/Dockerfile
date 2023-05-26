## Download the latest version of the rocker container
FROM rocker/tidyverse:latest

## Copy the files inside the container
COPY /install_packages.R /install_packages.R

## Install the packages
RUN Rscript /install_packages.R

## Mount the workR bee directory
ADD workR-bees/ /workR-bees/