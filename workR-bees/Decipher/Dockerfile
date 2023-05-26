## Download the latest version of the rocker container
FROM rocker/tidyverse:latest

## Copy the files inside the container
COPY /install_packages.R /install_packages.R
COPY /decipher.R /decipher.R

## Install the packages
RUN Rscript /install_packages.R

## Execute the decipher script
CMD ["Rscript", "decipher.R"]