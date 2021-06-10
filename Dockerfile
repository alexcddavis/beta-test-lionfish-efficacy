#First thing is to install the R version needed
FROM r-base:4.1.0
#Set your working directory
WORKDIR /main
RUN R -e "options(repos =\
  list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2021-06-09/')); \
  install.packages('readr'); install.packages ('ggplot2')"
#mran is an archive of all the R packages, look at the snapshot from this day
#install these packages 
CMD ["Rscript", "__main__.R"]
#run the main file in the command line,and tell it what kind of file it is

#all CAPS words are Docker commands
# e= echo -echo means take away the echo
