#global R to contain global helper functions
#
library(shiny)
library(tidyverse)
#library(mosaic)
#library(rstatix)
library(ggplot2)
library(diann)
library(fs)
library(shinyFiles)
library(listenv)
library(DT)
#read the diann file with the diann r package
rep_df=data.frame()
readDiannReport<-function(file_path)
{
    rep_df <- diann_load(file_path)
    #cat(class(rep_df),"\n")
    #cat("dimension:", dim(rep_df),"\n")
    return (rep_df)
}

#readDiannReport<-function(file_path)
#{
#    rep_df <- diann_load(file_path)
#    #cat(class(rep_df),"\n")
#    #cat("dimension:", dim(rep_df),"\n")
#    return (rep_df)
#}
#'@param ds dirchoose, input$directory_select
checkFileExist<-function(ds, vols, filename="report.tsv")
{
if(!is.integer(ds))
        {
            
            if(file.exists(file.path(parseDirPath(vols, ds),filename)))
            {
                return (TRUE)
            }
            else return(FALSE)
        }
else return(FALSE)
}

### function to extract the ptms strings based on the grepexpr indexes
#'@param mseq, one modified sequences, there might be multiple ptms on each sequence 
#'@ptm_indexes, this is vector of indexes with attribute match.length 
#'
#'@return a string vector (in case there are many ptms on this single sequence.  "", if no ptm on this sequence
#'           
#' 
extract_PTMs_each<-function( mseq)
{
    ptm_indexes<-gregexpr(text=mseq, pattern="\\([^()]+\\)")
    x<-c()
    if(ptm_indexes[[1]][1]<0)  #if the first element is -1, then there is no ptm on this sequence
    {
        x<-""
        #att(x, whitch="match.index")=ptms.index[[1]]
        return(x)
    }
    for(i in 1:length(ptm_indexes[[1]])){
            x<-c(x,substr(mseq, ptm_indexes[[1]][i], ptm_indexes[[1]][i]+attr(ptm_indexes[[1]], which="match.length")[i]-1))
    }
    return (x)
}

####Function to summarize 
#'@


##################################################
###         -for testing, the below section.
##################################################
mtcars.data<-mtcars; #show how to use to read data etc. mtcars.data used by server.R
a=15
freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )

  ggplot(df, aes(x, colour = g)) +
    geom_freqpoly(binwidth = binwidth, size = 1) +
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1, x2) {
  test <- t.test(x1, x2)
  
  # use sprintf() to format t.test() results compactly
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}
