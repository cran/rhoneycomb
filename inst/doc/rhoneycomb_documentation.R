## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(error = Sys.getenv("IN_PKGDOWN") != "true" || (getRversion() < "3.5"))

## ----setup , echo=FALSE-------------------------------------------------------
library(rhoneycomb)

## ----setup, eval=FALSE,warning=FALSE , message=FALSE--------------------------
#  install.packages("rhoneycomb")
#  library(rhoneycomb)

## -----------------------------------------------------------------------------
generate(1:60)

## -----------------------------------------------------------------------------
main_data<-HSD(7,2,10,10,1)

head(main_data,25) #Use the head function to get the top 25 rows.

## -----------------------------------------------------------------------------

main_data$Data<-wheat_data$main_spike_weight
result<-analysis(main_data,"Data",6)

head(result[[1]],10) #Use the head function to get the top 10 rows.
result[[2]] 

## -----------------------------------------------------------------------------
result<-analysis(main_data,"Data",blocks=TRUE,row_element=5,plant_element=5)
head(result[[1]],10) #Use the head function to get the top 10 rows.
result[[2]] 

## -----------------------------------------------------------------------------
main_data<-HSD0(10,10,1)
main_data$Data<-wheat_data$main_spike_weight
head(main_data,10) #Use the head function to get the top 10 rows.

## -----------------------------------------------------------------------------
main_data<-HSD01(7,10,10,1)
main_data$Data<-wheat_data$main_spike_weight

head(main_data,10) #Use the head function to get the top 10 rows.

## -----------------------------------------------------------------------------
result<-analysis(main_data,"Data")
head(result[[1]],10) #Use the head function to get the top 10 rows.

