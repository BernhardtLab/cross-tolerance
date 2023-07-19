# working with thermal performance curves
#ijeoma nwafor
#july 19th, 2023

#installing packages needed to load in thermal performance packages
install.packages("usethis") #needed for devtools
install.packages("devtools") # needed for install_github/version
install.packages("nls.multstart") # for nonlinear squares regression
install.packages("broom") #cleans up ^ and turns into tidy tibbles
install.packages("tidyverse")
install.packages("remotes")

library(usethis)
library(devtools)
library(nls.multstart)
library(broom)
library(tidyverse)
library(remotes)

##installing rTPC packages straight from github 
remotes::install_github("padpadpadpad/rTPC")
library(rTPC)

##Now lets load in ThermPerf
remotes::install_github("mdjbru-R-packages/thermPerf")
library(thermPerf)
