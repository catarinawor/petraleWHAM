# Vignette 1: Getting data into WHAM
library(here)
library(dplyr)

# Petrale Data
petrale<-readRDS(paste0(here(),"/data/Petrale_Dat.rds"))
names(petrale)

petrale$catch
names(petrale$lifehistory)
petrale$comps
petrale$lifehistory$M

lib.loc <- NULL
#lib.loc <- "c:/work/wham/old_packages/lab"
library("wham", lib.loc = lib.loc)


#########################################################
#not using asap3 dat file - combined sexes
##############################################
#read basic data files and make inputs
n_ages<-22
n_regions <- 1
n_stocks <- 1
n_years<-nrow(petrale$catch)
#create the maturity at age stock - importing numbers from assessment because
#of the way they were computed'

mat_comb<-(petrale$lifehistory$Mmaa+petrale$lifehistory$Fmaa)/2
mat <- array(0, dim = c(n_stocks, n_years, n_ages))
for(i in 1:n_years) mat[1,i,] <- mat_comb

dim(mat) #(n_stocks x n_years x n_ages)


#there are 7 WAA matrices
comb_waa<-(petrale$lifehistory$Mwaa +petrale$lifehistory$Fwaa)/2

#temp <- lapply(1:7, \(i) as.matrix(read.csv(here("data", paste0("waa_",i,".csv")))))
waa <- array(0, dim = c(1,n_years,n_ages))
for(i in 1:n_years) waa[1,i,] <- comb_waa
dim(waa)

#the 7 here refers to waa to the 5 indices, the catch, and the population 
#waa <- array(0, dim = c(7,dim(temp[[1]])))
#for(i in 1:(dim(waa)[1])) waa[i,,] <- temp[[i]]

waa_pointer_ssb <- 1

#1 column for 1 stock
#the fraction of the year that happened before the stock ocurred. 
#fracyr_ssb <- as.matrix(read.csv(here("data", "fracyr_ssb.csv")))
fracyr_ssb <- matrix(0, ncol=1, nrow=n_years)

#Make MAA array (n_stocks x n_regions x n_years x n_ages)
MAA <- array(NA, dim = c(n_stocks, n_regions, n_years, n_ages))
for(i in 1:n_stocks) for(j in 1:n_regions) MAA[i,j,,] <- petrale$lifehistory$M
  
#as.matrix(read.csv(file = here("data", paste0("MAA_stock_", i, "_region_",j,".csv"))))


##############################################
#read catch data and make inputs
catch <- as.matrix(petrale$catch$Catch, ncol=1)
#catch <- as.matrix(read.csv(here("data", "catch.csv")))
any(!catch>0) #cannot be any years without catch

#should be n_years x n_fleets
dim(catch) 

#just one column: 1 fleet
n_fleets <- NCOL(catch)

catch_cv <- as.matrix(petrale$catch$SE, ncol=1)
  #as.matrix(read.csv(here("data", "catch_cv.csv")))
any(!catch_cv>0) #cannot be any years with cv missing

#should be n_years x n_fleets
dim(catch_cv)

#proportions at age matrix for each fleet is n_years x n_ages
catch_paa_raw<-petrale$comps[petrale$comps$Fleet=="Fishery",]
  
  
  
catch_paa_crop<-catch_paa_raw[,grepl("^F[0-9]+$", names(petrale$comps))]+
                 catch_paa_raw[,grepl("^M[0-9]+$", names(petrale$comps))]
catch_paa_crop <-cbind(catch_paa_raw[,"Year"],(catch_paa_crop))
#expand matrix so that it has all years
catch_paa_join<-left_join(data.frame(Year=1938:2023),catch_paa_crop)

all_years<-data.frame(Year=1938:2023) 
df_filled <-merge(all_years, catch_paa_crop, by = "Year", all.x = TRUE)

  #as.matrix(read.csv(here("data", "catch_paa_fleet_1.csv")))
dim(catch_paa) 

sort(petrale$comps$"Year")


#array used by WHAM: (n_fleets x n_years x n_ages)
catch_paa <- array(catch_paa, dim = c(1,dim(catch_paa)))
catch_paa[which(catch_paa<0)] <- NA




catch_Neff <- as.matrix(petrale$comps$`Sample Size`,ncol=1)
  #as.matrix(read.csv(here("data", "catch_Neff.csv")))

#should be n_years x n_fleets
dim(catch_Neff)

#0/1 matrx (n_years x n_fleets) whether to use proportions at age each year (1), i.e., to include in the likelihoof or not. 
use_catch_paa <- matrix(0L, n_years, n_fleets)
for(f in 1:n_fleets) for(y in 1:n_years) use_catch_paa[y,f] <- sum(!any(is.na(catch_paa[f,y,])))

#Neff must be greater than 0)
use_catch_paa[which(!catch_Neff>0)] <- 0

#same selectivity model for the fleet for all years
selblock_pointer_fleets <- matrix(1, n_years, n_fleets)

#first WAA matrix
waa_pointer_fleets <- 1

##############################################

#read index data and make inputs

indices <- as.matrix(read.csv(here("data", "indices.csv")))
index_cv <- as.matrix(read.csv(here("data", "index_cv.csv")))
#indices must be greater than 0
indices[which(!indices>0)] <- NA
indices[which(!index_cv>0)] <- NA

#should be n_years x n_indices
dim(indices)

#5 indices
n_indices <- NCOL(indices)

#How is the index measured? 2 = numbers, 1 = biomass
units_indices <- c(2,2,2,1,1)
#How are the proportions at age measured? 2 = numbers, 1 = biomass
units_index_paa <- c(2,2,2,1,1)

index_Neff <- as.matrix(read.csv(here("data", "index_Neff.csv")))

#should be n_years x n_indices
dim(index_Neff)

#different selectivity model for each index
selblock_pointer_indices <- t(matrix(1+1:n_indices, n_indices, n_years))

#proportions at age matrix for each index is n_years x n_ages
temp <- lapply(1:n_indices, \(i) as.matrix(read.csv(here("data", paste0("index_paa_",i,".csv")))))

dim(temp)
#array used by WHAM: (n_indices x n_years x n_ages)
index_paa <- array(0, dim = c(n_indices,dim(temp[[1]])))
for(i in 1:n_indices) index_paa[i,,] <- temp[[i]]

dim(index_paa)

#0/1 matrix (n_years x n_fleets) whether to use indices and proportions at age each year)
use_index_paa <- use_indices <- matrix(0L, n_years, n_indices)

use_indices[] <- as.integer(!is.na(indices))
for(i in 1:n_indices) for(y in 1:n_years) use_index_paa[y,i] <- sum(!any(is.na(index_paa[i,y,])))
#Neff must be greater than 0)
use_index_paa[which(!index_Neff>0)] <- 0


index_fracyr <- as.matrix(read.csv(here("data", "index_fracyr.csv")))

#should be n_years x n_indices
dim(index_fracyr)

waa_pointer_indices <- 1  + 1:n_indices


##############################################
#create list arguments to prepare_wham_input and/or set_* functions
basic_info <- list(
  n_stocks = as.integer(n_stocks),
  ages = 1:n_ages,
  n_seasons = 1L,
  n_fleets = n_fleets,
  fracyr_SSB = fracyr_ssb,
  maturity = mat,
  years = 1973:2011, #length must be n_years
  waa = waa,
  waa_pointer_ssb = waa_pointer_ssb
)

catch_info <- list(
  n_fleets = n_fleets,
  agg_catch = catch,
  agg_catch_cv = catch_cv,
  catch_paa = catch_paa,
  use_catch_paa = use_catch_paa,
  catch_Neff = catch_Neff,
  selblock_pointer_fleets = selblock_pointer_fleets,
  waa_pointer_fleets = waa_pointer_fleets
)

index_info <- list(
  n_indices = n_indices,
  agg_indices = indices,
  units_indices = units_indices,
  units_index_paa = units_index_paa,
  agg_index_cv = index_cv,
  fracyr_indices = index_fracyr,
  use_indices = use_indices,
  use_index_paa = use_index_paa,
  index_paa = index_paa,
  index_Neff = index_Neff,
  selblock_pointer_indices = selblock_pointer_indices,
  waa_pointer_indices = waa_pointer_indices
)

#selectivity modeling
selectivity <- list(model = rep("logistic",6), n_selblocks = 6,
  fix_pars = list(NULL,NULL,NULL,NULL, 1:2, 1:2),
  initial_pars = list(c(2,0.2),c(2,0.2),c(2,0.2),c(2,0.2),c(1.5,0.1),c(1.5,0.1)))

#M modeling: fixed MAA
M_in <- list(initial_MAA = MAA)

##############################################

#make input all at once
input_all <- prepare_wham_input(basic_info = basic_info, selectivity = selectivity, catch_info = catch_info, index_info = index_info, M = M_in)

#piece by piece
input_seq <- prepare_wham_input(basic_info = basic_info)
input_seq <- set_M(input_seq, M = M_in)
input_seq <- set_catch(input_seq, catch_info = catch_info)
input_seq <- set_F(input_seq)
input_seq <- set_indices(input_seq, index_info = index_info)
input_seq <- set_q(input_seq)
input_seq <- set_selectivity(input_seq, selectivity = selectivity)

#from asap input
input_asap <- prepare_wham_input(asap3) 

names(input_all)

#compare 
nofit_all <- fit_wham(input_all, do.fit = FALSE)
nofit_seq <- fit_wham(input_seq, do.fit = FALSE)
nofit_asap <- fit_wham(input_asap, do.fit = FALSE)

nofit_seq$fn() - nofit_all$fn() #equal
length(nofit_seq$par) - length(nofit_all$par) #equal

nofit_all$fn() - nofit_asap$fn() #different

nofit_asap$par-nofit_all$par #different

nofit_all$fn() - nofit_asap$fn(nofit_all$par) #equal

fit_seq <- fit_wham(input_seq, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE)
fit_all <- fit_wham(input_all, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE)
fit_asap <- fit_wham(input_asap, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE)
fit_seq$opt$obj - fit_all$opt$obj # equal
fit_asap$opt$obj - fit_all$opt$obj # equal

res_dir <- file.path(getwd(),"temp")
dir.create(res_dir)

saveRDS(fit_asap, file.path(res_dir,"fit_asap.RDS"))
saveRDS(fit_all, file.path(res_dir,"fit_all.RDS"))
saveRDS(fit_seq, file.path(res_dir,"fit_seq.RDS"))

fit_asap <- do_reference_points(fit_asap, do.sdrep = TRUE)
fit_asap$peels <- retro(fit_asap)
fit_asap <- make_osa_residuals(fit_asap)

tmp.dir <- tempdir(check=TRUE)
plot_wham_output(fit_asap, dir.main = res_dir)

fit_RDS <- file.path(res_dir,"fit.RDS")
saveRDS(fit_asap, fit_RDS)

x <- jitter_wham(fit_RDS = fit_RDS, n_jitter = 10, res_dir = res_dir, do_parallel = FALSE)
sapply(x[[1]], function(y) y$obj) #nlls

