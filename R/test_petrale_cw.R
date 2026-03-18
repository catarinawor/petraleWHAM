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

petrale$indices

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
fracyr_ssb <- as.matrix(rep(0,n_years))
dim(fracyr_ssb)


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

dim(as.matrix(catch_paa_join[,-1]))

catch_paa <- array(as.matrix(catch_paa_join[,-1]), dim = c(1,dim(catch_paa_join[,-1])))

  #as.matrix(read.csv(here("data", "catch_paa_fleet_1.csv")))
dim(catch_paa) 


#array used by WHAM: (n_fleets x n_years x n_ages)
#catch_paa <- array(catch_paa, dim = c(1,dim(catch_paa)))
catch_paa[which(catch_paa<0)] <- NA

catch_Neff_raw <- catch_paa_raw[,c("Year","Sample Size")]

catch_Neff_join<-left_join(data.frame(Year=1938:2023),catch_Neff_raw)
 
catch_Neff<-as.matrix(catch_Neff_join$"Sample Size",ncol=1)
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
ind2<-petrale$indices[petrale$indices$Fleet==2,c("Year","CPUE")]|>rename(ind2=CPUE)
ind3<-petrale$indices[petrale$indices$Fleet==3,c("Year","CPUE")]|>rename(ind3=CPUE)
ind4<-petrale$indices[petrale$indices$Fleet==4,c("Year","CPUE")]|>rename(ind4=CPUE)

indices_long<-left_join(data.frame(Year=1938:2023),ind2)|>
  left_join(ind3)|>
  left_join(ind4)

cvind2<-petrale$indices[petrale$indices$Fleet==2,c("Year","SE")]|>rename(SEind2=SE)
cvind3<-petrale$indices[petrale$indices$Fleet==3,c("Year","SE")]|>rename(SEind3=SE)
cvind4<-petrale$indices[petrale$indices$Fleet==4,c("Year","SE")]|>rename(SEind4=SE)

cvindices_long<-left_join(data.frame(Year=1938:2023),cvind2)|>
  left_join(cvind3)|>
  left_join(cvind4)

indices <-as.matrix(indices_long[,-1])
index_cv <-as.matrix(cvindices_long[,-1])

#indices <- as.matrix(read.csv(here("data", "indices.csv")))
#index_cv <- as.matrix(read.csv(here("data", "index_cv.csv")))
#indices must be greater than 0
indices[which(!indices>0)] <- NA
indices[which(!index_cv>0)] <- NA

#should be n_years x n_indices
dim(indices)

#5 indices
n_indices <- NCOL(indices)

#How is the index measured? 2 = numbers, 1 = biomass
units_indices <- c(1,1,1)
#How are the proportions at age measured? 2 = numbers, 1 = biomass
units_index_paa <- c(2,2,2)


index_Neff_raw <- petrale$comps[petrale$comps$Fleet!="Fishery",]
index_Neff_raw<-left_join(expand.grid(Year=1938:2023,Fleet=c("QCS_Syn","HS_Syn","WCVI_Syn")),index_Neff_raw)

catch_Neff_raw2 <- index_Neff_raw[index_Neff_raw$Fleet=="QCS_Syn",c("Year","Sample Size")]
catch_Neff_raw3 <- index_Neff_raw[index_Neff_raw$Fleet=="HS_Syn",c("Year","Sample Size")]
catch_Neff_raw4 <- index_Neff_raw[index_Neff_raw$Fleet=="WCVI_Syn",c("Year","Sample Size")]

index_Neff <-as.matrix(cbind(catch_Neff_raw2$"Sample Size",
                             catch_Neff_raw3$"Sample Size",
                             catch_Neff_raw4$"Sample Size"))



#should be n_years x n_indices
dim(index_Neff)

#different selectivity model for each index
selblock_pointer_indices <- t(matrix(1+1:n_indices, n_indices, n_years))

#proportions at age matrix for each index is n_years x n_ages


index_paa_raw<-petrale$comps[petrale$comps$Fleet!="Fishery",]



index_paa_crop<-index_paa_raw[,grepl("^F[0-9]+$", names(petrale$comps))]+
  index_paa_raw[,grepl("^M[0-9]+$", names(petrale$comps))]



index_paa_crop <-cbind(index_paa_raw[,c("Year","Fleet")],index_paa_crop)

index_paa_join<-left_join(expand.grid(Year=1938:2023,Fleet=c("QCS_Syn","HS_Syn","WCVI_Syn")),index_paa_crop)
dim(index_paa_join)
Fleet_names=c("QCS_Syn","HS_Syn","WCVI_Syn")
index_paa <- array(0, dim = c(n_indices,n_years , n_ages))
for(i in 1:n_indices){index_paa[i,,] <- as.matrix(index_paa_join[index_paa_join$"Fleet"==Fleet_names[i],-c(1,2)])}




#temp <- lapply(1:n_indices, \(i) as.matrix(read.csv(here("data", paste0("index_paa_",i,".csv")))))


#array used by WHAM: (n_indices x n_years x n_ages)
#index_paa <- array(0, dim = c(n_indices,dim(temp[[1]])))
#for(i in 1:n_indices) index_paa[i,,] <- temp[[i]]

dim(index_paa)

#0/1 matrix (n_years x n_fleets) whether to use indices and proportions at age each year)
use_index_paa <- use_indices <- matrix(0L, n_years, n_indices)

use_indices[] <- as.integer(!is.na(indices))
for(i in 1:n_indices) for(y in 1:n_years) use_index_paa[y,i] <- sum(!any(is.na(index_paa[i,y,])))
#Neff must be greater than 0)
use_index_paa[which(!index_Neff>0)] <- 0




index_fracyr <- matrix(0.5,ncol=3, nrow=n_years)
#<- as.matrix(read.csv(here("data", "index_fracyr.csv")))

#should be n_years x n_indices
dim(index_fracyr)

waa_pointer_indices <- rep(1,n_indices)
  #1  + 1:n_indices


##############################################
#create list arguments to prepare_wham_input and/or set_* functions
basic_info <- list(
  n_stocks = as.integer(n_stocks),
  ages = 1:n_ages,
  n_seasons = 1L,
  n_fleets = n_fleets,
  fracyr_SSB = fracyr_ssb,
  maturity = mat,
  years = 1938:2023, #length must be n_years
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
selectivity <- list(model = rep("logistic",4), n_selblocks = 4,
  fix_pars = list(NULL,NULL,NULL,NULL),
  initial_pars = list(c(6,0.2),c(4,0.2),c(4,0.2),c(4,0.2)))

#M modeling: fixed MAA
M_in <- list(initial_MAA = MAA)

NAA_re <- list( recruit_model=3,
                sigma='rec',cor='iid')

##############################################

#make input all at once
input_all <- prepare_wham_input(basic_info = basic_info, 
                                selectivity = selectivity,
                                catch_info = catch_info, 
                                index_info = index_info, 
                                NAA_re = NAA_re,
                                M = M_in,
                                age_comp='logistic-normal-pool0'
                                )


names(input_all)
names(input_all$pa)
#compare 
nofit_all <- fit_wham(input_all, do.fit = FALSE)
names(nofit_all)

nofit_all$fn()



nofit_allrep<-nofit_all$report()
#nofit_allrep[[names(nofit_allrep)[grepl("nll",names(nofit_allrep))]]]
nofit_allrep$nll
nofit_allrep$"nll_sel"  
nofit_allrep$"nll_agg_catch"
nofit_allrep$"nll_Ecov_obs"
nofit_allrep$"nll_agg_indices" 
nofit_allrep$"nll_Ecov_obs_sig" 
nofit_allrep$"nll_NAA"          
nofit_allrep$"nll_catch_acomp"  
nofit_allrep$"nll_index_acomp" 

fit_all <- fit_wham(input_all, do.retro = FALSE, do.osa = FALSE, do.sdrep = FALSE)

#this is the best way to get the parameter estimates
names(fit_all$env$last.par.best)
names(fit_all$env$parList())


res_dir <- file.path(getwd(),"petrale_onesex")
dir.create(res_dir)

saveRDS(fit_all, file.path(res_dir,"fit_all.RDS"))


fit_all <- do_reference_points(fit_all, do.sdrep = TRUE)
fit_all$peels <- retro(fit_all)
fit_all <- make_osa_residuals(fit_all)

tmp.dir <- tempdir(check=TRUE)
plot_wham_output(fit_all, dir.main = res_dir)

fit_RDS <- file.path(res_dir,"fit.RDS")
saveRDS(fit_all, fit_RDS)

x <- jitter_wham(fit_RDS = fit_RDS, n_jitter = 10, res_dir = res_dir, do_parallel = FALSE)
sapply(x[[1]], function(y) y$obj) #nlls

