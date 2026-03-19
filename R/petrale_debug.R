

#read in input RDS
input_all<-readRDS("../data/input_all.RDS")

fit_all <- fit_wham(input_all, do.retro = FALSE, do.osa = FALSE, 
                    do.sdrep = FALSE)
check_convergence(fit_all)
plot_wham_output(fit_all, dir.main = "../")

