#### Checking for outliers ####
# Cleveland dotplots 
# The x-axes show the values of a variable and the y-axes the observations

# cleveland dotplots for "Crawfish eHf vs. U/Pb age"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(crawfish_hf$age_Ma,main="U/Pb age (Ma)")
dotchart(crawfish_hf$e_Hf_t,main="epsilon Hf")

# cleveland dotplots for "Crawfish eNd and 87Sr/86Sr vs. U/Pb age"
op<- par(mfrow=c(3,1),mar=c(3,3,3,1))

dotchart(crawfish_nd_sr$age_Ma,main="U/Pb age (Ma)")
dotchart(crawfish_nd_sr$e_Nd_t,main="epsilon Nd")
dotchart(crawfish_nd_sr$Sr_initial,main="87Sr/86Sr initial")

# cleveland dotplots for "Crawfish [Y] vs. U/Pb age"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(crawfish_y$age_Ma,main="U/Pb age (Ma)")
dotchart(crawfish_y$Y_cn,main="[Y] chondrite-normalized")

# cleveland dotplots for "Eastern SBB U/Pb age vs. distance from Sanak"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(eastern_sbb$age_Ma,main="U/Pb age (Ma)")
dotchart(eastern_sbb$distance,main="distance from Sanak (km)")

# cleveland dotplots for "SBB rock age vs. distance from Sanak for all available age dates"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(sbb_age_distance_all$age_Ma,main="sample age (Ma)")
dotchart(sbb_age_distance_all$distance,main="distance from Sanak (km)")

# cleveland dotplots for "SBB U/Pb age vs. distance from Sanak for all concordant U/Pb zircon ages"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(sbb_age_distance_zircons$age_Ma,main="U/Pb age (Ma)")
dotchart(sbb_age_distance_zircons$distance,main="distance from Sanak (km)")

# cleveland dotplots for "SBB eHf vs. U/Pb age for all SBB plutons"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(sbb_eHf_UPb_plutons$age_Ma,main="magmatic U/Pb age (Ma)")
dotchart(sbb_eHf_UPb_plutons$e_Hf_t,main="epsilon Hf")

# cleveland dotplots for "SBB eHf vs. U/Pb age for all CPW sediments with U/Pb age < 540 Ma"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(sbb_eHf_UPb_seds_540Ma$age_Ma_seds,main="detrital U/Pb age (Ma)")
dotchart(sbb_eHf_UPb_seds_540Ma$e_Hf_50Ma,main="epsilon Hf (t = 50 Ma)")

# cleveland dotplots for "SBB eHf vs. U/Pb age for all CPW sediments"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(sbb_eHf_UPb_seds$age_Ma_seds,main="detrital U/Pb age (Ma)")
dotchart(sbb_eHf_UPb_seds$e_Hf_50Ma,main="epsilon Hf (t = 50 Ma)")

# cleveland dotplots for "Sr/Y vs. distance from Sanak for all SBB intrusive rocks with SiO2 > 54 wt %"
op<- par(mfrow=c(2,2),mar=c(3,3,3,1))

dotchart(sbb_SrY_distance_54Si$SiO2_percent,main="SiO2 (wt %)")
dotchart(sbb_SrY_distance_54Si$Sr_ppm,main="[Sr] (ppm)")
dotchart(sbb_SrY_distance_54Si$Y_ppm,main="[Y] (ppm)")
dotchart(sbb_SrY_distance_54Si$Sr_Y,main="Sr/Y")

# cleveland dotplots for "Western SBB U/Pb age vs. distance from Sanak"
op<- par(mfrow=c(2,1),mar=c(3,3,3,1))

dotchart(western_sbb$age_Ma,main="U/Pb age (Ma)")
dotchart(western_sbb$distance,main="distance from Sanak (km)")

op<- par(mfrow=c(1,1),mar=c(3,3,3,1))

