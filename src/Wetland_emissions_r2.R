## Wetland_emissions_r2.R
## In use: 2021-11-02 20:00
#
# Load in the various state wetland fraction rasters
# These overlap somewhat, so crop each to the squares within each state
# Then add together and assign fluxes to each class
################################################################################
#Manually defined variables

Input_directory <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Raw_data_files/"
Output_directory <- "G:/My Drive/Shepson Group Drive/Kris/Indy Inversion/Inventories/Hi-res_CH4_inventory_development/Processed_output/Wetlands/"

# wetcharts_0.1_deg_file <- file.path(Output_directory,"XESMF_wetcharts_downscaling","2022-10-04_cold_NALCMS_XESMF_Wetcharts_2015.gri")
wetcharts_0.1_deg_file <- file.path(Output_directory,"XESMF_wetcharts_downscaling","2022-10-04_warm_NALCMS_XESMF_Wetcharts_2015.gri")
# wetcharts_0.1_deg_file <- file.path(Output_directory,"XESMF_wetcharts_downscaling","2022-09-15_NALCMS_XESMF_Wetcharts_2015.gri")
#higher res wetcharts that was disaggregated using NALCMS or NLCD.  Using
#projectraster or XESMF.

# built to loop code to calculate d01 and d03 emission maps each time
state_shapefile <- 'G:/My Drive/Shepson Group Drive/Kris/Old_outdated_or_complete/NYC SF work/tl_2019_us_state/tl_2019_us_state.shp'
wetland_fraction_dir <- Output_directory
domain <- c('d03',
            'd01')
output_dir <- c(Output_directory,
                Output_directory)
state_list <- list(c("IN"),
                   c("MI","WI","MN","IA","MO","IL","IN","OH","PA","NY","WV","VA","NC","TN","KY","AR","MD"))
nrows <- c(160, 92)
ncols <- c(230, 154)
xmn <- c(-87.3, -93.8)
xmx <- c((0.01*230)+-87.3, (0.1*154)+-93.8)
ymn <- c(39, 35.2)
ymx <- c(39+(0.01*160), 35.2+(0.1*92))

# lon.res<-0.1 # resolution in degrees longitude
# lat.res<-0.1  # resolution in degrees latitude
# numpix.x<-154  # number of pixels in x directions in grid
# numpix.y<-92  #number of pixels in y directions in grid
# lon.ll<--93.8
# lat.ll<-35.2
#Indy D01

# lon.res<- 0.01 # resolution in degrees longitude
# lat.res<- 0.01 # resolution in degrees latitude
# numpix.x <- 230 # number of pixels in x directions in grid
# numpix.y <- 160 #number of pixels in y directions in grid
# lon.ll<- -87.3
# lat.ll<- 39
# Indy D03
################################################################################
#load packages
packagecheck <- c("raster","rgdal","ncdf4","maps")
for(i in length(packagecheck)){
  if(length(find.package(packagecheck[i],quiet = TRUE))<1){
    install.packages(packagecheck[i])
  }
}

invisible(suppressPackageStartupMessages(lapply(packagecheck, library, character.only=TRUE)))
rm(packagecheck,i)

#raster + rgdal = raster filetype functionalities
#ncdf4 = .nc filetype functionalities
################################################################################
#load in and process the Wetland_fraction_r1 output to convert from wetland
#coverage to wetland emissions

states <- readOGR(state_shapefile)
states_wgs84 <- spTransform(states,CRS(SRS_string="EPSG:4326"))  # Transform to WGS84

for(j in 1:length(domain)){
  # Initialise rasters that will hold the total fluxes (all states)
  target_raster <- raster(nrows=nrows[j], ncols=ncols[j], xmn=xmn[j], xmx=xmx[j], ymn=ymn[j], ymx=ymx[j], crs=4326)  # WGS84
  target_raster[] <- 0
  E2_frac <- target_raster
  M2_frac <- target_raster
  R1_frac <- target_raster
  R2_frac <- target_raster
  R3_frac <- target_raster
  R4_frac <- target_raster
  L1_frac <- target_raster
  L2_frac <- target_raster
  PFO_frac <- target_raster
  PNF_frac <- target_raster
  
  # Load in state by state, and in each case retain only the fluxes for cells within that state
  # Combine all the fluxes in the _frac rasters as we go
  for(i in 1:length(state_list[[j]])){
    state_border <- subset(states_wgs84, STUSPS==state_list[[j]][i])
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_E2_',domain[j],'.grd'))){
      E2_frac <- E2_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_E2_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_M2_',domain[j],'.grd'))){
      M2_frac <- M2_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_M2_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_R1_',domain[j],'.grd'))){
      R1_frac <- R1_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_R1_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_R2_',domain[j],'.grd'))){
      R2_frac <- R2_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_R2_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_R3_',domain[j],'.grd'))){
      R3_frac <- R3_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_R3_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_R4_',domain[j],'.grd'))){
      R4_frac <- R4_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_R4_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_L1_',domain[j],'.grd'))){
      L1_frac <- L1_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_L1_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_L2_',domain[j],'.grd'))){
      L2_frac <- L2_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_L2_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_PFO_',domain[j],'.grd'))){
      PFO_frac <- PFO_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_PFO_',domain[j],'.grd')),state_border,updatevalue=0)
    }
    if(file.exists(paste0(wetland_fraction_dir,state_list[[j]][i],'_PNF_',domain[j],'.grd'))){
      PNF_frac <- PNF_frac + mask(raster(paste0(wetland_fraction_dir,state_list[[j]][i],'_PNF_',domain[j],'.grd')),state_border,updatevalue=0)
    }
  }
  
  # Check that the fractions are always between 0 and 1
  all_frac <- E2_frac + M2_frac + R1_frac + R2_frac + R3_frac + R4_frac + L1_frac + L2_frac + PFO_frac + PNF_frac
  max_frac <- cellStats(all_frac,max)
  min_frac <- cellStats(all_frac,min)
  cat("total wetland area ranges from",min_frac,"to",max_frac,"for",domain[j],"\n")
  
  # Take avg fluxes from SOCCR1 report (as in McKain et al) - these are in gCH4 per m2 per yr
  # In McKain et al there are no fluxes from open water
  E2_flux_SOCCR1 <- E2_frac*1.3*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  M2_flux_SOCCR1 <- M2_frac*1.3*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  PFO_flux_SOCCR1 <- PFO_frac*7.6*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  PNF_flux_SOCCR1 <- PNF_frac*7.6*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  
  # Now take avg wetland fluxes from SOCCR2 report - recalculated from the SOCCR2 lit review tables
  # in my spreadsheet "SOCCR1_vs_SOCCR2.xlsx"
  E2_flux_SOCCR2 <- E2_frac*20.44*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  M2_flux_SOCCR2 <- M2_frac*20.44*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  PFO_flux_SOCCR2 <- PFO_frac*18.52*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  PNF_flux_SOCCR2 <- PNF_frac*19.71*1e9/(16.043*365*24*60*60)  # convert to nmol/m2/s
  
  # Inland water CH4 fluxes are not included in either SOCCR1 or SOCCR2
  # For lakes, McDonald et al. (10.4319/lo.2012.57.2.0597) show that large lakes > 1 km2 constitute 71% of the total
  # lake area in the contiguous US (rising to 90% if the Great Lakes are included)
  # So use the median flux from the largest lakes class (>1 km) from Rosentreter et al. (10.1038/s41561-021-00715-2)
  L1_flux <- L1_frac*5.00*1e9/(16.043*365*24*60*60)
  L2_flux <- L2_frac*5.00*1e9/(16.043*365*24*60*60)
  
  # Also use the median river flux from Rosentreter et al. Both this and the lake flux come from extended data table 1
  R1_flux <- R1_frac*7.88*1e9/(16.043*365*24*60*60)
  R2_flux <- R2_frac*7.88*1e9/(16.043*365*24*60*60)
  R3_flux <- R3_frac*7.88*1e9/(16.043*365*24*60*60)
  R4_flux <- R4_frac*7.88*1e9/(16.043*365*24*60*60)
  
  # writeRaster(E2_flux_SOCCR1,
  #             paste0(output_dir[j],'E2_flux_SOCCR1_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning E2, based on fluxes from the SOCCR1 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(M2_flux_SOCCR1,
  #             paste0(output_dir[j],'M2_flux_SOCCR1_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning M2, based on fluxes from the SOCCR1 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(PFO_flux_SOCCR1,
  #             paste0(output_dir[j],'PFO_flux_SOCCR1_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning PFO, based on fluxes from the SOCCR1 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(PNF_flux_SOCCR1,
  #             paste0(output_dir[j],'PNF_flux_SOCCR1_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning P, except those beginning PFO, based on fluxes from the SOCCR1 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(E2_flux_SOCCR2,
  #             paste0(output_dir[j],'E2_flux_SOCCR2_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning E2, based on fluxes from the SOCCR2 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(M2_flux_SOCCR2,
  #             paste0(output_dir[j],'M2_flux_SOCCR2_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning M2, based on fluxes from the SOCCR2 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(PFO_flux_SOCCR2,
  #             paste0(output_dir[j],'PFO_flux_SOCCR2_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning PFO, based on fluxes from the SOCCR2 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(PNF_flux_SOCCR2,
  #             paste0(output_dir[j],'PNF_flux_SOCCR2_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning P, except those beginning PFO, based on fluxes from the SOCCR2 report',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(L1_flux,
  #             paste0(output_dir[j],'L1_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning L1, based on the median flux from the largest lakes class (>1 km) from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(L2_flux,
  #             paste0(output_dir[j],'L2_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning L2, based on the median flux from the largest lakes class (>1 km) from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(R1_flux,
  #             paste0(output_dir[j],'R1_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning R1, based on the median flux from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(R2_flux,
  #             paste0(output_dir[j],'R2_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning R2, based on the median flux from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(R3_flux,
  #             paste0(output_dir[j],'R3_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning R3, based on the median flux from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
  # 
  # writeRaster(R4_flux,
  #             paste0(output_dir[j],'R4_flux_',domain[j],'.nc'),
  #             force_v4=TRUE,
  #             varname='methane_emissions',
  #             varunit='nmol/m2/s',
  #             longname='Methane emissions from National Wetland Inventory classes beginning beginning R4, based on the median flux from Rosentreter et al. (2021)',
  #             NAflag=-9999,
  #             overwrite=TRUE)
}

################################################################################
# Finally load Israel's WetCHARTs-based maps and separate the d01 map into US and Canadian components
wetcharts_d01 <- raster(wetcharts_0.1_deg_file)
wetcharts_d01 <- wetcharts_d01*1e9/(1000*16.043*24*3600)

wetcharts_d03 <- raster(wetcharts_0.1_deg_file)
wetcharts_d03 <- wetcharts_d03*1e9/(1000*16.043*24*3600)
#My version was NOT in the proper units.  Converting from mg/m2day to nmol/m2s

target_raster <- raster(nrows=nrows[which(domain=="d03")],
                        ncols=ncols[which(domain=="d03")], 
                        xmn=xmn[which(domain=="d03")], 
                        xmx=xmx[which(domain=="d03")], 
                        ymn=ymn[which(domain=="d03")], 
                        ymx=ymx[which(domain=="d03")], crs="+init=epsg:4326")  #d03

wetcharts_d03 <- crop(wetcharts_d03,target_raster)

# Note that we only need to use mask here, because any cells with the centre in Canada are
# excluded from the NWI raster when we created it (also using mask)
wetcharts_canada <- mask(wetcharts_d01, states_wgs84, inverse=T, updatevalue=0)
wetcharts_US <- mask(wetcharts_d01, states_wgs84, inverse=F, updatevalue=0)


# writeRaster(wetcharts_d03,
#             file.path(Output_directory,'cold_wetcharts_d03.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions based on WetCHARTs',
#             NAflag=-9999,
#             overwrite=TRUE)
# 
# writeRaster(wetcharts_canada,
#             file.path(Output_directory,'cold_wetcharts_canada_d01.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions in Canada based on WetCHARTs',
#             NAflag=-9999,
#             overwrite=TRUE)
# 
# writeRaster(wetcharts_US,
#             file.path(Output_directory,'cold_wetcharts_US_d01.nc'),
#             force_v4=TRUE,
#             varname='methane_emissions',
#             varunit='nmol/m2/s',
#             longname='Methane emissions in the US based on WetCHARTs',
#             NAflag=-9999,
#             overwrite=TRUE)
# 
writeRaster(wetcharts_d03,
            file.path(Output_directory,'warm_wetcharts_d03.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions based on WetCHARTs',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(wetcharts_canada,
            file.path(Output_directory,'warm_wetcharts_canada_d01.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions in Canada based on WetCHARTs',
            NAflag=-9999,
            overwrite=TRUE)

writeRaster(wetcharts_US,
            file.path(Output_directory,'warm_wetcharts_US_d01.nc'),
            force_v4=TRUE,
            varname='methane_emissions',
            varunit='nmol/m2/s',
            longname='Methane emissions in the US based on WetCHARTs',
            NAflag=-9999,
            overwrite=TRUE)
