# ---
#
# title: "Elf_utils"
# 
# ---


library(sf)
library(raster)
library(tidyverse)
library(mapview)




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats


sf2coords = function(SF){
  
  
  testmode=F
  
  if (testmode){
    
    SF = st_read("D:/HerronK/ELF_Project/AndrewsWork/layers/test_Point.shp")
    
    
  }
  
  COORDS = st_coordinates(SF)
  
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### coord2SF - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats


coord2SF = function(COORDS, CRS, MV=F){
  
  
  testmode=F
  
  if (testmode){
    
    SF_test = st_read("D:/HerronK/ELF_Project/AndrewsWork/layers/New-Treeline-Layers/samp_points.shp")
    COORDS = sf2coords(SF_test) %>% 
      as_tibble()
    CRS = st_crs(SF_test)
    mapview(SF_test)
  }
  
  POINTS = st_as_sf(COORDS, coords = c("X","Y")) %>% st_set_crs(CRS)
  
  if (MV){mapview(SF_test)}
  
  
  return(POINTS)
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats


clip_raster_to_poly = function(SF){
  
  
  testmode=F
  
  if (testmode){
    
    SF = st_read("D:/HerronK/ELF_Project/AndrewsWork/layers/test_Point.shp")
    
    
  }
  
  COORDS = st_coordinates(SF)
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats


zonal_stats = function(SF){
  
  
  testmode=F
  
  if (testmode){
    
    SF = st_read("D:/HerronK/ELF_Project/AndrewsWork/layers/test_Point.shp")
    
    
  }
  
  COORDS = st_coordinates(SF)
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats




flatten_rast = function(RAST, PHI, XY_ORIG){
  
  RAST = DTM_cropped_masked
  ZVALS = RAST@data@values
  ncells = RAST@ncols*DTM_cropped_masked@nrows
  CELLS = 1:ncells
  XY = t(sapply(CELLS, function(x) {xyFromCell(RAST,x)} )) %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(Z = ZVALS)
  
  names(XY) = c("X", "Y","Z")
  
  X_cent = XY_ORIG[1]
  Y_cent = XY_ORIG[2]
  XY_flat = XY %>%
    mutate(
      DX = X - X_cent,
      DY = Y - Y_cent,
      R = sqrt(DX^2+DY^2),
      OMEGA = atan2(DX,DY),
      OMEGA_ADJ = OMEGA+PHI+pi/2,
      RX = R * cos(OMEGA_ADJ)+ median(X),
      RY = R * sin(OMEGA_ADJ)+ median(Y),
      XNEW = round(RX),
      YNEW = round(RY))
  XYZ_flat = XY_flat %>%
    dplyr::select(X = XNEW, Y = YNEW, Z = Z) %>%
    filter(!is.na(Z))
  r_flat = rasterFromXYZ(XYZ_flat, crs = 2193)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats


rasterFromXYZ = function(SF){
  
  
  testmode=F
  
  if (testmode){
    
    SF = st_read("D:/HerronK/ELF_Project/AndrewsWork/layers/test_Point.shp")
    
    
  }
  
  COORDS = st_coordinates(SF)
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Converts TM Coordinates to a vector of floats




crop_rast = function(RAST, VECT){
  RAST_cropped = crop(RAST, extent(VECT))
  RAST_cropped_masked = mask(RAST_cropped, VECT)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### sf2coords - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

find_steepest_angle = function(LEN, WID, DIV_deg, SP){
  PHI_deg_seq = seq(DIV_deg,360,DIV_deg)
  tb = data.frame(PHI_deg = PHI_deg_seq ) %>%
    mutate(
      PHI = PHI_deg * pi/180,
      SLOPE_m = NA,
      SLOPE_c = NA,
      SLOPE_r2 = NA,
    )
  for (iPHI in 1:length(PHI_deg_seq)){
    # iPHI=7
    print(paste("Processing angle", iPHI,"of",length(PHI_deg_seq) ))
    c_PHI_deg = PHI_deg_seq[iPHI]
    PHI = c_PHI_deg * pi/180
    RECT = create_rect(SP, LEN, WID, PHI)
    RECT_shp = RECT[[1]]
    DTM_cropped = crop(DTM, extent(RECT_shp))
    DTM_cropped_masked = mask(DTM_cropped, RECT_shp)
    ZVALS = DTM_cropped_masked@data@values
    ncells = DTM_cropped_masked@ncols*DTM_cropped_masked@nrows
    CELLS = 1:ncells
    XY = t(sapply(CELLS, function(x) {xyFromCell(DTM_cropped_masked,x)} )) %>%
      as.data.frame() %>%
      as_tibble() %>%
      mutate(Z = ZVALS)
    names(XY) <- c("X","Y","Z")
    X_cent = SP[1]
    Y_cent = SP[2]
    XY_dist = XY %>%
      mutate(
        DX = X - X_cent,
        DY = Y - Y_cent,
        R = sqrt(DX^2+DY^2))
    SLOPE_REG_STATS = summary(lm(Z ~ R, data = XY_dist))
    SLOPE_REG_ICP = SLOPE_REG_STATS$coefficients[1]
    SLOPE_REG_SLP = SLOPE_REG_STATS$coefficients[2]
    SLOPE_REG_RSQ = SLOPE_REG_STATS$r.squared
    tb$SLOPE_m[iPHI] = SLOPE_REG_SLP
    tb$SLOPE_c[iPHI] = SLOPE_REG_ICP
    tb$SLOPE_r2[iPHI] = SLOPE_REG_RSQ
  }
  #find angle of the most negative slope
  PHI_STEEPEST = tb$PHI[which.min(tb$SLOPE_m)]
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### xyToDist - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


xyToDist = function(DF, ORIGIN, X, Y){
  DF = DF %>% mutate(DIST = sqrt((X - ORIGIN[1])^2 +  (Y - ORIGIN[1])^2))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### treeline_plot - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


treeline_plot = function(DF_ZX, CH_targ_1, CH_targ_2){
  pt5_df = find_Z_at_CH(DF_ZX$ELEV_BELOW_TL, DF_ZX$CH, CH_targ_1)
  pt3_df = find_Z_at_CH(DF_ZX$ELEV_BELOW_TL, DF_ZX$CH, CH_targ_2)
  g = ggplot(DF_ZX) + geom_line(aes(ELEV_BELOW_TL, CH))
  g = g + geom_vline(xintercept = 0, color = "red")
  g = g + geom_point(data = pt5_df, aes(X, Y),color ="blue", size = 3)
  g = g + geom_point(data = pt3_df, aes(X, Y),color ="blue", size = 3)
  g = g + labs(x="Elevation below tree line (m)", y = "Canopy height (m)")
  g = g + theme_minimal()
  g
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### find_Z_at_CH - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


find_Z_at_CH = function(Z,CH, CH_targ){
  Z = DF_ZX$ELEV_BELOW_TL
  CH = DF_ZX$CH
  CH_targ = 5
  ix = which(CH>=CH_targ)
  Z_ix = Z[ix]
  CH_ix = CH[ix]
  min_Z_ix = min(Z_ix)
  ixx_min_Z_ix = which.min(Z_ix)
  CH_at_targ_CH = CH[ixx_min_Z_ix]
  zout = data.frame(X = min_Z_ix, Y = CH_at_targ_CH)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### create_rect - Converts TM Coordinates to a vector of floats
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


create_rect = function(SP, len, wid, PHI, ext_back = 0){
  testmode = F
  if (testmode){
    SP[1] = 5477617
    SP[2] = 1798886
    len = 500
    wid = 20
    PHI=360 * pi/180
    ext_back = 100
  }
  if (ext_back == 0){
    x1 = SP[1]
    y1 = SP[2]
  }else{
    x1_old = SP[1]
    y1_old = SP[2]
    x1 = x1_old + ext_back*cos(PHI+pi)
    y1 = y1_old + ext_back*sin(PHI+pi)
  }
  x2 = x1 + len * cos(PHI)
  y2 = y1 + len * sin(PHI)
  dx = x2 - x1
  dy = y2 - y1
  r = sqrt(dx**2 + dy**2)
  p = atan2(dy, dx)
  v1_x = x1 + (0.5*wid) * cos(p - pi/2)
  v2_x = x1 + (0.5*wid) * cos(p + pi/2)
  v3_x = x2 + (0.5*wid) * cos(p - pi/2)
  v4_x = x2 + (0.5*wid) * cos(p + pi/2)
  v1_y = (y1 + (0.5*wid) * sin(p - pi/2))*-1
  v2_y = (y1 + (0.5*wid) * sin(p + pi/2))*-1
  v3_y = (y2 + (0.5*wid) * sin(p - pi/2))*-1
  v4_y = (y2 + (0.5*wid) * sin(p + pi/2))*-1
  points_x = c(v1_x, v2_x, v4_x, v3_x, v1_x)
  points_y = -1*(c(v1_y, v2_y, v4_y, v3_y, v1_y))
  rect = data.frame(X =points_x , Y = points_y) %>%
    sf::st_as_sf(coords = c("X","Y")) %>%
    sf::st_set_crs(2193) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  if (testmode){
    MV = mapview(rect)
    PTS = mapview(xyTosf(x1,y1))
    PTS_ORIG = mapview(xyTosf(SP[1],SP[2]))
    MV + PTS + PTS_ORIG
  }
  OUT = list(rect, MV, points_x, points_y)
}
