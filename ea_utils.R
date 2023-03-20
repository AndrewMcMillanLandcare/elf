

# source("E:/OneDrive - Environmental Analytics/ea/EANZ-Code/code/R workdir/ea/ea_utils.R")


## source("D:/LUCA Team/Land Use Capability Assessments Limited/LUCA Directors - Documents/Code/luca_utils/luca_utils.R")

# pal

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Function : HS_write_filelist_to_file
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

HS_write_filelist_to_file <- function(filelist, filename) {
  fid <- file(filename, "w")
  writeLines(filelist, fid)
  close(fid)
}


library(sf)
library(tidyverse)
# library(mapview)
library(cowplot)
library(lubridate)
# library(xlsx)
library(readxl)
library(rmapshaper)
library(mapview)
# library(rmapshaper)
library(plotly)
library(tictoc)
library(parallel)
library(doParallel)
library(terra)
library(openxlsx)
library(scales)
# codedir =
# source(LP0012_utilities.R")


# source("D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/code/R/LP0012_R_Project_Nov2021/LP0012_intersection_functions.R")



# files and paths

# identify the computer (and OS)
Current_PC <- Sys.info()["nodename"]

if (Current_PC == "EANZ-DT01-Linux") {
  os <- "linux"
  rootDrive <- "/media/andrew/HDD data/"
  EDrive <- "/media/andrew/E-DRIVE-SSD/"
} else if (Current_PC == "EANZ-DT01") {
  os <- "win10"
  rootDrive <- "D:/"
  EDrive <- "E:/"
} else {
  rootDrive <- "D:/"
}

# 
# LP0012_root <- paste0(rootDrive, "LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/")
# # root2 =paste0(EDrive, "LUCA_projects/LP0012/Geospatial_data/")
# 
# # GeoSpatRoot <- "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/"
# GeoSpatRoot <- "E:/OneDrive - Environmental Analytics/ea/EANZ-Data/NZ_Geospatial_Data/"
# 
# 
# LP0012_datadir <- paste0(LP0012_root, "data/")
# berldir <- paste0(LP0012_datadir, "from_berl/")
# gisdir <- paste0(LP0012_root, "gis/")
# entity_search_terms_fn <- "Maori entities - for Andrew - with Andrews Additions.xlsx"
# entity_search_terms_ffn <- paste0(berldir, entity_search_terms_fn)
# 
# 
# Mfe_EMS_REM_RATES_dn <- LP0012_datadir
# Mfe_EMS_REM_RATES_fn <- "LUM_EMS_REM_LUT.xlsx"
# Mfe_EMS_REM_RATES_ffn <- paste0(Mfe_EMS_REM_RATES_dn, Mfe_EMS_REM_RATES_fn)
# 
# EMS_REM_RATES <- read_excel(Mfe_EMS_REM_RATES_ffn, sheet = "Classes_and_Subclasses", range = "B3:M34") %>%
#   dplyr::select(LUCSUBID, LUCNA, SUBNA, REM_RATE, EMS_RATE_DEF, EMS_RATE_HVT)
# 
# 
# Age_range_table_w_EMS_ffn <- paste0(LP0012_datadir, "Age_range_table_w_EMS.RDS")
# Age_range_table_w_EMS <- readRDS(Age_range_table_w_EMS_ffn)
# 
# NEFD_by_ROHE_w_EMS_ffn <- paste0(LP0012_datadir, "NEFD_by_ROHE_w_EMS.RDS")
# NEFD_by_ROHE_w_EMS <- readRDS(NEFD_by_ROHE_w_EMS_ffn)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: make the system open a csv file so we can look at it
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

copen <- function(ffn) {
  CMD <- paste0("\"", ffn, "\"")
  system("cmd.exe", input = CMD)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: savv  not working yet
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

savv <- function(D, dirname_to_write = LP0012_datadir) {
  
  # funtion to get the variable name
  getvarname <- function(v1) {
    deparse(substitute(v1))
  }
  
  # function to save the file
  s2file <- function(D, varname, dirname_to_write) {
    ffn <- paste0(dirname_to_write, varname, ".RDS")
    
    saveRDS(D, ffn)
    print(paste("Saving Data in variable ", varname, " to file: ", ffn))
  }
  
  varname <- getvarname(D)
  s2file(D, varname, dirname_to_write)
}

#

# DF = POxFCxLUM_w_ROHE_LO_NAT_AGE
# varname = "AGE_wtd_avg"
# trim_perc = 1
# DF[[var]]
# nbins = 10
#
# luca_hist(DF, varname, nbins = 10, trim_perc = 1, plot = F){
#
#
#
#   cdata = DF[[varname]]
#   n_orig = length(cdata)
#   cdata_notna = cdata[!is.na(cdata)]
#   n_notna = length(cdata_notna)
#   n_na_removed = n_orig - n_notna
#
#   q_lo = quantile(cdata_notna,trim_perc/1e2)
#   q_hi = quantile(cdata_notna,(100-trim_perc)/1e2)
#   cdata_trimmed = cdata_notna[cdata_notna>q_lo & cdata_notna<=q_hi]
#   cdata_trimmed_count=length(cdata_trimmed)
#   loss_on_trimming = cdata_count - cdata_trimmed_count
#   loss_on_trimming_perc = 100 * loss_on_trimming/cdata_count
#
#   data_min = min(cdata_trimmed)
#   data_max = max(cdata_trimmed)
#
#   data_rng = data_max - data_min
#   binwidth = data_rng/(nbins-1)
#   bin_lims = seq(data_min, data_max, binwidth)
#
#   factorx <- factor(cut(cdata_trimmed, breaks=nclass.Sturges(cdata_trimmed)))
#   factorx <- factor(cut(cdata_trimmed, breaks=bin_lims))
#   #Tabulate and turn into data.frame
#   xout <- as.data.frame(table(factorx))
#   #Add cumFreq and proportions
#   xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
#
#
#   table(cdata)
#
# if (
#
#
#
# }



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: validate_sh
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


validate_sh <- function(shp, poly_opt = T, paral = F) {
  print("Validating Shape File")
  
  if (dim(shp)[1] != 0) {
    print("Checkling for validity")
    if (length(which(!st_is_valid(shp))) > 0) {
      print("Making layer valid")
      
      
      if (paral) {
        
        
        # make the  cluster
        
        print("Detecting cores")
        n_cores_det <- detectCores(logical = TRUE) # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
        n_cores_to_use <- n_cores_det - 1
        
        print(paste("Parallel Processing: making a cluster of ", n_cores_to_use, "cores"))
        cluster1 <- makeCluster(n_cores_to_use)
        
        
        # register the  cluster
        registerDoParallel(cluster1)
        
        # split the data
        split_vector <- rep(1:n_cores_to_use, each = nrow(P1) / n_cores_to_use, length.out = nrow(shp))
        split_input <- split(shp, split_vector)
        
        print(paste("Parallel Processing: exporting data to cluster"))
        
        
        # export the data to the cluster
        clusterExport(cluster1, list("shp"), envir = environment())
        
        
        
        print(paste("Parallel Processing: starting multicore work"))
        # execute the command
        system.time(
          
          # print("validating in parallel")
          split_results <- c(parLapply(cluster1, split_input, st_make_valid, P2))
        )
        print(paste("Parallel Processing: finished multicore work"))
        
        # Define the output_class. If length is greater than two, then grab the second variable.
        output_class <- class(split_results[[1]])
        if (length(output_class) == 2) {
          output_class <- output_class[2]
        }
        
        print(paste("...finished Parallel Processing: recombining results"))
        
        # Combine results back together. Method of combining depends on the output from the function.
        if (output_class == "matrix") {
          shp_valid <- do.call("rbind", split_results)
          names(shp_valid) <- NULL
        } else if (output_class == "sfc") {
          shp_valid <- do.call("c", split_results)
          shp_valid <- sf_func(shp_valid) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
        } else if (output_class %in% c("list", "sgbp")) {
          shp_valid <- do.call("c", split_results)
          names(shp_valid) <- NULL
        } else if (output_class == "data.frame") {
          shp_valid <- do.call("rbind", split_results)
        } else {
          stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
        }
      } else {
        shp_valid <- st_make_valid(shp)
      }
    } else {
      shp_valid <- shp
    }
  } else {
    shp_valid <- shp
  }
  
  if (poly_opt) {
    shp_valid <- shp_valid %>% st_collection_extract("POLYGON")
  }
  
  return(shp_valid)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: get_ng
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_ng <- function(DF) {
  DF %>%
    mutate(
      DF_UID = 1:nrow(DF),
      DF_AREA_HA = as.numeric(st_area(.)) / 1e4
    ) %>%
    st_set_geometry(NULL) %>%
    as_tibble()
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: get_LO
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_LO <- function(DF) {
  DF %>%
    mutate(LO = ifelse(!is.na(BLOCK_ID), "ML", "GT"))
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: remove_accents
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

remove_accents <- function(input) {
  
  
  # repl_str = c("a" = "ā",
  #              "e" = "\u113",
  #              "i" = "\u12b",
  #              "o" = "\u14d",
  #              "u" = "\u101",
  #              "A" = "\u100",
  #              "E" = "\u112",
  #              "I" = "\u12a",
  #              "O" = "\u14c",
  #              "U" = "\u16a")
  
  # repl_str = c("ā" = "a",
  #              "ē" = "e",
  #              "ī" = "i",
  #              "ō" = "o",
  #              "ū" = "u",
  #              "Ā" = "A",
  #              "Ē" = "E",
  #              "Ī" = "I",
  #              "Ō" = "O",
  #              "Ū" = "U")
  
  
  output <- input %>%
    str_replace_all("\u101", "a") %>%
    str_replace_all("\u113", "e") %>%
    str_replace_all("\u12b", "i") %>%
    str_replace_all("\u14d", "o") %>%
    str_replace_all("\u16b", "u") %>%
    str_replace_all("\u100", "A") %>%
    str_replace_all("\u112", "E") %>%
    str_replace_all("\u12a", "I") %>%
    str_replace_all("\u14c", "O") %>%
    str_replace_all("\u16a", "U")
  
  
  output <- trimws(output)
  
  return(output)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: rdetect_many_owner
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

detect_many_owner <- function(Search_Results, owner_num_th = 5) {
  
  
  # Search_Results = search_PO("Kahungunu",prop_and_owners)
  
  # extract the element of search results that contains the filtered spatial data
  owner_hits_sp <- Search_Results[[4]]
  
  
  #
  
  owner_table <- as_tibble(data.frame(owners = owner_hits_sp, isLikelyEntity = T))
  
  for (i in 1:length(owner_hits_sp)) {
    owner_string <- owner_table$owners[i]
    owner_string_brk <- str_split(owner_string, ",")[[1]]
    num_sep_owners <- length(owner_string_brk)
    if (num_sep_owners > owner_num_th) {
      owner_table$isLikelyEntity[i] <- F
    }
  }
  
  return(owner_table)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: remove_many_owner
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

remove_many_owner <- function(Search_Results, owner_num_th = 5) {
  Search_Results <- POS[[2]]
  owner_num_th <- 5
  
  owner_table <- detect_many_owner(Search_Results, owner_num_th)
  owner_table_f <- owner_table %>% filter(isLikelyEntity)
  
  
  return(owner_table_f)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: search_PO
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

search_PO <- function(Search_str, prop_and_owners, anywhere = F, NOT = NULL) {
  if (anywhere) {
    
    # if the order of the occurrence of the words is unimportant then
    # split the search string up using spaces and make the condition that all words have to be in the owner but in no particular order
    
    words <- str_split(Search_str, " ")[[1]]
    
    PC_found <- prop_and_owners %>% filter(str_detect(owners, words[1]) & str_detect(owners, words[2]))
  } else {
    
    # or keep the order of the words
    
    PC_found <- prop_and_owners %>% filter(str_detect(owners, Search_str))
  }
  
  
  if (!is.null(NOT)) {
    PC_found <- PC_found %>% filter(!str_detect(owners, NOT))
  }
  
  
  
  
  
  nhits <- dim(PC_found)[1]
  
  
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print("       Search Results")
  print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste("Searched for the string : ", Search_str))
  print(paste("....Found a total of  : ", nhits, "owners"))
  
  if (nhits > 0) {
    
    
    # check if owners look like more like a trustee (many names) rather than an incorporations
    
    
    
    
    
    
    
    PC_found_owners <- PC_found %>% pull(owners)
    
    
    
    
    
    PC_found_Area <- PC_found %>%
      mutate(Area = as.numeric(st_area(.) / 1e4)) %>%
      summarise(Area = sum(Area)) %>%
      pull(Area)
    
    
    
    print(PC_found_owners)
    
    unq_owners <- unique(PC_found_owners)
    nunq_owners <- length(unq_owners)
    
    print(paste(" ... found", nunq_owners, "Unique owners: "))
    print(unq_owners)
    
    print(paste("Total area =", PC_found_Area, "ha "))
    print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  } else {
    PC_found <- NA
    PC_found_owners <- NA
    PC_found_Area <- NA
    unq_owners <- 0
    nunq_owners <- 0
  }
  
  
  OUT <- list(PC_found, PC_found_owners, PC_found_Area, unq_owners, nunq_owners)
  return(OUT)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: POS_search
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

POS_search <- function(D_entities, enum, prop_and_owners, disable_word12 = F) {
  ParentComp <- D_entities$`Parent organisation - Trading/formal name`[enum]
  othernames <- D_entities$`Other names used`[enum]
  subsids <- unlist(str_split(D_entities$Subsidiaries[enum], pattern = ","))
  
  
  ParentComp_ar <- remove_accents(ParentComp)
  othernames_ar <- remove_accents(othernames)
  subsids_ar <- remove_accents(subsids)
  
  PC_SearchRes <- search_PO(ParentComp_ar, prop_and_owners)
  ON_SearchRes <- search_PO(othernames_ar, prop_and_owners)
  SS_SearchRes <- search_PO(subsids_ar, prop_and_owners)
  
  
  if (!disable_word12) {
    words_PC <- str_split(ParentComp_ar, " ")[[1]]
    nwords_PC <- length(words_PC)
    
    Word1 <- str_split(ParentComp_ar, " ")[[1]][1]
    Word2 <- str_split(ParentComp_ar, " ")[[1]][2]
    
    
    # Try First two words of Parent Company
    Search_str <- paste(Word1, Word2)
    
    print(paste("Search using first two words of Parent Co :", Search_str))
    
    words12_SRC_RES <- search_PO(Search_str, prop_and_owners)
  } else {
    words12_SRC_RES <- NA
  }
  
  return(list(PC_SearchRes, ON_SearchRes, SS_SearchRes, words12_SRC_RES))
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: get_berl_pal()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_berl_pal <- function() {
  
  # BERL Standard Palette
  Berl_Std_Palette_ffn <- paste0(rootDrive, "LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-08-LP0005-BERL-GHGMaoriEconomy/Data/berl data/berl_standard_palette.csv")
  
  Berl_Std_Palette <- read_csv(Berl_Std_Palette_ffn) %>%
    mutate(berl_pal = rgb2hex(R, G, B))
  
  berl_pal <- Berl_Std_Palette %>% pull(berl_pal)
  
  return(berl_pal)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: get_ea_pal()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_ea_pal <- function() {
  
  # EAANZ Standard Palette
  ea_Std_Palette_ffn <- "E:/OneDrive - Environmental Analytics/ea/EANZ-Styles/palettes/pal_eanz.csv"
  
  ea_Std_Palette <- read_csv(ea_Std_Palette_ffn) %>%
    mutate(ea_pal = rgb2hex(R, G, B))
  
  ea_pal <- ea_Std_Palette %>% pull(ea_pal)
  
  return(ea_pal)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: ea_load(layer_code)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ea_load <- function(layer_code, use_E_drive = F, force_reload = F) {
  
  
  
  
  
  
  
  # layer_codes
  #
  # "ML"
  # "LCDB"
  # "LUM"
  # "NZCL"
  # "LUC"
  # "ROHE"
  # "CL"
  # "PA"
  # "AB"
  # "PO"
  # "FC"
  # "PLP_orig"
  # "PLP_CL"
  # "PLP_NCL"
  # "PLP_gt_1ha"
  # "UR"
  # "FLOL"
  
  if (!use_E_drive) {
    GeoSpatRoot <- "E:/OneDrive - Environmental Analytics/ea/EANZ-Data/NZ_Geospatial_Data/"
  } else {
    GeoSpatRoot <- paste0(EDrive, "LUCA_projects/LP0012/Geospatial_data/")
  }
  
  
  LUC_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC_2021/nzlri-land-use-capability-2021.shp")
  
  # This dataset below was created on 14/1/22. Corrected for the problem that the SI data set was in the wrong CRS
  # The correction was done using the script "Create_Combined_LRI_all_NZ_EPSG2193.R in the LP0012 Project
  LRI_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC/nzlri-ALL-NZ-edition-2-all-attributes.gpkg")
  
  
  LUC_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC_2021/nzlri-land-use-capability-2021.shp")
  
  # This dataset below was created on 14/1/22. Corrected for the problem that the SI data set was in the wrong CRS
  # The correction was done using the script "Create_Combined_LRI_all_NZ_EPSG2193.R in the LP0012 Project
  LRI_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC/nzlri-ALL-NZ-edition-2-all-attributes.gpkg")
  
  
  merge_LRI_NI_with_LRI_SI <- F
  
  if (merge_LRI_NI_with_LRI_SI) {
    LRI_LUC_NI_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC/nzlri-north-island-edition-2-all-attributes.shp")
    LRI_LUC_SI_ffn <- paste0(GeoSpatRoot, "NZLRI_LUC/nzlri-south-island-edition-2-all-attributes.shp")
    
    LRI_NI <- st_read(LRI_LUC_NI_ffn) %>% mutate(ISLAND = "NI")
    LRI_SI <- st_read(LRI_LUC_SI_ffn) %>% mutate(ISLAND = "SI")
    
    LRI <- LRI_NI %>% bind_rows(LRI_SI)
    st_write(LRI, LRI_ffn, append = F)
  }
  
  # This one below does not have the complete list of fields (it also only has 66070 features
  # LRI_LUC_ffn = paste0(GeoSpatRoot,'NZLRI_LUC/nzlri-land-use-capability.shp')
  # LRI_LUC_brf = st_read(LRI_LUC_ffn)
  # qmv(LRI_LUC_brf)
  #
  ML_ffn <- paste0(GeoSpatRoot, "MaoriLandOnline/MLC_2017_05-Shapefile/MLC_May2017_Shapefile/MLC_BLOCKS_May2017_joined2mgmt.shp")
  LCDB_ffn <- paste0(GeoSpatRoot, "LCDB_v5.0/lris-lcdb-v50-land-cover-database-version-50-mainland-new-zealand-SHP/lcdb-v50-land-cover-database-version-50-mainland-new-zealand.shp")
  LUM_ffn <- paste0(GeoSpatRoot, "LUCAS/lucas-nz-land-use-map-1990-2008-2012-2016-v008.shp")
  # NZ_coastline_ffn = 'D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/NZ Coastline/nz-coastlines-topo-1500k.shp'
  NZ_coastline_ffn <- paste0(GeoSpatRoot, "NZ Coastline/nz-coastlines-topo-1500k.shp")
  # Agribase_ffn = 'D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/Agribase_SheepBeef/Agribase_enhanced_001_sb.shp'
  Agribase_ffn <- paste0(GeoSpatRoot, "Agribase_MPI/agb_full_042021.gdb")
  # Property_with_Owners_ffn = D:\LUCA Team\Land Use Capability Assessments Limited\LUCA team site - Documents\LUCA-A\NZ_Geospatial_Data\Property_with_Owner_info/
  
  
  TA_dn <- paste0(GeoSpatRoot, "statsnzterritorial-authority-2019-generalised-SHP/")
  TA_fn <- "territorial-authority-2019-generalised.shp"
  TA_ffn <- paste0(TA_dn, TA_fn)
  CL_ffn <- paste0(GeoSpatRoot, "lds-linz-managed-crown-property-SHP/linz-managed-crown-property.shp")
  # PA_ffn = paste0(GeoSpatRoot,'lds-protected-areas-SHP/protected-areas.shp')
  PA_ffn <- paste0(GeoSpatRoot, "Protected-Areas/protected-areas.gpkg")
  
  # FC_dn = paste0(GeoSpatRoot,"mfe-lucas-nz-forest-clearing-2008-2018-v018-SHP/")
  # FC_fn = "lucas-nz-forest-clearing-2008-2018-v018.shp"
  # FC_ffn = paste0(FC_dn, FC_fn)
  
  FC_dn <- paste0(GeoSpatRoot, "mfe-lucas-nz-forest-clearing-2008-2020-v019-SHP/")
  FC_fn <- "lucas-nz-forest-clearing-2008-2020-v019.shp"
  FC_ffn <- paste0(FC_dn, FC_fn)
  
  
  PLP_dn <- paste0(GeoSpatRoot, "PLP_usable/")
  PLP_fn <- "PLP.gpkg"
  PLP_ffn <- paste0(PLP_dn, PLP_fn)
  
  PLP_CL_dn <- paste0(GeoSpatRoot, "PLP_usable/")
  PLP_CL_fn <- "PLP_CL.gpkg"
  PLP_CL_ffn <- paste0(PLP_CL_dn, PLP_CL_fn)
  
  PLP_NCL_dn <- paste0(GeoSpatRoot, "PLP_usable/")
  PLP_NCL_fn <- "PLP_NCL.gpkg"
  PLP_NCL_ffn <- paste0(PLP_NCL_dn, PLP_NCL_fn)
  
  PLP_gt_1ha_dn <- paste0(GeoSpatRoot, "PLP_usable/")
  PLP_gt_1ha_fn <- "PLP_gt_1ha.gpkg"
  PLP_gt_1ha_ffn <- paste0(PLP_gt_1ha_dn, PLP_gt_1ha_fn)
  
  PLP_orig_dn <- paste0(GeoSpatRoot, "primary-land-parcels/")
  PLP_orig_fn <- "nz-primary-land-parcels-all-nz.gpkg"
  PLP_orig_ffn <- paste0(PLP_orig_dn, PLP_orig_fn)
  
  
  IWI_dn <- paste0(GeoSpatRoot, "IWI/")
  IWI_fn <- "Layer_Iwi_AreasOfInterest.shp"
  IWI_ffn <- paste0(IWI_dn, IWI_fn)
  
  RC_dn <- paste0(GeoSpatRoot, "regional_councils/")
  RC_fn <- "regional-council-2020-generalised.shp"
  RC_ffn <- paste0(RC_dn, RC_fn)
  
  
  UR_dn <- paste0(GeoSpatRoot, "urban-rural-2022-generalised/")
  UR_fn <- "urban-rural-2022-generalised.gpkg"
  UR_ffn <- paste0(UR_dn, UR_fn)
  
  FLOL_dn <- paste0(GeoSpatRoot, "LINZ-LANDONLINE/")
  FLOL_fn <- "landonline-line.shp"
  FLOL_ffn <- paste0(FLOL_dn, FLOL_fn)
  
  
  
  rejoin_PLP <- F
  
  if (rejoin_PLP) {
    PLP_ffn1 <- paste0(PLP_dn, "nz-primary-land-parcels.shp")
    PLP_ffn2 <- paste0(PLP_dn, "nz-primary-land-parcels.2.shp")
    
    PLP_1 <- st_read(PLP_ffn1)
    PLP_2 <- st_read(PLP_ffn2)
    
    PLP <- PLP_1 %>% bind_rows(PLP_2)
    st_write(PLP, PLP_ffn, append = F)
  }
  
  
  
  ROHE_ffn <- paste0(GeoSpatRoot, "rohe_BERL/rohe.shp")
  
  
  F2OPEN <- case_when(
    layer_code == "ML" ~ ML_ffn,
    layer_code == "LCDB" ~ LCDB_ffn,
    layer_code == "LUM" ~ LUM_ffn,
    layer_code == "NZCL" ~ NZ_coastline_ffn,
    layer_code == "LUC" ~ LUC_ffn,
    layer_code == "LRI" ~ LRI_ffn,
    layer_code == "AB" ~ Agribase_ffn,
    layer_code == "TA" ~ TA_ffn,
    layer_code == "ROHE" ~ ROHE_ffn,
    layer_code == "CL" ~ CL_ffn,
    layer_code == "PA" ~ PA_ffn,
    layer_code == "PO" ~ "PO",
    layer_code == "FC" ~ FC_ffn,
    layer_code == "PLP_orig" ~ PLP_ffn,
    layer_code == "PLP" ~ PLP_ffn,
    layer_code == "PLP_CL" ~ PLP_CL_ffn,
    layer_code == "PLP_NCL" ~ PLP_NCL_ffn,
    layer_code == "PLP_gt_1ha" ~ PLP_gt_1ha_ffn,
    layer_code == "IWI" ~ IWI_ffn,
    layer_code == "RC" ~ RC_ffn,
    layer_code == "UR" ~ UR_ffn,
  )
  
  # if layer_code == PO (Property and Owners) then load by default from RDS file
  if (layer_code == "PO") {
    
    # Load the P&O Data Set
    prop_and_owners_dn <- paste0(rootDrive, "LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/Property_with_Owner_info/")
    prop_and_owners_fn <- "nz-property-titles-including-owners-all-NZ.shp"
    prop_and_owners_ffn <- paste0(prop_and_owners_dn, prop_and_owners_fn)
    
    # binary for of data set
    PO_rds_ffn <- paste0(prop_and_owners_dn, "nz-property-titles-including-owners-all-NZ.RDS")
    
    
    load_from_shp <- F
    if (load_from_shp) {
      SF.OUT <- st_read(prop_and_owners_ffn)
      write_rds(PO, PO_rds_ffn)
    } else {
      SF.OUT <- readRDS(PO_rds_ffn)
    }
    
    # otherwise read from shape files
  } else {
    SF.OUT <- st_read(F2OPEN)
  }
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### EANZ style - a style created by EANZ designed for inclusion into EANZ Reports
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# bkgd is the color of the page which the plot is to be inserted.
#    if bkgd = "dark" then the legend, axis titles, text and ticks and legend titles and text will be light coloured


ea_style <- function(g, option = "", bkgd = "light") {
  
  
  
  if (bkgd == "light"){
    clr_set = ea_pal[13] # coal
    clr_set_gl = ea_pal[14]
  }  else {
    clr_set = ea_pal[14] # ice
    clr_set_gl = '#2F5395'
    # clr_set_gl = "darkgrey"
    clr_set_gl = '#B2B2B2'
  }
  
  #inherit the colors
  gridline_clr = clr_set_gl
  axis_txt_clr = clr_set
  legd_txt_clr = clr_set
  
  
  g <- g + theme_minimal()
  g <- g + theme(panel.grid = element_blank())
  g <- g + theme(panel.grid.major.x = element_line(color = clr_set_gl, size = .2, linetype = "dashed"))
  g <- g + theme(panel.grid.major.y = element_line(color = clr_set_gl, size = .2, linetype = "dashed"))
  g <- g + theme(axis.title = element_text(color = clr_set))
  g <- g + theme(axis.text = element_text(color = clr_set))
  g <- g + theme(legend.title =  element_text(color = clr_set))
  g <- g + theme(legend.text = element_text(color = clr_set))
  
  if (option == "MNM") {
    g <- g + scale_fill_manual(berl_pal[c(1, 2)])
  }
  
  if (option == "comma.y") {
    g <- g + scale_y_continuous(labels = function(x) format(x, big.mark = ","))
  }
  
  if (option == "comma.x") {
    g <- g + scale_x_continuous(labels = function(x) format(x, big.mark = ","))
  }
  
  
  
  return(g)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ea_bin - a function to return a freq distribution table of variable, the original data with bins and a histogram
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# VAR must be an integer
# binsew is a 3 element vector of integers specifying 
#     [1] the lower limit of the first bin
#     [2] the upper limit of the last bin
#     [3] the bin width
# tag is an optional string that can be appended to the bin labels

ea_bin = function(D, VAR, binsew, tag = ""){
  
  # binsew = c(10,80,10)
  # D = GenderAge
  # VAR = "age"
  # tag = "years"
  
  binstt = binsew[1]
  binenn = binsew[2]
  binwid = binsew[3]
  
  binvec = seq(binstt, binenn, binwid)
  nbins = length(binvec-1)
  binlabs = paste0(binvec[1:nbins],"-",binvec[1:nbins]+binwid-1," ",tag)
  binlabs_df = data.frame(bin_id=1:nbins, binlabs)
  
  
  DOUT = data.frame(VAR = D[[VAR]])  %>% 
    mutate(bin = cut(VAR, binvec, labels=F)) %>% 
    left_join(binlabs_df, by = c("bin"="bin_id"))
  
  DOUT_FD = DOUT %>% 
    group_by(binlabs) %>% 
    summarise(count = length(binlabs))
  
  g = ggplot(DOUT_FD) + geom_bar(aes(binlabs  , count), stat = "identity")
  g = g + coord_flip()
  g = g + labs(x=VAR)
  g
  
  
  OUT = list(DOUT_FD, DOUT, g)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: clip_MLorNML
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




clip_MLorNML <- function(ROHE_curr, MLorNML, ML, PA, CL, rohe_parent_folder, ROHE_name_curr) {
  tic()
  
  ROHExML <- ML %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  ROHExML <- validate_sh(ROHExML)
  
  if (MLorNML == "ML") {
    OUT <- ROHExML
  } else {
    ROHExCL <- CL %>%
      st_intersection(ROHE_curr) %>%
      st_collection_extract("POLYGON")
    ROHExPA <- PA %>%
      st_intersection(ROHE_curr) %>%
      st_collection_extract("POLYGON")
    
    ROHExCL <- validate_sh(ROHExCL)
    ROHExPA <- validate_sh(ROHExPA)
    
    
    ROHExNML <- ROHE_curr %>%
      getLayerDiff(., ROHExML) %>%
      getLayerDiff(., ROHExCL) %>%
      getLayerDiff(., ROHExPA)
    
    ROHExNML <- validate_sh(ROHExNML)
    OUT <- ROHExNML
    toc()
  }
  
  
  # output_ffn = paste0(rohe_parent_folder,ROHE_name_curr,"/ROHEx", MLorNML, ".shp")
  output_ffn <- paste0(rohe_parent_folder, ROHE_name_curr, "/ROHEx", MLorNML, ".gpkg")
  st_write(obj = OUT, dsn = output_ffn, append = T, delete_layer = TRUE)
  
  
  return(OUT)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: preclip_layer_to_rohe
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

preclip_layer_to_rohe <- function(ROHE_curr, Layer, Layer_name, rohe_parent_folder, ROHE_name_curr, parallel = F) {
  tic()
  
  if (Layer_name == "LUM" & "SHAPE_Leng" %in% names(Layer)) {
    Layer <- Layer %>% rename("shp_len" = "SHAPE_Leng")
  }
  
  output_ffn <- paste0(rohe_parent_folder, ROHE_name_curr, "/ROHEx", Layer_name, ".gpkg")
  print("-------------")
  print(paste("Clipping", Layer_name, " to the boundaries of the rohe:", ROHE_name_curr, "and saving to", output_ffn))
  
  tic()
  if (parallel) {
    ROHExLAYER <- st_parallel(Layer, st_intersection, 15, ROHE_curr)
  } else {
    ROHExLAYER <- Layer %>%
      st_intersection(ROHE_curr) %>%
      st_collection_extract("POLYGON") # is the loss of multipolygons going to be a problem?
  }
  toc()
  
  
  st_write(obj = ROHExLAYER, dsn = output_ffn, append = T, delete_layer = TRUE)
  toc()
  return(ROHExLAYER)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: intersect_MLorNML_with_LAYER
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


intersect_MLorNML_with_LAYER <- function(ROHExMLorNML, ROHExLAYER, MLorNML_name, ROHExLAYER_fname, rohe_parent_folder, ROHE_name_curr) {
  tic()
  
  
  # output_ffn = paste0(rohe_folder,ROHE_name_curr,"/", fname, ".shp")
  output_ffn <- paste0(rohe_parent_folder, ROHE_name_curr, "/", ROHExLAYER_fname, ".gpkg")
  
  print("-------------")
  print(paste("Intersecting the", MLorNML_name, "land in the Rohe:", ROHE_name_curr, "with", ROHExLAYER_fname, "to create Layer:", paste0("ROHEx", ROHExMLorNML, "x", ROHExLAYER_fname)))
  
  
  
  
  ROHExMLorNMLxLAYER <- ROHExMLorNML %>%
    st_intersection(ROHExLAYER) %>%
    st_collection_extract("POLYGON") # is the loss of multipolygons going to be a problem?
  
  st_write(obj = ROHExMLorNMLxLAYER, dsn = output_ffn, append = T, delete_layer = TRUE)
  
  
  toc()
  return(ROHExMLorNMLxLAYER)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: clip_seq
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


clip_seq <- function(ROHE_curr, ROHExMLorNML, MLorNML_arg, LAYER, LAYER_name, rohe_parent_folder, ROHE_name_curr) {
  tic()
  
  final_layer_name <- paste0("ROHEx", MLorNML_arg, "x", LAYER_name)
  
  
  # get the ROHExAB by preclipping the layer to the Rohe
  ROHExLAYER <- preclip_layer_to_rohe(ROHE_curr, LAYER, LAYER_name, rohe_parent_folder, ROHE_name_curr)
  
  # get the ROHExMLxAB
  ROHExMLorNMLxLAYER <- intersect_MLorNML_with_LAYER(ROHExMLorNML, ROHExLAYER, MLorNML_arg, LAYER_name, rohe_parent_folder, ROHE_name_curr)
  # intersect_MLorNML_with_LAYER = function(ROHExMLorNML, ROHExLAYER, MLorNML_name, ROHExLAYER_fname, rohe_folder, ROHE_name_curr)
  toc()
  
  
  return(ROHExMLorNMLxLAYER)
}





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: calc_tot_area
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

calc_tot_area <- function(SF) {
  SF_Area <- SF %>%
    mutate(Area = as.numeric(st_area(.)) / 1e4) %>%
    st_set_geometry(NULL) %>%
    summarise(Total_Area = sum(Area)) %>%
    pull(Total_Area)
  
  return(SF_Area)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: getLayerDiff
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


getLayerDiff <- function(L1, L2) {
  tic()
  out <- tryCatch(
    {
      # try first using the st_make_valid
      dL <- ms_erase(st_make_valid(L1), st_make_valid(L2))
      return(dL)
    },
    error = function(cond) {
      
      # or if there is an error dont make valid
      dL2 <- ms_erase(L1, L2)
      return(dL2)
    },
    finally = {
      message("processed the differencing")
    }
  )
  toc()
  return(out)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: plot_land_types
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


plot_land_types <- function(rohe_name, ROHE, MLE, MLC) {
  
  # rohe_name = "Waitaha"
  
  # select a single Rohe
  ROHE_curr <- ROHE %>% filter(ROHE == rohe_name)
  
  # intersect Rohe Boundary with MLE and MLC
  
  ROHExMLE <- MLE %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  ROHExMLC <- MLC %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  
  # print areas
  print(paste("Area of MLE land found in", rohe_name, "=", calc_tot_area(ROHExMLE)))
  print(paste("Area of MLC land found in", rohe_name, "=", calc_tot_area(ROHExMLC)))
  
  # get intersection of these layers
  ROHExMLExMLC <- ROHExMLE %>%
    st_intersection(ROHExMLC) %>%
    st_cast("POLYGON")
  print(paste("Area of MLE that is also MLC", rohe_name, "=", calc_tot_area(ROHExMLExMLC)))
  
  # get difference between ROHExMLExMLC and MLE
  ROHExMLE.NOT.MLC <- getLayerDiff(ROHExMLE, ROHExMLExMLC)
  print(paste("Area of MLE that is NOT MLC", rohe_name, "=", calc_tot_area(ROHExMLE.NOT.MLC)))
  
  rohe_plot_dir <- paste0(gisdir, "rohe_plots_and_data/", rohe_name, "/")
  if (!dir.exists(rohe_plot_dir)) {
    dir.create(rohe_plot_dir, recursive = T)
  }
  
  # save the layers
  st_write(ROHExMLE, paste0(rohe_plot_dir, "ROHExMLE.shp"), append = T)
  st_write(ROHExMLC, paste0(rohe_plot_dir, "ROHExMLC.shp"), append = T)
  st_write(ROHExMLExMLC, paste0(rohe_plot_dir, "ROHExMLExMLC.shp"), append = T)
  st_write(ROHExMLE.NOT.MLC, paste0(rohe_plot_dir, "ROHExMLE.NOT.MLC.shp"), append = T)
  
  # create some plots
  
  # create a plot of the rohe boundary
  g <- ggplot(ROHE_curr) +
    geom_sf(color = "darkblue", fill = "lightblue")
  g <- g + ggtitle(rohe_name)
  g
  ggsave(paste0(rohe_plot_dir, "fig-1a-ROHE_Boundary.png"), g)
  
  # add in the Maori Land Court Land
  g <- g + geom_sf(data = ROHExMLC, fill = "green", show.legend = T)
  g <- g + ggtitle(paste0(rohe_name, " M\u0101ori Land Court Land\n"))
  # g = g + scale_fill_manual(values = c("MLC" = "green", "MEL" = "red"))
  g
  ggsave(paste0(rohe_plot_dir, "fig-1b-ROHExMLC.png"), g)
  
  g <- g + geom_sf(data = ROHExMLE, color = "red", fill = "red", show.legend = T)
  g <- g + ggtitle(paste0(rohe_name, " M\u0101ori Land Court Land and M\u0101ori Entity Land\n"))
  # g = g + scale_fill_manual(values = c("MLC" = "green", "MEL" = "red"))
  # g = g + guide_legend(override.aes = list(linetype = "blank", shape = NA))
  g
  ggsave(paste0(rohe_plot_dir, "fig-1c-ROHExMLC.AND.ROHExMLE.png"), g)
  
  
  g <- g + geom_sf(data = ROHExMLExMLC, color = "yellow", fill = "yellow")
  g <- g + ggtitle(paste0(rohe_name, " - BOTH M\u0101ori Land Court Land \nand Entity-owned Land"))
  g
  ggsave(paste0(rohe_plot_dir, "fig-1d-ROHExMLC.AND.ROHExMLE.png"), g)
  
  g <- ggplot(ROHE_curr) +
    geom_sf(color = "darkblue", fill = "lightblue")
  g <- g + geom_sf(data = ROHExMLE.NOT.MLC, color = "red", fill = "red")
  g <- g + ggtitle(paste0(rohe_name, " - Entity-owned Land \nbut NOT M\u101ori Land Court Land"))
  g
  ggsave(paste0(rohe_plot_dir, "fig-1e-ROHExMLE.NOT.ROHExMLC.png"), g)
  
  g <- ggplot(ROHE_curr) +
    geom_sf(color = "darkblue", fill = "lightblue")
  g <- g + geom_sf(data = ROHExMLE.NOT.MLC, color = "red", fill = "red")
  g <- g + geom_sf(data = ROHExMLC, color = "green", fill = "green")
  g <- g + ggtitle(paste0(rohe_name, " - Entity-owned Land only \nand  M\u101ori Land Court Land Only"))
  g
  ggsave(paste0(rohe_plot_dir, "fig-1f-ROHExMLE.NOT.ROHExMLC.png"), g)
  
  
  
  
  
  
  
  
  
  
  OUT <- list(ROHExMLE, ROHExMLC, ROHExMLExMLC, ROHExMLE.NOT.MLC)
  
  return(OUT)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: do_agribase_analysis
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


do_agribase_analysis <- function(rohe_name, ROHE, MLE, MLC) {
  rohe_name <- "Tairawhiti"
  
  rohe_plot_dir <- paste0(gisdir, "rohe_plots_and_data/", rohe_name, "/")
  
  
  if (!exists("AB")) {
    AB <- luca_load("AB")
  }
  if (!exists("ROHE")) {
    ROHE <- luca_load("ROHE")
  }
  
  # select a single Rohe
  ROHE_curr <- ROHE %>% filter(ROHE == rohe_name)
  
  # read the MLE and MLC (for the current Rohe)
  MLE <- st_read(paste0(rohe_plot_dir, "ROHExMLE.NOT.MLC.shp")) %>% st_collection_extract("POLYGON")
  MLC <- st_read(paste0(rohe_plot_dir, "ROHExMLC.shp")) %>% st_collection_extract("POLYGON")
  
  # intersect Rohe Boundary with MLE and MLC
  ROHExAB <- AB %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  
  # combine the CL and PA
  if (!exists("CL")) {
    CL <- luca_load("CL")
  }
  if (!exists("PA")) {
    PA <- luca_load("PA")
  }
  
  CL_c <- CL %>%
    st_collection_extract("POLYGON") %>%
    summarize(geometry = st_union(geometry)) %>%
    st_make_valid()
  PA_c <- PA %>%
    st_collection_extract("POLYGON") %>%
    summarize(geometry = st_union(geometry)) %>%
    st_make_valid()
  
  
  ROHE.CL <- CL_c %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  ROHE.PA <- PA_c %>%
    st_intersection(ROHE_curr) %>%
    st_collection_extract("POLYGON")
  
  
  
  
  
  # get Rohe - MLC
  TMP_A <- getLayerDiff(ROHE_curr, MLC) # subtract MLC
  TMP_B <- getLayerDiff(TMP_A, MLE) # subtract MLE
  TMP_C <- getLayerDiff(TMP_B, ROHE.CL) # subtract Crown Land
  TMP_D <- getLayerDiff(TMP_C, ROHE.PA) # subtract Protected Areas
  
  NML <- TMP_D
  
  ML <- st_union(st_combine(MLC), st_combine(MLE))
  mapView(ML)
  
  
  NML_ffout <- paste0(rohe_plot_dir, "ROHE_NML.shp")
  st_write(NML, NML_ffout)
  
  mapView(NML)
  
  
  
  return(OUT)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: qm
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


qm <- function(Layer) {
  g <- ggplot(Layer) +
    geom_sf()
  
  return(g)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: qmp
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



qmp <- function(Layer) {
  g <- qm(Layer)
  
  p <- ggplotly(g)
  
  return(p)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   Function: qmv
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



qmv <- function(Layer) {
  m <- mapView(Layer)
  
  return(m)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  rgb2hex - converts and RGB color value to its hex equivalent
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rgb2hex <- function(r, g, b) {
  rgb(r, g, b, maxColorValue = 255)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### BERL style - a style created by LUCA designed for inclusion into BERL Reports
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

berl_style <- function(g, option = "") {
  g <- g + theme_minimal()
  g <- g + theme(panel.grid = element_blank())
  g <- g + theme(panel.grid.major.x = element_line(color = "darkgrey"))
  
  if (option == "MNM") {
    g <- g + scale_fill_manual(berl_pal[c(1, 2)])
  }
  
  if (option == "comma.y") {
    g <- g + scale_y_continuous(labels = function(x) format(x, big.mark = ","))
  }
  
  if (option == "comma.x") {
    g <- g + scale_x_continuous(labels = function(x) format(x, big.mark = ","))
  }
  
  
  return(g)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# hbar_stack function
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

hbar_stack <- function(D, X, Y, FILL, hori_title = "Y", vert_title = "X", pal = NULL, facet = NULL, leg_title = NULL, POS = "stack") {
  g <- ggplot(D, aes_string(X, Y, fill = FILL)) +
    geom_bar(stat = "identity", position = POS)
  
  if (!is.null(facet)) {
    g <- g + facet_grid(facet)
  }
  
  g <- g + labs(y = hori_title, x = vert_title)
  
  g <- g + coord_flip()
  
  if (!is.null(pal) & !is.null(leg_title)) {
    g <- g + scale_fill_manual(name = leg_title, values = pal)
  }
  
  
  
  g <- berl_style(g)
  
  
  g <- g + guides(fill = guide_legend(reverse = TRUE))
  
  
  return(g)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# get area
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_area <- function(D) {
  Area <- D %>%
    mutate(Area = as.numeric(st_area(.) / 1e4)) %>%
    summarise(Area = sum(Area)) %>%
    pull(Area)
  return(Area)
}


#
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ### area_wts_avgs
# #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# area_wts_avgs = function(DF, AREA_VAR, VARS_2_AVG){
#
#   DF = FOREST_AND_LIVESTOCK_EMS_PER_HA
#   AREA_VAR = "LUM_AREA"
#   VARS_2_AVG = c("EMS_DEF" ,"REMOVALS_T_CO2E"  )
#
#   AREA_VAR = DF[[AREA_VAR]]
#   DF %>% select(VARS_2_AVG)
#
#
#   mutate(DF, !!paste0(VARS_2_AVG[1], "_wtd") := AREA_VAR/sum(AREA_VAR))
#
#
#  DF %>%
#     mutate(
#            AREA_FRAC = AREA_VAR/sum(AREA_VAR))
#
#
#            ,
#            !!VARS_2_AVG[1] := VARS_2_AVG[1]  *AREA_FRAC)
#
#
# }





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# save_fig
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 5/1/23  New argument BG makes the figure background transparent or not
#         However, if BG remains NULL then the background of the plot uses the 
#         plot.background fill value from the plot theme.

save_fig <- function(g,
                     fig_dn,
                     fig_fn,
                     fig_fmt = ".png",
                     fig_desc,
                     plot_titles_on,
                     WD = 22,
                     HT = 12,
                     UNITS = "cm",
                     BG = NULL,
                     report_fignum = NA,
                     finalreportfiguresdir,
                     makeplotly = F) {
  fig_ffn <- paste0(fig_dn, fig_fn)
  
  if (plot_titles_on) {
    g <- g + labs(caption = fig_fn)
    g
  }
  
  ggsave(
    filename = fig_ffn,
    plot = g, width = WD, height = HT, units = UNITS, bg = BG
  )
  
  if (!is.na(report_fignum)) {
    figfmt <- ".png"
    fig_fn <- paste0("Fig-", formatC(report_fignum, width = 2, flag = "0"), "-", fig_desc, fig_fmt)
    fig_ffn <- paste0(finalreportfiguresdir, fig_fn)
  }
  
  
  
  # THIS PART BELOW IS NOT WORKING YET
  if (makeplotly) {
    cwd <- getwd()
    
    
    
    # create a plotly directory under current plot directory
    plotly_dn <- paste0(fig_dn, "plotly/")
    if (!dir.exists(plotly_dn)) {
      dir.create(plotly_dn)
    }
    plotly_fn <- paste0(tools::file_path_sans_ext(fig_fn), ".html")
    plotly_fn <- gsub(" ", "_", plotly_fn)
    
    
    plotly_ffn <- paste0(plotly_dn, plotly_fn)
    
    library(htmlwidgets)
    p <- ggplotly(g)
    
    setwd(plotly_dn)
    htmlwidgets::saveWidget(as_widget(p), plotly_fn)
    setwd(plotly_dn)
  }
}









#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# create_change_table
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# creates a data frame showing area of LUC Class i (i = 71-82) Converted to LUC Class j (j= 71-82) over
# 6 phases:
# AB (1990-2008)
# AC (1990-2012)
# AD (1990-2016)
# BC (2008-2012)
# BD (2008-2016)
# CD (2012-2016)

create_change_table <- function(DF, ffout = NULL, phase_select = NULL) {
  class_stt <- seq(71, 82)
  class_enn <- seq(71, 82)
  
  if (is.null(phase_select)) {
    lucas_phase <- c("AB", "AC", "AD", "BC", "BD", "CD")
  } else {
    lucas_phase <- phase_select
  }
  
  
  d1 <- expand.grid(lucas_phase, class_stt, class_enn)
  names(d1) <- c("LUCAS_PHASE", "From_LUC", "To_LUC")
  
  d1 <- d1 %>%
    mutate(AreaChange_ha = 0) %>%
    as_tibble()
  
  nrows_tot <- nrow(d1)
  
  row_count <- 0
  
  prog_seed <- 5
  cat(paste("Creating LUC Change Tablle : Progress : ", 0, "%"))
  
  for (Phase_curr in lucas_phase) {
    for (i in class_stt) {
      for (j in class_enn) {
        row_count <- row_count + 1
        percent_complete <- 100 * row_count / nrows_tot
        
        if (percent_complete >= prog_seed) {
          cat(paste("..", prog_seed, "%.."))
          prog_seed <- prog_seed + 5
        }
        
        
        #
        #       Phase_curr = "AD"
        #       i = 71
        #       j = 71
        
        
        Timepoint_1 <- case_when(
          substr(Phase_curr, 1, 1) == "A" ~ "LUCID_1990",
          substr(Phase_curr, 1, 1) == "B" ~ "LUCID_2008",
          substr(Phase_curr, 1, 1) == "C" ~ "LUCID_2012"
        )
        
        Timepoint_2 <- case_when(
          substr(Phase_curr, 2, 2) == "B" ~ "LUCID_2008",
          substr(Phase_curr, 2, 2) == "C" ~ "LUCID_2012",
          substr(Phase_curr, 2, 2) == "D" ~ "LUCID_2016"
        )
        
        
        AreaChange <- DF %>% filter(get(Timepoint_1) == i & get(Timepoint_2) == j)
        
        if (nrow(AreaChange) > 0) {
          LUC_AREA_CHANGE <- AreaChange %>%
            mutate(Area_LUChange = as.numeric(st_area(.) / 1e4)) %>%
            # group_by(BLOCK_ID) %>%
            summarise(Area_LUChange = sum(Area_LUChange)) %>%
            pull(Area_LUChange)
        } else {
          LUC_AREA_CHANGE <- 0
        }
        
        d1$AreaChange_ha[d1$LUCAS_PHASE == Phase_curr & d1$From_LUC == i & d1$To_LUC == j] <- LUC_AREA_CHANGE
        # print(paste("Change in Land Use between", substr(Timepoint_1,7,11), "and", substr(Timepoint_2,7,11), ": from LUCID",i,"to LUCID",j,"=", round(LUC_AREA_CHANGE),"ha (number of features:", nrow(AreaChange),")"))
      }
    }
  }
  
  
  # order the data frame by LUCAS_PHASE
  
  d1 <- d1 %>% dplyr::arrange(LUCAS_PHASE, From_LUC, To_LUC)
  
  if (!is.null(ffout)) {
    write_csv(d1, Lucas_Change_Table_ffout)
  }
  
  return(d1)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### st_parallel - # Paralise any simple features analysis.
# from https://www.spatialanalytics.co.nz/post/2018/04/01/fixing-st-par/
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



# Paralise any simple features analysis.
st_parallel <- function(sf_df, sf_func, n_cores, os = os, ...) {
  
  # sf_df= SHP
  # n_cores = 15
  # sf_func = st_intersects
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # if (os == "win10"){
  #
  #   # make the  cluster
  #   # n_cores_det <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  #   # n_cores_to_use = n_cores_det - 1
  #   cluster1 <- makeCluster(n_cores)
  #
  #   print(paste("Parallel Processing: created a cluster of ", n_cores,"cores"))
  #
  #
  #   # register the  cluster
  #   registerDoParallel(cluster1)
  #
  #   #split the data
  #   split_input <- split(sf_df, split_vector)
  #
  #
  #   print(paste("Parallel Processing: exporting data to cluster"))
  #
  #   # export the data to the cluster
  #   clusterExport(cluster1,list("sf_df"),envir=environment())
  #
  #
  #
  #   print(paste("Parallel Processing: starting multicore work"))
  #   # execute the command
  #
  #     ANON_FCN = (function(x) sf_func(x))
  #     split_results = c(parLapply(cluster1,split_input, ANON_FCN))
  #
  #
  #
  
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2) {
    output_class <- output_class[2]
  }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix") {
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (output_class == "sfc") {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
  } else if (output_class %in% c("list", "sgbp")) {
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (output_class == "data.frame") {
    result <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}
#
# dnn = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/"
# P1_ffn = paste0(dnn, "ROHE.gpkg")
# P1 = st_read(P1_ffn)
# sf_df = luca_load("ML")
#
#
# library(doParallel)
# library(parallel)
#
# n_cores <- detectCores(logical = TRUE)  # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
#
# # Create a vector to split the data set up by.
# split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
#
# cl <- makeCluster(n_cores-1)
# registerDoParallel(cl)
#
# # clusterExport(cl,list('myfunction','all_samples'))
#
#
# clusterExport(cl,list('P1','sf_df'))
#
# split_results <- split(sf_df, split_vector)
#
#
# system.time(
#   results<- c(parLapply(cl,split_results,st_intersection))
# )
#
#
# system.time(
#   results<- c(parLapply(cl,split_results,st_intersection))
# )
#

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### make_UID -
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

make_UID <- function(ffn, UID_name, var = NULL) {
  
  # ffn ="D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_BERL/rohe.shp"
  # var = "ROHE"
  # UID_name = "ROHE_UID_X"
  D <- st_read(ffn)
  
  if (is.null(var)) {
    UID_data <- 1:nrow(D)
    D[UID_name] <- 1:nrow(D)
  } else {
    V <- D %>%
      pull(var) %>%
      unique()
    UID_data <- 1:length(V)
    df <- data.frame(V, UID_data)
    names(df) <- c(var, UID_name)
    D <- D %>% left_join(df, by = var)
  }
  st_write(D, ffn, append = F)
  
  return(D)
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### rasterize -
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rasterize <- function(FIN, FOUT, ATTR = NULL, OF = "Gtiff", TR = 10, OT = "Float64") {
  tic()
  
  
  # TR = 20
  # OT = "Float64"
  # OF = "Gtiff"
  # FIN = "/media/andrew/HDD data/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.gpkg"
  # FOUT = "/media/andrew/HDD data/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.tif"
  #
  # FIN = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.gpkg"
  # FOUT = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.tif"
  #
  
  # FIN = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/rohe_BERL/rohe.shp"
  # FOUT = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/NZ_Geospatial_Data/rohe_BERL/rohe_rast.tif"
  #
  # file.exists(FIN)
  
  
  
  # FIN = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.gpkg"
  # FOUT = "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/rohe_level_data_sets/Waitaha/ROHE.tif"
  #
  # ATTR = NULL
  
  
  
  if (length(TR) == 1) {
    TR_input <- paste(TR, TR)
  } else if (length(TR) == 2) {
    TR_input <- paste(TR[1], TR[2])
    
    TR <- paste(TR[1], TR[2])
  }
  
  
  
  if (is.null(ATTR)) {
    CMD <- paste0("gdal_rasterize -tr ", TR_input, " -burn 1 -ot ", OT, " \"", FIN, "\"", " ", "\"", FOUT, "\"")
  } else {
    CMD <- paste0("gdal_rasterize -tr ", TR_input, " -a ", ATTR, " -ot ", OT, " \"", FIN, "\"", " ", "\"", FOUT, "\"")
    
    CMD <- paste0("gdal_rasterize -tr ", TR, " -a ", ATTR, " -ot ", OT, " \"", FIN, "\"", " ", "\"", FOUT, "\"")
  }
  
  print(paste("Executing the system command:", CMD))
  system(CMD)
  
  # r = rast(FOUT)
  r <- NA
  toc()
  
  
  r <- rast(FOUT)
  
  return(r)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ea -
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ea_intersection <- function(P1, P2, PC, ffn_stub, dn, val = "none", parallel = T, ncore = 15, filetype = "GPKG", diff_opt = F, simplify_opt = F, tol = NULL) {
  print(paste("#####################################################################"))
  print(paste("#############            Starting interesection              ########"))
  print(paste("#####################################################################"))
  
  
  tic()
  
  if (!dir.exists(dn)) {
    dir.create(dn, recursive = T)
  }
  
  if (!diff_opt) {
    print(paste("####--->   performing a ea_intersection to create", ffn_stub))
  } else {
    print(paste("####--->   performing a ea_difference to create", ffn_stub))
  }
  
  
  print(paste("#####################################################################"))
  
  # P2_ffn = paste0(dnn, "ROHE.gpkg")
  # P2 = st_read(P1_ffn)
  # P1 = luca_load("ML")
  #
  
  if (simplify_opt) {
    if (is.null(tol)) {
      tol <- 20
    }
    
    P1 <- st_simplify(P1, preserveTopology = F, tol)
    P2 <- st_simplify(P2, preserveTopology = F, tol)
  }
  
  
  if (val == "both") {
    print("validating both P1 and P2")
    P1 <- validate_sh(P1)
    P2 <- validate_sh(P2)
  } else if (val == "P1") {
    print("validating P1")
    P1 <- validate_sh(P1)
  } else if (val == "P2") {
    print("validating P2")
    P2 <- validate_sh(P2)
  }
  
  
  
  
  
  if ("fid" %in% names(P1)) {
    P1 <- P1 %>% dplyr::select(-fid)
  }
  if ("fid" %in% names(P2)) {
    P2 <- P2 %>% dplyr::select(-fid)
  }
  
  if ("fid" %in% names(P1)) {
    P1 <- P1 %>% dplyr::select(-fid)
  }
  if ("fid" %in% names(P2)) {
    P2 <- P2 %>% dplyr::select(-fid)
  }
  
  
  
  if (parallel) {
    
    
    
    # make the  cluster
    n_cores_det <- detectCores(logical = TRUE) # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
    n_cores_to_use <- n_cores_det - 1
    cluster1 <- makeCluster(n_cores_to_use)
    
    print(paste("Parallel Processing: created a cluster of ", n_cores_to_use, "cores"))
    
    
    # register the  cluster
    registerDoParallel(cluster1)
    
    # split the data
    split_vector <- rep(1:n_cores_to_use, each = nrow(P1) / n_cores_to_use, length.out = nrow(P1))
    split_input <- split(P1, split_vector)
    
    print(paste("Parallel Processing: exporting data to cluster"))
    
    
    # export the data to the cluster
    clusterExport(cluster1, list("P1", "P2"), envir = environment())
    
    
    
    
    
    
    print(paste("Parallel Processing: exporting data to cluster"))
    
    
    print(paste("Parallel Processing: starting multicore work"))
    # execute the command
    system.time(
      if (!diff_opt) {
        split_results <- c(parLapply(cluster1, split_input, st_intersection, P2))
      } else {
        split_results <- c(parLapply(cluster1, split_input, st_difference, P2))
      }
    )
    print(paste("Parallel Processing: finished multicore work"))
    
    # Define the output_class. If length is greater than two, then grab the second variable.
    output_class <- class(split_results[[1]])
    if (length(output_class) == 2) {
      output_class <- output_class[2]
    }
    
    print(paste("...finished Parallel Processing: recombining results"))
    
    # Combine results back together. Method of combining depends on the output from the function.
    if (output_class == "matrix") {
      P1xP2 <- do.call("rbind", split_results)
      names(P1xP2) <- NULL
    } else if (output_class == "sfc") {
      P1xP2 <- do.call("c", split_results)
      P1xP2 <- sf_func(P1xP2) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
    } else if (output_class %in% c("list", "sgbp")) {
      P1xP2 <- do.call("c", split_results)
      names(P1xP2) <- NULL
    } else if (output_class == "data.frame") {
      P1xP2 <- do.call("rbind", split_results)
    } else {
      stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
    }
  } else if (parallel & PC == "EANZ-DT01-Linux") {
    P1xP2 <- st_parallel(P1, st_intersection, 15, P2)
  } else if (!parallel) {
    P1xP2 <- st_intersection(P1, P2)
  }
  
  # if  (PC == "EANZ-DT01"){
  #
  #   x = mapview(P1xP2)
  #   mapshot(x, url = paste0( dn,ffn_stub, ".html"))
  # }
  
  if ("fid" %in% names(P1xP2)) {
    P1xP2 <- P1xP2 %>% dplyr::select(-fid)
  }
  
  
  
  # shp_ffout = paste0( paste0(dn,ffn_stub, ".gpkg"))
  
  if (filetype == "GPKG") {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".gpkg"))
  } else {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".shp"))
  }
  
  P1xP2 <- P1xP2 %>%
    st_collection_extract("POLYGON") %>%
    validate_sh()
  
  print(paste0("Writing the newly generated shape file to", shp_ffout))
  st_write(P1xP2, shp_ffout, append = F)
  
  print(paste0("...finished luca_intersection..."))
  print(paste0("................................"))
  
  toc()
  
  # close the cluster
  stopCluster(cluster1)
  
  
  print(paste("#####################################################################"))
  print(paste("#############            Finished interesection              ########"))
  print(paste("#####################################################################"))
  
  
  
  return(P1xP2)
}





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### luca_union -
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

luca_union <- function(P1, P2, PC, ffn_stub, dn, val = "none", parallel = T, ncore = 15, filetype = "GPKG") {
  tic()
  
  
  
  
  if (val == "both") {
    print("validating both P1 and P2")
    P1 <- validate_sh(P1)
    P2 <- validate_sh(P2)
  } else if (val == "P1") {
    print("validating P1")
    P1 <- validate_sh(P1)
  } else if (val == "P2") {
    print("validating P2")
    P2 <- validate_sh(P2)
  }
  
  
  
  if ("fid" %in% names(P1)) {
    P1 <- P1 %>% dplyr::select(-fid)
  }
  if ("fid" %in% names(P2)) {
    P2 <- P2 %>% dplyr::select(-fid)
  }
  
  
  
  # make the  cluster
  n_cores_det <- detectCores(logical = TRUE) # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  n_cores_to_use <- n_cores_det - 1
  cluster1 <- makeCluster(n_cores_to_use)
  
  print(paste("Parallel Processing: created a cluster of ", n_cores_to_use, "cores"))
  
  
  # register the  cluster
  registerDoParallel(cluster1)
  
  # split the data
  split_vector <- rep(1:n_cores_to_use, each = nrow(P1) / n_cores_to_use, length.out = nrow(P1))
  split_input <- split(P1, split_vector)
  
  # export the data to the cluster
  clusterExport(cluster1, list("P1", "P2"), envir = environment())
  
  print(paste("Parallel Processing: exporting data to cluster"))
  
  print(paste("Parallel Processing: starting multicore work to create union"))
  # execute the command
  system.time(
    split_results <- c(parLapply(cluster1, split_input, st_union, P2))
  )
  
  print(paste("Parallel Processing: finished multicore work"))
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2) {
    output_class <- output_class[2]
  }
  
  print(paste("...finished Parallel Processing: recombining results"))
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix") {
    P1xP2 <- do.call("rbind", split_results)
    names(P1xP2) <- NULL
  } else if (output_class == "sfc") {
    P1xP2 <- do.call("c", split_results)
    P1xP2 <- sf_func(P1xP2) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
  } else if (output_class %in% c("list", "sgbp")) {
    P1xP2 <- do.call("c", split_results)
    names(P1xP2) <- NULL
  } else if (output_class == "data.frame") {
    P1xP2 <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  
  
  
  
  if (filetype == "GPKG") {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".gpkg"))
  } else {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".shp"))
  }
  
  P1xP2 <- P1xP2 %>%
    st_collection_extract("POLYGON") %>%
    validate_sh()
  
  print(paste0("Writing the newly generated shape file to", shp_ffout))
  st_write(P1xP2, shp_ffout, append = F)
  
  print(paste0("...finished luca_union..."))
  print(paste0("................................"))
  
  toc()
  return(P1xP2)
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### luca_selfdiff - windows only
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

luca_selfdiff <- function(P1, ffn_stub, dn, val = "none", parallel = T, ncore = 15, filetype = "GPKG") {
  tic()
  
  
  
  P1 <- validate_sh(P1)
  
  
  
  
  # make the  cluster
  n_cores_det <- detectCores(logical = TRUE) # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  n_cores_to_use <- n_cores_det - 1
  cluster1 <- makeCluster(n_cores_to_use)
  
  print(paste("Parallel Processing: created a cluster of ", n_cores_to_use, "cores"))
  
  
  # register the  cluster
  registerDoParallel(cluster1)
  
  # split the data
  split_vector <- rep(1:n_cores_to_use, each = nrow(P1) / n_cores_to_use, length.out = nrow(P1))
  split_input <- split(P1, split_vector)
  
  # export the data to the cluster
  clusterExport(cluster1, list("P1"), envir = environment())
  
  print(paste("Parallel Processing: exporting data to cluster"))
  
  print(paste("Parallel Processing: starting multicore work to create self-difference"))
  # execute the command
  system.time(
    split_results <- c(parLapply(cluster1, split_input, st_difference))
  )
  
  print(paste("Parallel Processing: finished multicore work"))
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2) {
    output_class <- output_class[2]
  }
  
  print(paste("...finished Parallel Processing: recombining results"))
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix") {
    RESULT <- do.call("rbind", split_results)
    names(RESULT) <- NULL
  } else if (output_class == "sfc") {
    RESULT <- do.call("c", split_results)
    RESULT <- sf_func(RESULT) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
  } else if (output_class %in% c("list", "sgbp")) {
    RESULT <- do.call("c", split_results)
    names(RESULT) <- NULL
  } else if (output_class == "data.frame") {
    RESULT <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  
  
  
  
  if (filetype == "GPKG") {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".gpkg"))
  } else {
    shp_ffout <- paste0(paste0(dn, ffn_stub, ".shp"))
  }
  
  RESULT <- RESULT %>%
    st_collection_extract("POLYGON") %>%
    validate_sh()
  
  print(paste0("Writing the newly generated shape file to", shp_ffout))
  st_write(RESULT, shp_ffout, append = F)
  
  print(paste0("...finished luca_selfdiff..."))
  print(paste0("................................"))
  
  toc()
  return(RESULT)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### df_tidy_for_plot -
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_df_for_plot <- function(DF, revLO = F, revROHE = F) {
  LP0012_datadir <- "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/"
  
  
  
  
  
  if ("ROHE" %in% names(DF)) {
    ROHE_rs_ffn <- paste0(LP0012_datadir, "ROHE_rs.RDS")
    # saveRDS( ROHE_rs,  ROHE_rs_ffn)
    ROHE_rs <- readRDS(ROHE_rs_ffn)
    
    ROHE_order_with_accents_ffn <- paste0(LP0012_datadir, "ROHE_order_with_accents.RDS")
    # saveRDS( ROHE_order_with_accents,  ROHE_order_with_accents_ffn)
    ROHE_order_with_accents <- readRDS(ROHE_order_with_accents_ffn)
    
    DF <- DF %>% mutate(ROHE = str_replace_all(ROHE, ROHE_rs))
    
    if (length(unique(DF$ROHE)) < 12) {
      DF$ROHE <- factor(DF$ROHE, rev(ROHE_order_with_accents))
    } else {
      
      # this is the condition where National Stats are included in the ROHE too
      
      ROHE_order_with_accents_w_NZ <- c(ROHE_order_with_accents, "Aotearoa (all NZ)")
      DF$ROHE <- factor(DF$ROHE, rev(ROHE_order_with_accents_w_NZ))
    }
    
    if (revROHE) {
      DF$ROHE <- factor(DF$ROHE, levels = rev(levels(DF$ROHE)))
    }
  }
  
  
  
  
  
  
  if ("LO" %in% names(DF)) {
    LO_rs_ffn <- paste0(LP0012_datadir, "LO_rs.RDS")
    
    # saveRDS( LO_rs,  LO_rs_ffn)
    LO_rs <- readRDS(LO_rs_ffn)
    
    LO_order_ffn <- paste0(LP0012_datadir, "LO_order.RDS")
    
    LO_order <- c("Whenua M\u101ori", "General Title")
    # saveRDS( LO_order,  LO_order_ffn)
    
    LO_order <- readRDS(LO_order_ffn)
    
    DF <- DF %>% mutate(LO = str_replace_all(LO, LO_rs))
    DF$LO <- factor(DF$LO, rev(LO_order))
  }
  
  if ("farm_type2" %in% names(DF)) {
    AB_FARM_TYPE_rs_ffn <- paste0(LP0012_datadir, "AB_FARM_TYPE_rs.RDS")
    # saveRDS( LO_rs,  AB_FARM_TYPE_rs_ffn)
    LO_rs <- readRDS(LO_rs_ffn)
    
    DF <- DF %>% mutate(
      farm_type2 = str_replace_all(farm_type2, AB_FARM_TYPE_rs)
    )
    
    
    DF$farm_type2 <- factor(DF$farm_type2, )
  }
  
  if ("LUCNA" %in% names(DF) | "LUCNA_2016" %in% names(DF)) {
    LUCNA_old <- c(
      "Natural Forest",
      "Planted Forest - Pre 1990",
      "Post 1989 Forest",
      "Grassland - With woody biomass",
      "Grassland - High producing",
      "Grassland - Low producing",
      "Cropland - Orchards and vineyards (perennial)",
      "Cropland - Annual",
      "Wetland - Open Water",
      "Wetland - Vegetated non forest",
      "Settlements or built-up area",
      "Other"
    )
    
    LUCNA_new <- c(
      "Pre-1990 natural forest",
      "Pre-1990 planted forest",
      "Post-1989 forest",
      "Grassland - with woody biomass",
      "Grassland - high producing",
      "Grassland - low producing",
      "Cropland - orchards and vineyards (perrenial) ",
      "Cropland - annual",
      "Wetland - open water",
      "Wetland - vegetated non-forest",
      "Settlements or built-up area",
      "Other"
    )
    
    
    LUCNA_rs <- LUCNA_new
    names(LUCNA_rs) <- LUCNA_old
    
    
    
    if ("LUCNA" %in% names(DF)) {
      DF <- DF %>% mutate(
        LUCNA_std = LUCNA,
        LUCNA_std = str_replace_all(LUCNA_std, LUCNA_rs)
      )
    } else {
      DF <- DF %>% mutate(
        LUCNA_std = LUCNA_2016,
        LUCNA_std = str_replace_all(LUCNA_std, LUCNA_rs)
      )
    }
    
    DF$LUCNA_std <- factor(DF$LUCNA_std, levels = LUCNA_new)
  }
  
  
  if (revLO) {
    DF$LO <- factor(DF$LO, levels = rev(levels(DF$LO)))
  }
  
  
  
  return(DF)
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### qload
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# this looks to see whether the argument exists in the current environment. If not then it will read it from disk

qload <- function(DF, dn = LP0012_datadir, extn = ".RDS", tictoc_opt = T) {
  var_name <- deparse(substitute(DF))
  if (!exists(var_name)) {
    fname <- paste0(var_name, extn)
    ffname <- paste0(dn, fname)
    
    print(paste("...> beginning to load", var_name, "From disk (", ffname, ")"))
    
    if (tictoc_opt) {
      tic()
    }
    DF <- readRDS(ffname)
    if (tictoc_opt) {
      toc()
    }
    
    print(paste("...> finished loading", var_name))
  } else {
    print(paste("...> The variable", var_name, "already exists in the environment"))
    DF <- DF
  }
  
  
  return(DF)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### remove_overlaps
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


rev_order <- function(DF, VAR) {
  X <- DF[[VAR]]
  X_levels <- levels(X)
  X_levels_rev <- rev(X_levels)
  DF[[VAR]] <- factor(DF[[VAR]], levels = X_levels_rev)
  
  return(DF)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### remove_overlaps
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

remove_overlaps <- function(SHP) {
  if (class(SHP) == "character") {
    SHP <- st_read(SHP)
  }
  
  setwd("D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/test/")
  
  wd_dn <- paste0(getwd(), "/")
  
  
  SHP <- st_read("POxTB.gpkg")
  
  SHP_sel_1 <- SHP %>% filter(title_no == "WN1300/9")
  SHP_sel_2 <- SHP %>% filter(title_no == "WN1300/11")
  
  SHP$IX <- st_overlaps(SHP, sparse = F)
  
  
  A1 <- SHP %>% slice(1085)
  A2 <- SHP %>% slice(5535)
  A3 <- SHP %>% slice(22846)
  A4 <- SHP %>% slice(22904)
  A5 <- SHP %>% slice(22957)
  
  COLORS <- berl_pal
  
  SHPS <- list(A1, A2, A3, A4, A5)
  
  shp_lst <- 1:5
  
  for (i in shp_lst) {
    if (i == 1) {
      MV_SUM <- mapview(SHPS[[i]], col.regions = COLORS[i])
    } else {
      MV_SUM <- MV_SUM + mapview(SHPS[[i]], col.regions = COLORS[i])
    }
  }
  
  
  MV1 <- mapview(SHPS[[1]], col.regions = COLORS[1])
  MV2 <- mapview(SHPS[[2]], col.regions = COLORS[2])
  MV3 <- mapview(SHPS[[3]], col.regions = COLORS[3])
  MV4 <- mapview(SHPS[[4]], col.regions = COLORS[4])
  MV5 <- mapview(SHPS[[5]], col.regions = COLORS[5])
  
  MV1 + MV2 + MV3 + MV4 + MV5
  
  
  MV_SUM
  OL_NOTSPARSE <- st_overlaps(SHP, sparse = F)
  class(OL_NOTSPARSE)
  dim(OL_NOTSPARSE)
  
  OVER_LAP_ROWS <- which(lapply(OL_SPARSE, function(x) length(x) > 0) %>% unlist())
  
  OL_SPARSE[[1085]]
  
  OL_SPARSE <- st_overlaps(SHP, sparse = T)
  class(OL_SPARSE)
  
  
  IX_DIFF <- st_difference(SHP)
  
  
  
  
  SHP <- st_read("POxTB.gpkg")
  
  SHP_UNQ <- distinct(SHP)
  
  SHP_NO_OVERLAP <- luca_selfdiff(SHP, dn = wd_dn, ffn_stub = "SHP_DIFF") %>% st_collection_extract("POLYGON")
  
  
  SHP_NO_OVERLAP <- st_difference(SHP)
  
  
  SHP_NO_OVERLAP_OVERLAPS <- st_overlaps(SHP_NO_OVERLAP, sparse = T)
  
  
  calc_tot_area(SHP)
  calc_tot_area(SHP_NO_OVERLAP)
  
  
  BOX <- st_read("test_box.shp")
  calc_tot_area(BOX)
  
  
  mapview(BOX) + mapview(SHP_UNQ)
  mapview(BOX) + mapview(SHP_NO_OVERLAP)
  
  
  
  OVER_LAP_ROWS <- which(lapply(SHP_NO_OVERLAP_OVERLAPS, function(x) length(x) > 0) %>% unlist())
  
  OLAPS <- SHP_NO_OVERLAP_OVERLAPS[[1320]]
  
  A1 <- SHP %>% slice(OLAPS[1])
  A2 <- SHP %>% slice(OLAPS[2])
  A3 <- SHP %>% slice(OLAPS[3])
  A4 <- SHP %>% slice(OLAPS[4])
  A5 <- SHP %>% slice(OLAPS[5])
  
  COLORS <- berl_pal
  
  SHPS <- list(A1, A2, A3, A4, A5)
  
  
  calc_tot_area(A1)
  calc_tot_area(A2)
  calc_tot_area(A3)
  calc_tot_area(A4)
  calc_tot_area(A5)
  
  
  MV1 <- mapview(SHPS[[1]], col.regions = COLORS[1])
  MV2 <- mapview(SHPS[[2]], col.regions = COLORS[2])
  MV3 <- mapview(SHPS[[3]], col.regions = COLORS[3])
  MV4 <- mapview(SHPS[[4]], col.regions = COLORS[4])
  MV5 <- mapview(SHPS[[5]], col.regions = COLORS[5])
  
  MV1 + MV2 + MV3 + MV4 + MV5
  
  
  
  
  
  
  IX_DIFF_PARAL$IX_DIFF_CHK <- st_overlaps(IX_DIFF_PARAL, sparse = T) %>% lengths() > 0
  IX_DIFF_PARAL
  
  
  new <- IX_DIFF_PARAL %>% filter(IX_DIFF_CHK)
  mapview(new)
  
  X <- as.matrix(IX_DIFF_CHK)
  
  class(IX_DIFF_CHK)
  IX_DIFF_CHK
  mapview(IX_DIFF)
  
  SHP_sel_101 <- SHP %>% slice(5992)
  SHP_sel_102 <- SHP %>% slice(23587)
  
  
  A <- mapview(SHP_sel_101, col.region = "blue")
  B <- mapview(SHP_sel_102, col.region = "green")
  A + B
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### luca_save_DF
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

luca_save_DF <- function(DF, DF_name, dn = LP0012_datadir) {
  DF_ffn <- paste0(dn, DF_name, ".RDS")
  write_rds(DF, DF_name)
  print(paste("....writing the tibble", DF_name, "to", DF_ffn))
  
  
  return(DF_ffn)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### luca_luca_write_xls
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


luca_write_xls <- function(DF, ffn, sheetname, rowStart, colStart) {
  wb <- loadWorkbook(ffn)
  shtnames <- openxlsx::getSheetNames(ffn)
  
  if (length(which(sheetname %in% shtnames)) == 0) {
    openxlsx::addWorksheet(wb, sheetname)
  }
  
  openxlsx::writeData(x = DF, wb = wb, sheet = sheetname, startCol = colStart, startRow = rowStart, col.names = TRUE)
  saveWorkbook(wb = wb, file = ffn, overwrite = T)
  
  print(paste("Writing data to page", sheetname, "of", ffn))
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_NZU_price
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


get_NZU_price <- function() {
  
  # this data from: https://github.com/theecanmole/nzu
  
  NZU_Price_ffn <- "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/NZU_price/nzu-month-price.csv"
  
  NZU_Price <- read_csv(NZU_Price_ffn)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### calc_livestock_emissions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


calc_livestock_emissions <- function(df, fieldname = NULL, type = NULL) {
  
  # df = AB_ng
  # fieldname = NULL
  # type = NULL
  #
  
  
  
  if (is.null(fieldname) & is.null(type)) {
    mode <- "auto"
  } else {
    mode <- "manual"
  }
  
  
  
  
  # df = LIVESTOCK_EXTRAP
  #
  # fieldname = "bef_tot_GT"
  # type = "bef"
  #
  
  
  
  EF_Table_dn <- "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/MfE-Emission-Factors/"
  
  
  # EF_Table_fn = "MfE-Emission-Factors-v2.xlsx"
  # EF_Table_ffn = paste0(EF_Table_dn, EF_Table_fn)
  
  
  EF_Table_fn <- "MfE-Emission-Factors.xlsx"
  EF_Table_ffn <- paste0(EF_Table_dn, EF_Table_fn)
  
  EF_table <- read_excel(EF_Table_ffn) # THIS TABLE USES UNITS OF kg CO2-e/head
  
  
  
  # add a column that allows it to be linked to the agribase livestock numbers
  EF_table_cln <- EF_table %>%
    mutate(
      sps_key = substr(AB_key, 1, 3),
      EM_CAT = paste0(sps_key, "_", `Emission Type`),
      GHG = GHG_kg_CO2e / 1e3,
      CH4 = CH4_kg_CO2e / 1e3,
      N2O = N2O_kg_CO2e / 1e3,
      CO2 = CO2_kg_CO2e / 1e3,
    ) %>%
    dplyr::select(EM_type = `Emission Type`, sps_key, GHG, CH4, N2O, CO2, EM_CAT)
  
  
  
  
  if (mode == "manual") {
    colnum <- which(names(df) == fieldname)
    
    data <- df %>% pull(colnum)
    
    
    EMS <- as_tibble(data) %>%
      rename(nos = value) %>%
      mutate(
        sps = type,
        ENT_CH4_EMS = nos * (EF_table_cln %>% filter(EM_type == "ENT" & str_detect(sps_key, type))) %>% pull(CH4),
        MMG_CH4_EMS = nos * (EF_table_cln %>% filter(EM_type == "MMG" & str_detect(sps_key, type))) %>% pull(CH4),
        MMG_N2O_EMS = nos * (EF_table_cln %>% filter(EM_type == "MMG" & str_detect(sps_key, type))) %>% pull(N2O),
        AGS_N2O_EMS = nos * (EF_table_cln %>% filter(EM_type == "AGS" & str_detect(sps_key, type))) %>% pull(N2O),
        TOT_CH4_EMS = ENT_CH4_EMS + MMG_CH4_EMS,
        TOT_N2O_EMS = MMG_N2O_EMS + AGS_N2O_EMS,
        TOT_ENT_EMS = ENT_CH4_EMS + MMG_CH4_EMS,
        TOT_MMG_EMS = MMG_CH4_EMS + MMG_N2O_EMS,
        TOT_AGS_EMS = MMG_CH4_EMS,
        TOT_GHG_EMS = TOT_CH4_EMS + TOT_N2O_EMS
      )
    
    
    
    
    
    print("units are t CO2-e")
    df_out <- df %>% bind_cols(EMS)
    return(df_out)
  } else {
    nms <- names(df)
    
    EM_sps <- EF_table_cln %>%
      pull(sps_key) %>%
      unique()
    # EM_type = EF_table_cln %>% pull(EM_type) %>% unique()
    # EM_gases = c("CH4", "N2O", "CO2")
    EM_GAS_TYPES <- c("CH4_ENT", "CH4_MMG", "N2O_MMG", "N2O_AGS")
    
    
    
    for (ix in 1:length(EM_sps)) {
      c_sps <- EM_sps[ix]
      col2_srch <- paste0(c_sps, "_nos")
      
      if (col2_srch %in% nms) {
        c_sps_nos <- df[[col2_srch]]
        
        for (i_EMS_TYPE in EM_GAS_TYPES) {
          
          # i_EMS_TYPE = EM_GAS_TYPES[1]
          c_gas <- substr(i_EMS_TYPE, 1, 3)
          c_type <- substr(i_EMS_TYPE, 5, 7)
          
          c_EF <- EF_table_cln[[c_gas]][EF_table_cln$EM_type == c_type & EF_table_cln$sps_key == c_sps]
          EMS_col_name <- paste0(c_sps, "_", i_EMS_TYPE, "_EMS")
          df[[EMS_col_name]] <- c_sps_nos * c_EF
        }
      }
    }
    
    
    df_w_totals <- df %>%
      mutate(
        dai_EMS_TOT = dai_CH4_ENT_EMS + dai_CH4_MMG_EMS + dai_N2O_MMG_EMS + dai_N2O_AGS_EMS,
        bef_EMS_TOT = bef_CH4_ENT_EMS + bef_CH4_MMG_EMS + bef_N2O_MMG_EMS + bef_N2O_AGS_EMS,
        shp_EMS_TOT = shp_CH4_ENT_EMS + shp_CH4_MMG_EMS + shp_N2O_MMG_EMS + shp_N2O_AGS_EMS
      ) %>%
      rowwise() %>%
      mutate(
        ENT_EMS_TOT = sum(c_across(contains("ENT_EMS"))),
        MMG_EMS_TOT = sum(c_across(contains("MMG_EMS"))),
        AGS_EMS_TOT = sum(c_across(contains("AGS_EMS"))),
        CH4_EMS_TOT = sum(c_across(contains("CH4"))),
        N2O_EMS_TOT = sum(c_across(contains("N2O"))),
        GHG_EMS_TOT = CH4_EMS_TOT + N2O_EMS_TOT
        #
      )
    
    
    return(df_w_totals)
  }
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_LUM_LFH
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_LUM_LFH <- function(LUM) {
  
  # Go through each LUM Polygon and workout the LUM forest scenarios and add this as a field called "Scenario"
  LUM_w_LFH <- LUM %>%
    mutate(Scenario = case_when(
      # conversion from 71 to 73
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 == 71 & LUCID_2016 == 71 ~ 1,
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 == 71 & LUCID_2016 == 73 ~ 2,
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 3,
      LUCID_1990 == 71 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 4,
      # conversion from 71 to 74 or above
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 == 71 & LUCID_2016 >= 74 ~ 5,
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 6,
      LUCID_1990 == 71 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 7,
      # conversion from 71 to a mix of 73 and 74 or above
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 8,
      LUCID_1990 == 71 & LUCID_2008 == 71 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 9,
      LUCID_1990 == 71 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 10,
      LUCID_1990 == 71 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 11,
      LUCID_1990 == 71 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 12,
      LUCID_1990 == 71 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 13,
      LUCID_1990 == 71 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 14,
      LUCID_1990 == 71 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 15,
      
      # conversion from 72 to 73
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 == 72 & LUCID_2016 == 72 ~ 16,
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 == 72 & LUCID_2016 == 73 ~ 17,
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 18,
      LUCID_1990 == 72 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 19,
      # conversion from 72 to 74 or above
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 == 72 & LUCID_2016 >= 74 ~ 20,
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 21,
      LUCID_1990 == 72 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 22,
      # conversion from 72 to a mix of 73 and 74 or above
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 23,
      LUCID_1990 == 72 & LUCID_2008 == 72 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 24,
      LUCID_1990 == 72 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 25,
      LUCID_1990 == 72 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 26,
      LUCID_1990 == 72 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 27,
      LUCID_1990 == 72 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 28,
      LUCID_1990 == 72 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 29,
      LUCID_1990 == 72 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 30,
      
      # conversion from >=74 to 73
      LUCID_1990 >= 74 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 31,
      LUCID_1990 >= 74 & LUCID_2008 >= 74 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 32,
      LUCID_1990 >= 74 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 33,
      LUCID_1990 >= 74 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 == 73 ~ 34,
      # conversion from >= 74 to a mix of 73 and >=74 or above
      LUCID_1990 >= 74 & LUCID_2008 >= 74 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 35,
      LUCID_1990 >= 74 & LUCID_2008 == 73 & LUCID_2012 == 73 & LUCID_2016 >= 74 ~ 36,
      LUCID_1990 >= 74 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 >= 74 ~ 37,
      LUCID_1990 >= 74 & LUCID_2008 == 73 & LUCID_2012 >= 74 & LUCID_2016 == 73 ~ 38,
      TRUE ~ 99
    ))
  
  
  return(LUM_w_LFH)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_forest_type_and_ETS_class
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_forest_type_and_ETS_class <- function(LUM_w_LFH) {
  
  # Use the subclasses to workout the forest type
  LUM_w_LFH_a_CLASS <- LUM_w_LFH %>% mutate(
    LUM_2016_FOREST_CLASS = case_when(
      LUCID_2016 == 72 & SUBID_2016 == 0 ~ "PINRAD",
      LUCID_2016 == 72 & SUBID_2016 == 201 ~ "PINRAD",
      LUCID_2016 == 72 & SUBID_2016 == 202 ~ "DOUFIR",
      LUCID_2016 == 72 & SUBID_2016 == 203 ~ "PINRAD",
      LUCID_2016 == 73 & SUBID_2016 == 0 ~ "PINRAD",
      LUCID_2016 == 73 & SUBID_2016 == 122 ~ "WLDLNG",
      LUCID_2016 == 73 & SUBID_2016 == 201 ~ "PINRAD",
      LUCID_2016 == 73 & SUBID_2016 == 202 ~ "DOUFIR",
      LUCID_2016 == 73 & SUBID_2016 == 203 ~ "PINRAD",
      LUCID_2016 == 73 & SUBID_2016 == 204 ~ "INDFST"
    ),
    LUM_ETS_CLASS = case_when(
      LUCID_2016 == 71 | LUCID_2016 == 72 ~ "p90",
      LUCID_2016 == 73 ~ "P89"
    )
  )
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_LUM_emissions
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_LUM_emissions <- function(LUM) {
  
  
  
  # Read the forest change scenarios (LFH) (ie the LUM Forest Histories of LFH) from a spreadsheet
  LUM_w_LFH <- get_LUM_LFH(LUM)
  
  
  ForestAge_minmax_ffn <- paste0(LP0012_datadir, "Forest_Change_Scenarios.xlsx")
  ForestAge_minmax <- read_excel(ForestAge_minmax_ffn, sheet = "Sheet2", range = "U6:AC44") %>%
    dplyr::select(Scenario, Age_min, Age_max, ETS_CLASS)
  
  # Add the calculated minimum and maximum ages (this gives NA for those scenarios that were not forest in 2016)
  LUM_w_LFH <- LUM_w_LFH %>% left_join(ForestAge_minmax, by = "Scenario")
  
  # Use the subclasses to workout the forest type
  LUM_w_LFH_a_CLASS <- LUM_w_LFH %>% get_forest_type_and_ETS_class()
  
  #
  # mutate(
  # LUM_2016_FOREST_CLASS = case_when(
  #   LUCID_2016 == 72 & SUBID_2016 == 0 ~ "PINRAD",
  #   LUCID_2016 == 72 & SUBID_2016 == 201 ~ "PINRAD",
  #   LUCID_2016 == 72 & SUBID_2016 == 202 ~ "DOUFIR",
  #   LUCID_2016 == 72 & SUBID_2016 == 203 ~ "PINRAD",
  #   LUCID_2016 == 73 & SUBID_2016 == 0 ~ "PINRAD",
  #   LUCID_2016 == 73 & SUBID_2016 == 122 ~ "WLDLNG",
  #   LUCID_2016 == 73 & SUBID_2016 == 201 ~ "PINRAD",
  #   LUCID_2016 == 73 & SUBID_2016 == 202 ~ "DOUFIR",
  #   LUCID_2016 == 73 & SUBID_2016 == 203 ~ "PINRAD",
  #   LUCID_2016 == 73 & SUBID_2016 == 204 ~ "INDFST"),
  # LUM_ETS_CLASS = case_when(
  #   LUCID_2016 == 71 | LUCID_2016 == 72 ~ "p90",
  #   LUCID_2016 == 73 ~ "P89"))
  #
  #
  
  # load the Age_range_table_w_EMS Table which provides emissions as a function of forest age
  # for the ETS Class (ETS_CLASS), the tree species (NEFD_Forest_Class_ETS),
  # the ROHE and the age
  
  
  # NEFD_by_ROHE_w_EMS_ffn = paste0(LP0012_datadir, "NEFD_by_ROHE_w_EMS.RDS")
  # NEFD_by_ROHE_w_EMS = readRDS(NEFD_by_ROHE_w_EMS_ffn)
  #
  
  
  
  
  # attach these Cstocks and Emissions to the LUM
  FCS_w_EMS <- LUM_w_LFH_a_CLASS %>%
    dplyr::select(-Age_min, -Age_max) %>%
    left_join(Age_range_table_w_EMS, by = c(
      "ROHE" = "ROHE",
      "LUM_2016_FOREST_CLASS" = "NEFD_Forest_Class_ETS",
      "Scenario" = "Scenario"
    )) %>%
    mutate(ANN_INC_REM_PER_HA_wtd_avg = ifelse(LUCID_2016 == 71, -1.57, ANN_INC_REM_PER_HA_wtd_avg))
  
  
  # now also calculate MfE Emissions
  
  ## first grab the emission factors
  
  Mfe_EMS_REM_RATES_dn <- LP0012_datadir
  Mfe_EMS_REM_RATES_fn <- "LUM_EMS_REM_LUT.xlsx"
  Mfe_EMS_REM_RATES_ffn <- paste0(Mfe_EMS_REM_RATES_dn, Mfe_EMS_REM_RATES_fn)
  
  EMS_REM_RATES <- read_excel(Mfe_EMS_REM_RATES_ffn, sheet = "Classes_and_Subclasses", range = "B3:M34") %>%
    dplyr::select(LUCSUBID, LUCNA, SUBNA, REM_RATE, EMS_RATE_DEF, EMS_RATE_HVT)
  
  ## then jjoin the emission factors onto the LUM data
  FCS_w_EMS <- FCS_w_EMS %>%
    mutate(LUCSUBID = LUCID_2016 * 10000 + SUBID_2016) %>%
    left_join(EMS_REM_RATES, by = "LUCSUBID") %>%
    mutate(MfE_REMOVALS_T_CO2E = LUM_AREA * REM_RATE)
  
  
  return(FCS_w_EMS)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### reclass_farm_types
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



reclass_farm_types <- function(DF, FT = "farm_type") {
  DF <- DF %>%
    mutate(FT4 = case_when(
      farm_type == "BEF" | farm_type == "SNB" | farm_type == "SHP" | farm_type == "GRA" | farm_type == "DEE" ~ "Sheep, beef, deer, grazing",
      farm_type == "DAI" ~ "Dairy",
      farm_type == "FOR" ~ "Forestry",
      TRUE ~ "Other farm types"
    ))
  
  return(DF)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_clean_farm_ids
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

get_clean_farm_ids <- function() {
  recalc_clean_farm_ids <- F
  
  if (recalc_clean_farm_ids) {
    
    # load the intersection of ROHE/PLP/AB (since the PLP will be differenced from the CL
    ROHExPOxAB_NAT_ffn <- paste0(LP0012_datadir, "ROHExPOxAB_NAT.RDS")
    ROHExPOxAB_NAT <- readRDS(ROHExPOxAB_NAT_ffn)
    
    AB_gt_1ha_ng_ffn <- paste0(LP0012_datadir, "AB_gt_1ha_ng.RDS")
    AB_gt_1ha_ng <- readRDS(AB_gt_1ha_ng_ffn)
    
    farm_ids_gt_1ha <- AB_gt_1ha_ng %>% pull(farm_id)
    
    
    # filter out NA titles
    clean_farm_ids <- ROHExPOxAB_NAT %>%
      filter(!is.na(titles) & !str_detect(owner_surn, "Majesty") & farm_id %in% farm_ids_gt_1ha) %>%
      pull(farm_id) %>%
      unique()
    
    clean_farm_ids_ffn <- paste0(LP0012_datadir, "clean_farm_ids.RDS")
    saveRDS(clean_farm_ids, clean_farm_ids_ffn)
  } else {
    clean_farm_ids_ffn <- paste0(LP0012_datadir, "clean_farm_ids.RDS")
    clean_farm_ids <- readRDS(clean_farm_ids_ffn)
  }
  
  return(clean_farm_ids)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### proj_forward
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

proj_forward <- function() {
  
  # YearStt, YearEnd, Age, LUM_2016_FOREST_CLASS, ROHE, Scenario, method
  
  
  NEFD_by_ROHE_w_EMS
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### calc_simple_MfE_ems_for_LUM
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

calc_simple_MfE_removals_for_LUM <- function(LUM_data) {
  LP0012_datadir <- "D:/LUCA Team/Land Use Capability Assessments Limited/LUCA team site - Documents/LUCA-A/Projects/2021-04-11-LP0012-BERL-GHGMaoriLand/data/"
  
  Mfe_EMS_REM_RATES_dn <- LP0012_datadir
  Mfe_EMS_REM_RATES_fn <- "LUM_EMS_REM_LUT.xlsx"
  Mfe_EMS_REM_RATES_ffn <- paste0(Mfe_EMS_REM_RATES_dn, Mfe_EMS_REM_RATES_fn)
  
  EMS_REM_RATES <- read_excel(Mfe_EMS_REM_RATES_ffn, sheet = "Classes_and_Subclasses", range = "B3:M34") %>%
    dplyr::select(LUCSUBID, LUCNA, SUBNA, REM_RATE, EMS_RATE_DEF, EMS_RATE_HVT)
  
  
  LUM_data_mdf <- LUM_data %>%
    mutate(LUCSUBID_2016 = LUCID_2016 * 1e4 + SUBID_2016) %>%
    left_join(EMS_REM_RATES, by = c("LUCSUBID_2016" = "LUCSUBID")) %>%
    mutate(
      REMOVALS = POLY_AREA * REM_RATE,
      HVT_EMS = POLY_AREA * EMS_RATE_HVT, # this is incorrect because emissions are not annual
      DEF_EMS = POLY_AREA * EMS_RATE_DEF # this is incorrect because emissions are not annual
    )
  
  return(LUM_data_mdf)
}





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### proj_MfE_ems_for_LFE
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

proj_MfE_ems_for_LFH <- function(LUM_w_LFH, method, EMS_REM_RATES = EMS_REM_RATES, Age_range_table_w_EMS = Age_range_table_w_EMS) {
  AGE_at_MAT <- NEFD_by_ROHE_w_EMS %>%
    dplyr::select(ETS_CLASS, NEFD_Forest_Class_ETS, ROHE, AGE_AT_MAT) %>%
    distinct()
  
  
  method <- "MfE"
  LUM_w_LFH <- readRDS(paste0(LP0012_datadir, "ROHExPOxLUM_NAT.RDS"))
  
  # grab the
  LUM_w_LFH <- get_LUM_LFH(LUM)
  
  
  gtt <- function(age, stt_year, enn_year, REM_RATE, EMS_RATE_DEF, EMS_RATE_HVT, AGE_at_MAT) {
    age <- 21
    AGE_at_MAT <- 28
    stt_year <- 2016
    enn_year <- stt_year + 30 - 1
    REM_RATE <- -36.15
    EMS_RATE_HVT <- 587.9
    EMS_RATE_DEF <- 433.2
    AGE_at_MAT <- 28
    
    year_of_mat <- round(stt_year + (AGE_at_MAT - age))
    
    years <- seq(stt_year, enn_year)
    
    dout <- data.frame(YEAR = years) %>%
      as_tibble() %>%
      mutate(
        REM_RATE = REM_RATE,
        HVT_EMS = ifelse(YEAR == year_of_mat, EMS_RATE_HVT, 0),
        DEF_EMS = ifelse(YEAR == year_of_mat, EMS_RATE_DEF, 0),
        REM_PLUS_HVT = REM_RATE + HVT_EMS,
        REM_PLUS_DEF = REM_RATE + DEF_EMS,
        REM_PLUS_HVT_CUM = cumsum(REM_PLUS_HVT)
      )
    
    # c_inc = REM_RATE
    
    
    
    
    
    
    
    
    
    
    out <- list(c(age, age + 1, age + 1))
  }
  
  
  
  
  if (method == "MfE") {
    LUM_w_LFH_tmp <- LUM_w_LFH %>%
      # assign a LUCSUBID
      mutate(LUCSUBID_2016 = LUCID_2016 * 10000 + SUBID_2016) %>%
      # assign a MfE emission rate
      left_join(EMS_REM_RATES, by = c("LUCSUBID_2016" = "LUCSUBID")) %>%
      # assign an LFH
      get_LUM_LFH() %>%
      # get forest type and ETS class
      get_forest_type_and_ETS_class() %>%
      # join on the age
      left_join(Age_range_table_w_EMS, by = c(
        "ROHE" = "ROHE",
        "LUM_2016_FOREST_CLASS" = "NEFD_Forest_Class_ETS",
        "Scenario" = "Scenario"
      )) %>%
      # add the age of the forest at maturity
      left_join(AGE_at_MAT, by = c(
        "ROHE" = "ROHE",
        "LUM_2016_FOREST_CLASS" = "NEFD_Forest_Class_ETS",
        "ETS_CLASS" = "ETS_CLASS"
      )) %>%
      mutate(ANN_INC_REM_PER_HA_wtd_avg = ifelse(LUCID_2016 == 71, -1.57, ANN_INC_REM_PER_HA_wtd_avg)) %>%
      mutate(TS = gtt(Age_wtd_avg))
  }
}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### rohe_heat_map
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rohe_heat_map <- function(DF, var2plot, varname, plot_fn = NULL, plot_dn = NULL) {
  
  # DF = NEFD_TABLES_2018_smy_PINRAD
  # var2plot = "wtd_avg_age"
  # varname = "Area-weighted\naverage age\n(years)"
  # plot_dn = NEFD_plotdir
  # plot_fn = "PINRAD_cmb_avg_age.png"
  
  
  if (!exists("ROHE_smy")) {
    ROHE <- luca_load("ROHE")
    ROHE_smy <- ROHE %>%
      group_by(ROHE) %>%
      summarise()
  }
  
  
  if (is.null(plot_dn)) {
    plot_dn <- paste0(plotdir, "rohe_heat_maps/")
  }
  if (is.null(plot_fn)) {
    plot_fn <- paste0("heat_map_", strftime(Sys.time(), format = "%Y-%m-%d-%H%M.png"))
  }
  
  plot_ffn <- paste0(plot_dn, plot_fn)
  
  
  ROHE_smy_w_data <- ROHE_smy %>%
    left_join(DF, by = "ROHE") %>%
    dplyr::select(ROHE, geometry, var2plot2 = var2plot)
  
  
  
  
  g <- ggplot(data = ROHE_smy_w_data) +
    # geom_sf() +
    geom_sf(aes(fill = var2plot2)) +
    scale_fill_continuous(name = varname, labels = scales::comma)
  
  g <- g + theme_minimal() + theme(axis.text = element_blank(), panel.grid = element_blank(), legend.position = c(0.15, 0.6))
  g <- g + guides(color = "colorsteps")
  g
  
  ggsave(filename = plot_ffn, g)
  
  print(paste("Saved Rohe Heat Map as", plot_ffn))
  
  return(g)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### rohe_heat_map
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

add_NZ_to_plot <- function(DF) {
  
  # DF = ROHE_NCLxPLPxML_and_GTxLUM_P89_stats_ROHE_LO_PERCENT_OF_P89_TYPE %>% select(ROHE, LO, AgeRange, POLY_AREA)
  
  
  names_orig <- names(DF)
  names(DF) <- c("ROHE", "LO", "CLASS", "VAR")
  
  DF_NZ_LO_PRPN <- DF %>%
    ungroup() %>%
    group_by(LO, CLASS) %>%
    summarise(VAR = sum(VAR)) %>%
    ungroup() %>%
    group_by(LO) %>%
    mutate(
      VAR_PERC = 100 * VAR / sum(VAR),
      ROHE = "Aotearoa (all NZ)"
    )
  
  
  DF_ROHE_LO_PRPNS <- DF %>%
    ungroup() %>%
    group_by(ROHE, LO) %>%
    # summarise(CLASS = first(CLASS),
    #           VAR = sum(VAR)) %>%
    mutate(VAR_PERC = 100 * VAR / sum(VAR)) %>%
    bind_rows(DF_NZ_LO_PRPN) %>%
    tidy_df_for_plot(revLO = T)
  
  
  
  
  names(DF_ROHE_LO_PRPNS) <- c(names_orig, "VAR_PERC")
  
  
  return(DF_ROHE_LO_PRPNS)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### qroots - a quadratic root solver
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


qroots <- function(a, b, c) {
  x1 <- -b + sqrt(b^2 - 4 * a * c) / (2 * a)
  x2 <- -b - sqrt(b^2 - 4 * a * c) / (2 * a)
  
  roots <- c(x1, x2)
  return(roots)
}





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### make_qparm - creates a quadratic curve fitted to each of the input points
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


make_qparm <- function(xp = 0:40, xin = c(0, 20, 40), yin = c(.4, .6, .2), plotopt = T, plotlims = c(0, 1)) {
  
  # xp = 0:40
  # xin = c(0,20, 40)
  # yin = c(.4,.6,.2)
  # plotopt = T
  # plotlims = c(0,1)
  #
  
  # yin = c(.4,.6,.2)
  df <- data.frame(xin = xin, yin = yin)
  
  df$xin2 <- df$xin^2
  
  LM <- lm(yin ~ xin + xin2, data = df)
  LM_smy <- summary(LM)
  
  a <- LM_smy$coefficients[3, 1]
  b <- LM_smy$coefficients[2, 1]
  c <- LM_smy$coefficients[1, 1]
  
  coeffs <- c(a, b, c)
  
  df_pred <- data.frame(xp) %>% mutate(yp = a * xp^2 + b * xp + c)
  
  
  if (plotopt) {
    g <- ggplot(df_pred, aes(xp, yp)) +
      geom_line() +
      geom_point(data = df, aes(xin, yin), color = "red") +
      lims(y = plotlims)
    g
  }
  
  roots <- qroots(a, b, c)
  
  out <- list(df_pred$yp, g, coeffs, roots)
  
  return(out)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### get_TA_data - reads a sf dataset from the TA folders
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



get_TA_data = function(TA_id, varname){
  
  
  # TA_id = 2
  # TA_id = "Wai"
  
  
  if (!exists("TA")){TA = ea_load("TA")}
  TA_names = TA %>% pull(TA2019_V_1) %>% unique()
  
  if (is.numeric(TA_id)){
    TA_name_curr = TA_names[TA_id]
  } else {
    
    ix = which(str_detect(tolower(TA_names), tolower(TA_id)))
    
    if (length(ix) == 1){
      TA_name_curr = TA_names[ix]
      print(paste("you selected", TA_name_curr))
    }else{
      TAs_selected = TA_names[ix]
      TAx_selected_ix = (1:length(TA_names))[ix]
      TAs_selected_df = data.frame(
        TA.no = 1:length(ix), 
        TA.name = TAs_selected, 
        TA_num = TAx_selected_ix)
      print(paste("your query pulled up more than one council:"))
      print(TAs_selected_df)
      print("select which one you want (by TA.no)")
      ix_sel_chr = readkey()
      ix_sel_num = as.numeric(ix_sel_chr)
      TA_name_curr = TAs_selected_df$TA.name[ix_sel_num]
      print(paste("you selected", TA_name_curr))
    }
    
    
    
    
    
    
  }
  
  TA_poly_curr = TA[TA$TA2019_V_1 == TA_name_curr,]
  TA_folder_curr = paste0(TA_parent_folder, TA_name_curr,'/')
  TA_plotdir = paste0(plotdir, TA_name_curr,"/")
  
  var_ffn = paste0(TA_folder_curr, varname, ".gpkg")
  
  VAR = st_read(var_ffn)
  
  
  
  return(VAR)
  
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### readkey - waits for user input
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### cf_save_points - Takes a cf_station object from clifro and saves it to a shape file
###
###  This is because there is an error with cf_save_kml
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


cf_save_points <- function(sf_obj, ffn, fmt = "shp", showmap = F)
{
  # sf_obj = curr_wstns
  # ffn = "E:/OneDrive - Environmental Analytics/ea/EANZ-Projects/Development-Projects/EADP0009-22-09-19-EC-advice-for_NRC/GIS/test.shp"
  # 
  
  stns_df = data.frame(
    name = curr_wstns$name,
    network = curr_wstns$network,
    agent = curr_wstns$agent,
    start = curr_wstns$start,
    end = curr_wstns$end,
    open = curr_wstns$open,
    distance = curr_wstns$distance,
    lat = curr_wstns$lat,
    lon = curr_wstns$lon) %>% as_tibble()
  
  
  stns_pts = st_as_sf(stns_df, coords = c("lon","lat"), crs = 4326)
  st_write(stns_pts, ffn, append = F)
  
  print(paste("Saved the Station data to the file", ffn))
  
  if (showmap){
    MAP = mapview(stns_pts, label = 'name')
    return(list(stns_pts, MAP))
    
  } else {
    
    return(stns_pts)
    
  }
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ea_read_csv - reads a csv file with column names starting on an arbitrary row and the data starting on an arbitrary row
###
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ea_read_csv = function(csv_ffn, hline = 1, dline = 2, uline = NULL){
  
  #get headers
  H = read_csv(csv_ffn, skip=hline-1, n_max = 1, col_names = T ) %>% names()
  
  #get data
  D = read_csv(csv_ffn, skip=dline-1, col_names = F )
  
  # add headers to data
  names(D) <-H
  
  # if non-NULL value for the uline argument is passed (ie, the row numbers that the units are on)
  # then instead of returning just the data table, a list is returned, with the first element being 
  # the data tabkle and the 2nd elemment being the character array of units
  
  if (!is.null(uline)){
    U = read_csv(csv_ffn, skip=uline-1, n_max = 1, col_names = T ) %>% names()
    D = list(D,U)
  }
  
  return(D)
  
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ea_read_csv - reads a csv file with column names starting on an arbitrary row and the data starting on an arbitrary row
###
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


create_parallel_lines = function(LL = c(1000000, 4700000), UR = c(2100000, 6200000), x_interv = 1e4, y_interv=1e4, direction = "H"){
  # create points data
  # 
  # LL = c(1000000, 4700000)
  # UR = c(2100000, 6200000)
  # x_interv = 10000
  # y_interv = 10000
  
  LL_x = LL[1]
  LL_y = LL[2]
  
  UR_x = UR[1]
  UR_y = UR[2]
  
  
  X_divs = seq(LL_x, UR_x, by=x_interv)
  Y_divs = seq(LL_y, UR_y, by=y_interv)
  
  XY = expand.grid(X_divs, Y_divs) 
  
  m = XY[XY[,1] == LL_x | XY[,1] == UR_x,] %>% as.matrix() %>% as.data.frame()
  
  NR=nrow(m)
  m$grp=sort(rep(1:(NR/2),2))
  
  # 
  # class(m)
  # size(m)
  # 
  
  
  hLines <- data.frame(
    x = m$Var1,
    y = m$Var2,
    attr = m$grp
  ) %>% 
    sf::st_as_sf(coords = c("x","y")) %>% 
    sf::st_set_crs(2193) %>% 
    group_by(attr) %>% 
    summarise(mean(attr)) %>% 
    st_cast("LINESTRING")
  

  CL = st_read("D:/HerronK/ELF_Project/AndrewsWork/Layers/NZ_Coastline_properpolygon.shp") %>%   st_transform(crs = st_crs(hLines))
  # D:\HerronK\ELF_Project\AndrewsWork\layers
  hLinesNZ = st_intersection(hLines, CL)
  
  mapview(hLinesNZ)
  
  return(hLinesNZ)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### st_parallel - parallises a given st function
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


st_parallel = function(sf_df, sf_func, n_cores = NULL, ...){
  
  n_cores_det <- detectCores(logical = TRUE) # returns the number of available hardware threads, and if it is FALSE, returns the number of physical cores
  n_cores_to_use <- n_cores_det - 1
  
  cluster1 <- makeCluster(n_cores_to_use)
  
  print(paste("Parallel Processing: created a cluster of ", n_cores_to_use, "cores"))
  
  
  # register the  cluster
  registerDoParallel(cluster1)
  
  # split the data
  split_vector <- rep(1:n_cores_to_use, each = nrow(P1) / n_cores_to_use, length.out = nrow(P1))
  split_input <- split(P1, split_vector)
  
  
  
  
  
  # export the data to the cluster
  print(paste("Parallel Processing: exporting data to cluster"))
  clusterExport(cluster1, list("P1"), envir = environment())
  
  print(paste("Parallel Processing: doing analysis"))
  split_results <- c(parLapply(cluster1, split_input, sf_func))
  
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2) {
    output_class <- output_class[2]
  }
  
  print(paste("...finished Parallel Processing: recombining results"))
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix") {
    P1_RESULT <- do.call("rbind", split_results)
    names(P1_RESULT) <- NULL
  } else if (output_class == "sfc") {
    P1_RESULT <- do.call("c", split_results)
    P1_RESULT <- sf_func(P1_RESULT) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions.
  } else if (output_class %in% c("list", "sgbp")) {
    P1_RESULT <- do.call("c", split_results)
    names(P1_RESULT) <- NULL
  } else if (output_class == "data.frame") {
    P1_RESULT <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  return(P1_RESULT)
  
}


