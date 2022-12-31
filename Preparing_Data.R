# Removing NaN Data
names(SPECTRAL_BARE)[names(SPECTRAL_BARE) == "grid_code"] <- "NDVI"
names(SPECTRAL_BARE)[names(SPECTRAL_BARE) == "MSMMI"] <- "SMI"

SAMPLE_BARE <- SPECTRAL_BARE[,-c(1,12,6)]
col_order <- c("Elevation", "SLOPE", "ASPECT", "CTI", "TPI", "VDEPTH", "TRI", "LSFactor",
               "LAI", "NDVI", "CARI", "NDBI", "BSI",
               "CFaktor", "SCRI", "SMI", "SBR", "CI", "NISI", "NDTI", "NBR",
               "FOX", "FRI", "FRO", "GOS", "LAT",
               "LST", "CH",
               "POINT_X", "POINT_Y")
DATA_BARE <- SAMPLE_BARE[, col_order]

SPECTRAL_SAMPLE_BARE <- DATA_BARE[!(DATA_BARE$CARI=="-9999"|
                                      DATA_BARE$CTI=="-9999"|
                                      DATA_BARE$SLOPE=="-9999"|
                                      DATA_BARE$ASPECT=="-9999"|
                                      DATA_BARE$TPI=="-9999"),]

SPECTRAL_DATA_BARE <- SPECTRAL_SAMPLE_BARE[complete.cases(SPECTRAL_SAMPLE_BARE),]
SPECTRAL_PLOT_BARE <- subset(SPECTRAL_DATA_BARE, ASPECT > 0)
SPECTRAL_FIX_BARE <- subset(SPECTRAL_PLOT_BARE, VDEPTH > 0)