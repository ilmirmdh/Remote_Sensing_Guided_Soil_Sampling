# Load Packages
library(boot)
library(MASS)
library(rcompanion)
library(nortest)

# Removing Outliers for Each Covariates/Variables (Using IQR Method)
# ELEVATION
ELEVATION <- quantile(SPECTRAL_FIX_BARE$Elevation, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(SPECTRAL_FIX_BARE$Elevation)

Lower <- ELEVATION[1] - 1.5*IQR
Upper <- ELEVATION[2] + 1.5*IQR 

T1 <- subset(SPECTRAL_FIX_BARE, SPECTRAL_FIX_BARE$Elevation > Lower & SPECTRAL_FIX_BARE$Elevation < Upper)


# SLOPE
SLOPE <- quantile(T1$SLOPE, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T1$SLOPE)

Lower <- SLOPE[1] - 1.5*IQR
Upper <- SLOPE[2] + 1.5*IQR 

T2 <- subset(T1, T1$SLOPE > Lower & T1$SLOPE < Upper)


# ASPECT
ASPECT <- quantile(T2$ASPECT, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T2$ASPECT)

Lower <- ASPECT[1] - 1.5*IQR
Upper <- ASPECT[2] + 1.5*IQR 

T3 <- subset(T2, T2$ASPECT > Lower & T2$ASPECT < Upper)


# CTI
CTI <- quantile(T3$CTI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T3$CTI)

Lower <- CTI[1] - 1.5*IQR
Upper <- CTI[2] + 1.5*IQR 

T4 <- subset(T3, T3$CTI > Lower & T3$CTI < Upper)


# TPI
TPI <- quantile(T4$TPI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T4$TPI)

Lower <- TPI[1] - 1.5*IQR
Upper <- TPI[2] + 1.5*IQR 

T5 <- subset(T4, T4$TPI > Lower & T4$TPI < Upper)


# VDEPTH
VDEPTH <- quantile(T5$VDEPTH, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T5$VDEPTH)

Lower <- VDEPTH[1] - 1.5*IQR
Upper <- VDEPTH[2] + 1.5*IQR 

T6 <- subset(T5, T5$VDEPTH > Lower & T5$VDEPTH < Upper)


# TRI
TRI <- quantile(T6$TRI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T6$TRI)

Lower <- TRI[1] - 1.5*IQR
Upper <- TRI[2] + 1.5*IQR 

T7 <- subset(T6, T6$TRI > Lower & T6$TRI < Upper)


# LSFACTOR
LSFactor <- quantile(T7$LSFactor, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T7$LSFactor)

Lower <- LSFactor[1] - 1.5*IQR
Upper <- LSFactor[2] + 1.5*IQR 

T8 <- subset(T7, T7$LSFactor > Lower & T7$LSFactor < Upper)


# LAI
LAI <- quantile(T8$LAI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T8$LAI)

Lower <- LAI[1] - 1.5*IQR
Upper <- LAI[2] + 1.5*IQR 

T9 <- subset(T8, T8$LAI > Lower & T8$LAI < Upper)


# NDVI
NDVI <- quantile(T9$NDVI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T9$NDVI)

Lower <- NDVI[1] - 1.5*IQR
Upper <- NDVI[2] + 1.5*IQR 

T10 <- subset(T9, T9$NDVI > Lower & T9$NDVI < Upper)


# CARI
CARI <- quantile(T10$CARI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T10$CARI)

Lower <- CARI[1] - 1.5*IQR
Upper <- CARI[2] + 1.5*IQR 

T11 <- subset(T10, T10$CARI > Lower & T10$CARI < Upper)


# NDBI
NDBI <- quantile(T11$NDBI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T11$NDBI)

Lower <- NDBI[1] - 1.5*IQR
Upper <- NDBI[2] + 1.5*IQR 

T12 <- subset(T11, T11$NDBI > Lower & T11$NDBI < Upper)


# BSI
BSI <- quantile(T12$BSI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T12$BSI)

Lower <- BSI[1] - 1.5*IQR
Upper <- BSI[2] + 1.5*IQR 

T13 <- subset(T12, T12$BSI > Lower & T12$BSI < Upper)


# CFaktor
CFaktor <- quantile(T13$CFaktor, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T13$CFaktor)

Lower <- CFaktor[1] - 1.5*IQR
Upper <- CFaktor[2] + 1.5*IQR 

T14 <- subset(T13, T13$CFaktor > Lower & T13$CFaktor < Upper)


# SCRI
SCRI <- quantile(T14$SCRI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T14$SCRI)

Lower <- SCRI[1] - 1.5*IQR
Upper <- SCRI[2] + 1.5*IQR 

T15 <- subset(T14, T14$SCRI > Lower & T14$SCRI < Upper)


# SMI
SMI <- quantile(T15$SMI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T15$SMI)

Lower <- SMI[1] - 1.5*IQR
Upper <- SMI[2] + 1.5*IQR 

T16 <- subset(T15, T15$SMI > Lower & T15$SMI < Upper)


# SBR
SBR <- quantile(T16$SBR, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T16$SBR)

Lower <- SBR[1] - 1.5*IQR
Upper <- SBR[2] + 1.5*IQR 

T17 <- subset(T16, T16$SBR > Lower & T16$SBR < Upper)


# CI
CI <- quantile(T17$CI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T17$CI)

Lower <- CI[1] - 1.5*IQR
Upper <- CI[2] + 1.5*IQR 

T18 <- subset(T17, T17$CI > Lower & T17$CI < Upper)


# NISI
NISI <- quantile(T18$NISI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T18$NISI)

Lower <- NISI[1] - 1.5*IQR
Upper <- NISI[2] + 1.5*IQR 

T19 <- subset(T18, T18$NISI > Lower & T18$NISI < Upper)


# NDTI
NDTI <- quantile(T19$NDTI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T19$NDTI)

Lower <- NDTI[1] - 1.5*IQR
Upper <- NDTI[2] + 1.5*IQR 

T20 <- subset(T19, T19$NDTI > Lower & T19$NDTI < Upper)


# NBR
NBR <- quantile(T20$NBR, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T20$NBR)

Lower <- NBR[1] - 1.5*IQR
Upper <- NBR[2] + 1.5*IQR 

T21 <- subset(T20, T20$NBR > Lower & T20$NBR < Upper)


# FOX
FOX <- quantile(T21$FOX, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T21$FOX)

Lower <- FOX[1] - 1.5*IQR
Upper <- FOX[2] + 1.5*IQR 

T22 <- subset(T21, T21$FOX > Lower & T21$FOX < Upper)


# FRI
FRI <- quantile(T22$FRI, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T22$FRI)

Lower <- FRI[1] - 1.5*IQR
Upper <- FRI[2] + 1.5*IQR 

T23 <- subset(T22, T22$FRI > Lower & T22$FRI < Upper)


# FRO
FRO <- quantile(T23$FRO, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T23$FRO)

Lower <- FRO[1] - 1.5*IQR
Upper <- FRO[2] + 1.5*IQR 

T24 <- subset(T23, T23$FRO > Lower & T23$FRO < Upper)


# GOS
GOS <- quantile(T24$GOS, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T24$GOS)

Lower <- GOS[1] - 1.5*IQR
Upper <- GOS[2] + 1.5*IQR 

T25 <- subset(T24, T24$GOS > Lower & T24$GOS < Upper)


# LAT
LAT <- quantile(T25$LAT, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T25$LAT)

Lower <- LAT[1] - 1.5*IQR
Upper <- LAT[2] + 1.5*IQR 

T26 <- subset(T25, T25$LAT > Lower & T25$LAT < Upper)


# LST
LST <- quantile(T26$LST, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T26$LST)

Lower <- LST[1] - 1.5*IQR
Upper <- LST[2] + 1.5*IQR 

T27 <- subset(T26, T26$LST > Lower & T26$LST < Upper)


# CH
CH <- quantile(T27$CH, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(T27$CH)

Lower <- CH[1] - 1.5*IQR
Upper <- CH[2] + 1.5*IQR 

T28 <- subset(T27, T27$CH > Lower & T27$CH < Upper)

# Normality Test
plotNormalHistogram(T28$LST)
plotNormalHistogram(SPECTRAL_FIX_BARE$LST)
qqnorm(SPECTRAL_FIX_BARE$LST, pch = 1, frame = FALSE)
qqline(T17$SLOPE, col = "steelblue", lwd = 2)
qqnorm(DATA_INPUT_BR$SLOPE, pch = 1, frame = FALSE)
qqline(DATA_INPUT_BR$SLOPE, col = "steelblue", lwd = 2)
ks.test(SPECTRAL_FIX_BARE$NDVI, "pnorm")
ks.test(T28$NDVI, "pnorm")
