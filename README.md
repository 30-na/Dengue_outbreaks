# Global risk of Dengue outbreaks and the impact of El Niño events


--------------------------
# Dataset:

1. [Global Aedes Distribution](https://www.dropbox.com/sh/bpxcmzmmpiiav8u/AAAl3CBKnBYwXb0n1s1C4-K-a?dl=0). 
Uncertainty estimates for mosquito distribution at 5 km x 5 km resolution (R datafiles and Aedes maps)
(Kraemer et al., 2015)

2. [Geolocalized Economic Data](https://gecon.yale.edu/data-and-documentation-g-econ-project).
Geophysically scaled dataset linking per capita gross product (GDP) at purchasing power parity (PPP) rates. The G-Econ project is gridded data set at a 1 degree longitude by 1 degree latitude resolution. This is approximately 100 km by 100 km.  

3. [CRU TS4.06: Climatic Research Unit (CRU) Time-Series (TS) version 4.06 of high-resolution gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2021)](https://catalogue.ceda.ac.uk/uuid/e0b4e1e56c1c4460b796073a31366980)   
The gridded Climatic Research Unit (CRU) Time-series (TS) data version 4.06 data are month-by-month variations in climate over the period 1901-2021, provided on high-resolution (0.5x0.5 degree) grids, produced by CRU at the University of East Anglia and funded by the UK National Centre for Atmospheric Science (NCAS), a NERC collaborative centre.
--------------------------
# Missing values
<!-- We replace Somalia PPP2005_40 value with the lowest Djibouti PPP2005_40 value in Geolocalized Economic Data -->  
Replacing G-Econ data for countries with NA values in 2005 (PPP2005_40) with the corresponding values from 1990 (PPP1990_40).   

# Acknowledgments: 
We would like to thank the UNM Center for Advanced Research Computing, supported in part by the National Science Foundation, for providing the high performance computing and large-scale storage resources used in this work.




