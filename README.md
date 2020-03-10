# Homogenization-De-Bilt
Homogenization of De Bilt using Eelde as a reference

This directory contains the following files:

1. Readme file.

2. Input data file 'Eelde.txt'. 
   Containing the standardised daily data of Eelde (1906-1970,
   Tg, Tn, Tx) (Eelde = 4).
   
3. Input data file 'DeBilt_prehom.txt'.
   Containing the daily data of De Bilt (1906-2015, Tg, Tn, Tx) prior to homogenisation 
   in the KNMI database.
   
4. Output file 'DeBiltTg_hom_v0.txt'.
   Containing the daily non-homogenized and homogenized Tg data of De Bilt (1901-2015), Tg, T.hom. 
   
5. Output file 'DeBiltTn_hom_v0.txt'.
   Containing the daily non-homogenized and homogenized Tn data of De Bilt (1901-2015), Tn, T.hom. 

6. Output file 'DeBiltTx_hom_v0.txt'.
   Containing the daily non-homogenized and homogenized Tx data of De Bilt (1901-2015), Tx, T.hom.    
   
5. R-code file 'hom_DeBilt.R.
   Containing the R-code use for homogenizing De Bilt using Eelde as a reference. 
   
The R-code runs with standard R libraries. It is assumed that data and code 
are in the working directory. Running the code with the given input
files produces the given output file. In lines 14-15 of the code a selection between Tg, Tn, or Tx must 
made. 
