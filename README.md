# College ROI
This repo contains code to analyze U.S. Department of Education College Scorecard data.
# Setup
In order to work with these files, create a local "raw_data" subdirectory in the same folder as the "college-roi" repository folder. Add all unzipped raw data files from the College Scorecard data page, at https://collegescorecard.ed.gov/data/.

>[!NOTE]
> - The crosswalks found in the input folder were created using the documentation from the College Scorecard data page.
> - The CPS series used is the R-CPI-U-RS (without food and energy) series, available [here](https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm)
> - The College Scorecard 2024 release included a [data errata](https://collegescorecard.ed.gov/assets/EarningsDataErrata.pdf) showing that instead of measuring 6-, 8-, and 10-year post-college-entrance earnings, they actually measured 7-, 9-, and 11-year post-college-entrance earnings. The code reflects the necessary adjustments for this situation. 
