# **ECARO Violence Against Children Statistical Analysis**

Analysis done for UNICEF ECARO based on secondary data analysis.

### Instructions

Download the github repo in your local system.

The code contains already analysis done, and if re-run it will overwrite the analysis outcome.

The analysis can be run all together running `00 all analysis.R` script, or as a standalone bits, considering that it is divided into two bits that should be run to have the code working properly.

-   Extraction and cleaning, running first `01 data extraction.R` and secondly any script into `cleaning scripts/` folder, concluding with `cleaning scripts/09 consolidate.R` which consolidates the cleaned data into the same excel.

-   Analysis phase, where any script into `analysis scripts/` folder works independently to output into `analysis/` folder divided per ICVAC categories.
