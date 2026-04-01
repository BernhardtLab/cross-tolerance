All CNV analyses were performed in a Jupyter Notebook on the HPCs available via the 
Digital Research Alliance of Canada (alliance​can​.ca). Some scripts in this directory 
rely on submitting jobs via SLURM. Many of the files used here were previously generated with 
Bcftools during major variant calling (see that directory in our Github: 
(https://github.com/BernhardtLab/cross-tolerance/tree/main/Python-scripts/Bcftools).

1. First, calculate the depth of each genomic position (ie. how many reads cover each position) 
using the script from '1. Calculate_Read_Depth'. 

2. After this, run through each cell in the 'CNV_analysis.ipynb' file. This will _____ 
