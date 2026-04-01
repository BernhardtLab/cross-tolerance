LoFreq (with the scripts and inputs in this directory) was used for marginal/subpopulation variant calling. 
Many of the files used here were previously generated with Bcftools during major variant calling (see that 
directory in our Github (https://github.com/BernhardtLab/cross-tolerance/tree/main/Python-scripts/Bcftools).
The initial LoFreq analyses were NOT similarly performed on the HPCs available via the Digital Research Alliance of Canada (alliance​can​.ca) since LoFreq was not supported.

1. Use the '1. Call_Variants' script to generate indel qualities (ie. the confidence that no insertion 
or deletion occurs at each base for the previously generated .BAM files for each evolved population, 
index them, and finally call variants via LoFreq.

2. Next, use the '2. Filter_Variants' script to remove any variants called by LoFreq that were either
only present in less than 5% of the reads, OR were flagged as low-quality by Bcftools. 
Then, it compresses and indexes the filtered VCFs, and merges them into one multi-sample VCF.
