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

3. After this, we can take each filtered .VCF and convert them into a usable table:

for f in *.af005.vcf; do   sample=$(basename $f .af005.vcf);   bcftools query -f '%CHROM\t%POS\t%REF\t%ALT\t%INFO/AF\n' $f | awk -v s=$sample '{print $0 "\t" s}' > ${sample}.af.tsv; done

4. Finally, use '3. Merge_Matrices' to merge all of the individual .tsv files into one .xlsx output where
all of the samples are listed with the corresponding variants. This final output can be seen in the 
current directory (variants_AF_matrix.xlsx). This was then subsequently filtered and organized in Excel following the methods stated in the manuscript. All of these filtering steps and the final list of organized warm-evolved-specific major variants are listed in Table S5: Marginal (Subpopulation) Variants (LoFreq). 
