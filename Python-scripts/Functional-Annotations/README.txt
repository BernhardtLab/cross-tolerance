Some of the inputs needed here were previously generated with Bcftools during major variant calling (see that 
directory in our Github (https://github.com/BernhardtLab/cross-tolerance/tree/main/Python-scripts/Bcftools),
namely the 585.fasta and 585_liftoff_CGD.gff3.

The Coding_vs_Noncoding.ipynb file simply calculates the % of coding vs. noncoding DNA in the 585.fasta genome (YL07, ancestor)
using coding sequence coordinates in the 585_liftoff_CGD.gff3 file.
