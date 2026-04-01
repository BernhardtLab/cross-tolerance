Some of the inputs needed here were previously generated with Bcftools during major variant calling (see that 
directory in our Github (https://github.com/BernhardtLab/cross-tolerance/tree/main/Python-scripts/Bcftools),
namely the 585.fasta and 585_liftoff_CGD.gff3.

The Coding_vs_Noncoding.ipynb notebook simply calculates the % of coding vs. noncoding DNA in the 585.fasta genome (YL07, ancestor)
using coding sequence coordinates in the 585_liftoff_CGD.gff3 file.

The Assign_Mutation_Type.ipynb notebook takes one of the LoFreq-generated variant table inputs, 
"All_Filtered_40C_Unique.csv" OR "95%_CI_40C_Uniques.csv" (ie. no rare variants), and assigns a functional consequence 
(synonymous, missense, nonsense, frameshift, etc.) to each variant. It does this by mapping variants onto CDS sequences 
and translating codons to determine amino acid changes. It requires 585_CDS.fasta as an additional input, as a result.
The output .tsv files are listed in this directory ("Mutations_All_Filtered_40C_Unique.tsv" and "Mutations_95%_CI_40C_Unique.tsv").
The organized version of the results here can be seen in Table S7: LoFreq Mutation Classifications.
