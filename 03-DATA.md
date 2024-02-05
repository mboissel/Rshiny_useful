### Phenotypes and analyses

Two main statistical analyses have been performed:

  - a case-control study for obesity (adjusted for age, sex, ethnicity PC1 and ethnicity PC2).
  
  - a case-control study for type 2 diabetes risk (adjusted for age, sex, BMI, ethnicity PC1 and ethnicity PC2).

For ethnicity, we used 15,020 SNPs present in both our sequencing project and 1000 Genomes project (with minor allele frequency higher than 5% in both projects).

Common variants (with a MAF >= 1%) were independently analyzed via a standard model while rare variants (with a MAF < 1%) were analyzed in aggregation (per transcript) using the MiST method.

Then two strategies have been used about filtering SNPs ('LARGE' or 'STRICT') :

  - strategy LARGE : we analysed all variants with the following 'consequence' tags: initiator_codon, synonymous, stop_retained, stop_lost, stop_gained, missense, incomplete_terminal_codon, coding_sequence, splice_acceptor, splice_donor, splice_region, regulatory_region and initiator_codon_variant.

  - strategy STRICT : we analysed only variants with the following 'consequence' tags: initiator_codon, stop_retained, stop_lost, stop_gained, missense and initiator_codon.
