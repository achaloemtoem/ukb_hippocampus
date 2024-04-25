# --------------------------------------------------------------------------------- #
#                                                                                   #
#  Hippocampal volumes in UK Biobank are associated with APOE only in older adults  #
#                                                                                   #
#                                 Chaloemtoem et al                                 #
#                                                                                   #
#                                                                                   #
#  Script: apoe_processing.R                                                        #
#                                                                                   #
# --------------------------------------------------------------------------------- #


# _____________________________________ INPUT _____________________________________ #

# 1. APOE haplotypes (extracted from UKB phased haplotypes in BGEN v1.2 format)

#    Format: ID, Hap1, Hap2
#            1, CC, CT
#            .
#            .
#            n, TC, CC


# _____________________________________ SCRIPT ____________________________________ #

### Assign APOE status based on haplotypes

apoe_geno <- c()

for (i in 1:nrow(haplo_table)) {
  hap1 = haplo_table$Hap1
  hap2 = haplo_table$Hap2
  
  if (hap1 == "CT") {
    if (hap2 == "CT") { apoe_geno[i] = "e1/e1"
    } else if (hap2 == "TT") { apoe_geno[i] = "e1/e2"
    } else if (hap2 == "TC") { apoe_geno[i] = "e1/e3"
    } else if (hap2 == "CC") { apoe_geno[i] = "e1/e4" }
  } else if (hap1 == "TT") {
    if (hap2 == "CT") { apoe_geno[i] = "e1/e2"
    } else if (hap2 == "TT") { apoe_geno[i] = "e2/e2"
    } else if (hap2 == "TC") { apoe_geno[i] = "e2/e3"
    } else if (hap2 == "CC") { apoe_geno[i] = "e2/e4" }
  } else if (hap1 == "TC") {
    if (hap2 == "CT") { apoe_geno[i] = "e1/e3"
    } else if (hap2 == "TT") { apoe_geno[i] = "e2/e3"
    } else if (hap2 == "TC") { apoe_geno[i] = "e3/e3"
    } else if (hap2 == "CC") { apoe_geno[i] = "e3/e4" }
  } else if (hap1 == "CC") {
    if (hap2 == "CT") { apoe_geno[i] = "e1/e4"
    } else if (hap2 == "TT") { apoe_geno[i] = "e2/e4"
    } else if (hap2 == "TC") { apoe_geno[i] = "e3/e4"
    } else if (hap2 == "CC") { apoe_geno[i] = "e4/e4" }
  }
}

apoe_table <- data.frame(haplo_table, apoe_geno)

apoe_table <- apoe_table %>%
  select(ID, apoe_geno)

# _________________________________ END OF SCRIPT _________________________________ #
