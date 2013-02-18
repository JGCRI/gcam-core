# L221_RGCAM_trn_adv.R
# Generates state level detailed transportation advanced input file
# Universal header file - provides logging, file support, etc.

source( "rgcam-processing-code/headers/RGCAM_header.R" )
source( "rgcam-data/Assumptions/A_RGCAM_data.R" )
logstart( "L221_RGCAM_trn_adv.R" )
printlog( "RGCAM detailed transportation advanced input file" )

# -----------------------------------------------------------------------------
# 1. Read files

A_trn_elec_adv <- readdata( "A_trn_elec_adv" )

# -----------------------------------------------------------------------------
# 2. Perform computations and build model input files

# Create a for technology parameters including costs, efficiencies, inputs, etc
printlog( "L221_GlobalDBTechInput: TECH INPUT" )
# The technologies that pull from states will not be placed in the global tech db
L221_GlobalDBTechInput <- A_trn_elec_adv[, names( A_trn_elec_adv ) %!in% c( "tech_index" ) ]
# TODO: rounding?

# -----------------------------------------------------------------------------
# 3. Write all csvs as tables, and paste csv filenames into a single batch XML file

write_mi_data( L221_GlobalDBTechInput, "TrnGlobalDBTechInput", "L221_GlobalDBTechInput", "batch_rgcam_trn_elec_adv.xml" )

insert_file_into_batchxml( "batch_rgcam_trn_elec_adv.xml", "rgcam_trn_elec_adv.xml", "", xml_tag="outFile" )

logstop()
