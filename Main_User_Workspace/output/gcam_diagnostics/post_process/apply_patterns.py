#!/usr/bin/env python

# LEGAL NOTICE
# This computer software was prepared by Battelle Memorial Institute,
# hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
# with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
# CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
# LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
# sentence must appear on any copies of this computer software.
# 
# EXPORT CONTROL
# User agrees that the Software will not be shipped, transferred or
# exported into any country or used in any manner prohibited by the
# United States Export Administration Act or any other applicable
# export laws, restrictions or regulations (collectively the "Export Laws").
# Export of the Software may require some form of license or other
# authority from the U.S. Government, and failure to obtain such
# export control license may result in criminal liability under
# U.S. laws. In addition, if the Software is identified as export controlled
# items under the Export Laws, User represents and warrants that User
# is not a citizen, or otherwise located within, an embargoed nation
# (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
#     and that User is not otherwise prohibited
# under the Export Laws from receiving the Software.
# 
# Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
# Distributed as open-source under the terms of the Educational Community 
# License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
# 
# For further details, see: http://www.globalchange.umd.edu/models/gcam/
#

# Post processing script which will send commands to a "server" running insid eof GIMP
# to apply patterns etc.  This script simply loops through all of the figures and
# determine which post processing if any should happen then tell the "server" to do it.

import xmlrpclib
from os import listdir
from os import path

# Connect to an image processing server running from with in GIMP which will do
# the actual processing
s = xmlrpclib.ServerProxy('http://localhost:8000')

# Patterns look "correct" at 300dpi, if the figures are in another dpi you might want to
# Change the scale i.e. at 150 use a scale of 50 (for 50%), if no scaling is needed
# you can use -1 to indicate no scaling
s.set_pattern_scale(50)

# Go through all file in the figures directory and determine what to do with it
figures_dir = '../figures'
all_files = listdir(figures_dir)
for curr_file in all_files:
    print 'Processing ', curr_file
    full_path_name = path.abspath(figures_dir+'/'+curr_file)
    if not curr_file.endswith('.png'):
        # Skip non image files.
        print 'Skipping'
    elif curr_file.find('Industry') != -1 and curr_file.find('Diff') == -1:
        print 'Industry'
        s.tile_feedstocks(full_path_name)
    elif curr_file.find('FinalEnergyConsumptionbySector') != -1 and curr_file.find('Diff') == -1:
        print 'FE'
        s.barify_enduse(full_path_name)
    elif (curr_file.find('PrimaryEnergyConsumption') != -1 or curr_file.find('CCS') != -1 or curr_file.find('Hydrogen') != -1) and curr_file.find('Diff') == -1:
        print 'CCS'
        s.hashify_ccs(full_path_name)
    elif curr_file.find('RenewablesFocus') != -1 and curr_file.find('Diff') == -1:
        print 'Renew'
        s.hashify_storage(full_path_name)
    elif curr_file.find('RefinedLiquid') != -1 and curr_file.find('Diff') == -1:
        print 'Ref liq'
        s.sprinkle_refliq(full_path_name)
    elif curr_file.find('CO2emissions') != -1 and curr_file.find('Diff') == -1:
        print 'CO2'
        s.barify_emissions(full_path_name)
    else:
        # Just convert the color profile for consistency and resave it
        print 'Convert rgb'
        s.change_color_profile(full_path_name)

# Tell the image processing host to shutdown
s.all_done()
