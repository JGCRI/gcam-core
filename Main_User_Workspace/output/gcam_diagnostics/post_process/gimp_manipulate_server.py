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

# Add patterns for various figures including hashing for CCS or renewable + storage,
# rounding effects for enduse and emissions, tiling feedstocks, and sprinkling refined
# liquids

from gimpfu import *
from SimpleXMLRPCServer import SimpleXMLRPCServer

# Start a server that will we will recieve commands from
# The commands available are added using register_function
server = SimpleXMLRPCServer(("localhost", 8000))

pattern_scale = -1

# Done processing, close the server
def all_done():
    server.server_close()
    return True

server.register_function(all_done)

# Allow users to scale the patterns before filling since they were
# Created to look "right" at 300dpi using something else may look add
# so a user would want to scale it up or down
def set_pattern_scale(new_scale):
    print 'Setting new pattern scale ', new_scale
    global pattern_scale
    pattern_scale = new_scale
    return True

server.register_function(set_pattern_scale)

# Convert a string for color in "HTML" format to RGB triplets
def rgb(triplet):
    return (int(triplet[0:2], 16), int(triplet[2:4], 16), int(triplet[4:6], 16))

# Replace a named HTML color with a named pattern
# Warning we are assuming the pattern has already been created
def replace_color_with_pattern(image, drawable, color_str, pattern_str):
    replace_color = rgb(color_str)

    # The color should be accurate so set a low threshold to select by color
    pdb.gimp_context_set_sample_threshold(0.05)
    pdb.gimp_image_select_color(image, CHANNEL_OP_REPLACE, drawable, replace_color)
    pdb.gimp_selection_sharpen(image)

    # If the color didn't exist in the figure we are done
    if pdb.gimp_selection_is_empty(image):
        return

    # Load the pattern, if the user set the scale size we need to scale it 
    pdb.gimp_context_set_pattern(pattern_str)
    if pattern_scale != -1:
        pdb.script_fu_scale_pattern(pattern_scale, 10)

    # Fill the pattern which will replace the color
    pdb.gimp_edit_bucket_fill(drawable, PATTERN_BUCKET_FILL, NORMAL_MODE, 100.0, 15.0, False, 0, 0)
    return

def hashify_ccs(file_name) :
    print 'Adding hashes for CCS to ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        # WARNING: assuming these patterns exist
        replace_color_with_pattern(image, drawable, "dab4c7", "coal ccs")
        replace_color_with_pattern(image, drawable, "84e7f9", "gas ccs")
        replace_color_with_pattern(image, drawable, "f7988f", "oil ccs")
        replace_color_with_pattern(image, drawable, "88c892", "bio ccs")

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(hashify_ccs)

def hashify_storage(file_name) :
    print 'Adding hashes for storage to ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        # WARNING: assuming these patterns exist
        replace_color_with_pattern(image, drawable, "9ec1fd", "wind storage")
        replace_color_with_pattern(image, drawable, "f5c88c", "csp storage")
        replace_color_with_pattern(image, drawable, "ffe791", "pv storage")

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(hashify_storage)

def sprinkle_refliq(file_name) :
    print 'Adding sprinkles on refliq ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        # WARNING: assuming these patterns exist
        replace_color_with_pattern(image, drawable, "ab4500", "sprinkle") # conv oil
        replace_color_with_pattern(image, drawable, "ff9593", "sprinkle") # unconv oil
        replace_color_with_pattern(image, drawable, "ff2600", "sprinkle") # coal
        replace_color_with_pattern(image, drawable, "ff8d78", "coal ccs refliq")
        replace_color_with_pattern(image, drawable, "00931d", "sprinkle") # biomass
        replace_color_with_pattern(image, drawable, "728f72", "bio ccs")
        replace_color_with_pattern(image, drawable, "1633ff", "sprinkle") # gas

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(sprinkle_refliq)

def tile_feedstocks(file_name) :
    print 'Adding  tiles to ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        # WARNING: assuming these patterns exist
        replace_color_with_pattern(image, drawable, "ff7467", "feedstocks")

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(tile_feedstocks)

# Replace the color given given in HTML format with a rounded bar
# looking gradient of the same color
def replace_color_with_gradient(image, drawable, color_str):
    replace_color = rgb(color_str)
    # Set the color as the foreground color since that is what
    # the gradient will check to use as the color
    pdb.gimp_context_set_foreground(replace_color)
    # WARNING: assuming the gradient exists
    pdb.gimp_context_set_gradient('rounded bar FG')

    # The color should be accurate so we will set the threshold pretty low
    pdb.gimp_context_set_sample_threshold(0.05)
    pdb.gimp_image_select_color(image, CHANNEL_OP_REPLACE, drawable, replace_color)
    pdb.gimp_selection_sharpen(image)

    # We need to break the selection into each "bar" because we need to apply the
    # gradient to each independently
    pdb.plug_in_sel2path(image, drawable)
    selection_vector = image.vectors[0]
    selection_strokes = selection_vector.strokes
    for curr_stroke in selection_strokes:
        # The points define a Bezier curve to form the selection area
        # since our shape is just a rectangle this is pretty each, all we
        # need are the max+min x and y coordinates
        ctrl_points = pdb.gimp_vectors_stroke_get_points(selection_vector, curr_stroke.ID)
        i = 0
        x_points = set()
        y_points = set()
        while i < ctrl_points[1]:
            x_points.add(ctrl_points[2][i])
            y_points.add(ctrl_points[2][i+1])
            i = i + 2
        x_points = list(x_points)
        y_points = list(y_points)
        x_points.sort()
        y_points.sort()

        # Change the selection to just the current bar
        pdb.gimp_image_select_rectangle(image, CHANNEL_OP_REPLACE, x_points[0], y_points[0], x_points[-1] - x_points[0], y_points[-1] - y_points[0])
        # Apply the gradient across the current bar in a flat linear pattern
        pdb.gimp_edit_blend(drawable, CUSTOM_MODE, NORMAL_MODE, GRADIENT_LINEAR, 100, 0, REPEAT_NONE, False, False, 1, 0, False, x_points[0]-2, y_points[0], x_points[-1], y_points[0])

    return

def barify_enduse(file_name) :
    print 'Adding rounding effect to ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        replace_color_with_gradient(image, drawable, "facda4") # buildings
        replace_color_with_gradient(image, drawable, "cef4d1") # industry
        replace_color_with_gradient(image, drawable, "d0f6f7") # transport

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(barify_enduse)

def barify_emissions(file_name) :
    print 'Adding rounding effect to ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Replace colors with pattern
        replace_color_with_gradient(image, drawable, "facda4") # buildings
        replace_color_with_gradient(image, drawable, "cef4d1") # industry
        replace_color_with_gradient(image, drawable, "d0f6f7") # transport
        replace_color_with_gradient(image, drawable, "ea9219") # electricity
        replace_color_with_gradient(image, drawable, "ff230e") # cement

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(barify_emissions)

def change_color_profile(file_name) :
    print 'Change RGB profile ', file_name
    try:
        # Load image
        image = pdb.gimp_file_load(file_name,  file_name)
        # The RGB values are specified in the sRGB profile however ggplot seems to be marking
        # them as generic RGB.  The causes problems when we try to select the colors so we
        # tell GIMP to convert the RGB profile.
        pdb.plug_in_icc_profile_apply_rgb(image, 0, 0)
        drawable = pdb.gimp_image_get_active_layer(image)

        # Save the image and clean up
        pdb.gimp_file_save(image, drawable, file_name, file_name)
        pdb.gimp_image_delete(image)

    except:
        pdb.gimp_message("Error processing file: "+file_name)
    return True

server.register_function(change_color_profile)

# Start the server
try:
    server.serve_forever()
except:
    print 'Done'

