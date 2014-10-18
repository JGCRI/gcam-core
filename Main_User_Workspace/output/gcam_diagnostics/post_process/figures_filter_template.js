/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

/*
 *  Javascript to handle dynamically displaying and filtering figures generated
 *  by the GCAM diagnostics scripts.
 */

// All available figures and their metadata
REPLACE_FIGURE_DATA

// All available filters
REPLACE_FILTER_DATA

// The currently active filters
var curr_filters = new Object;
var disabled_filters = new Object;
var num_figures_selected = 0;

/*
 * Since the number of figures my be large it can take a while to initialize
 * so we displace a "Please wait" banner.  Once we are done we simply hide it.
 */
function hideInitializeBanner() {
    document.getElementById("initializingBanner").style.visibility = "hidden";
}

/*
 * Populate the menu of available filters which should be done on start up.
 */
function populateAvailableFiltersMenu() {
    var filter_list_html = '';
    for(var filter in filter_data) {
        filter_list_html += '<input type="checkbox" onClick="toggleActiveFilters(this)" value="' +
            filter + '">' + filter + '</input><br/>';
        // Save a COPY of the filters for when a filter needs to be set to
        // active again.
        disabled_filters[filter] = filter_data[filter].slice(0);
    }
    document.getElementById('availableFilterMenu').innerHTML = filter_list_html;
}

/*
 * Add/remove an active filter.  Note that filters that are moved to/from the
 * disabled_filters varaible instead of filter_data so that previously set
 * filter selections can be remembered.
 */
function toggleActiveFilters(filter_checkbox) {
    var filter_key = filter_checkbox.value;
    if(filter_checkbox.checked) {
        // Need to show this filter
        // Restore a COPY of the filter selection from the last time
        // it was enabled.
        curr_filters[filter_key] = disabled_filters[filter_key].slice(0);
        var all_values = filter_data[filter_key];
        var selected_values = curr_filters[filter_key];
        var filter_html = '<b>'+filter_key+'</b><br/><select multiple name="'+filter_key+'" size="'+
            Math.min(all_values.length, 20)+'" onchange="selectionChanged(this);">';
        for(var i = 0; i < all_values.length; ++i) {
            filter_html += '<option value="'+all_values[i]+'" ';
            if(selected_values.indexOf(all_values[i]) != -1) {
                filter_html += 'selected';
            }
            filter_html += '>'+all_values[i]+'</option>';
        }
        filter_html += '</select>';
        var filter_row = document.getElementById('activeFiltersArea');
        var new_cell = filter_row.insertCell(-1);
        new_cell.innerHTML = filter_html;
        new_cell.id = filter_key+'FilterArea';
    } else {
        // Save a COPY of the current selection in case a user wants to bring it
        // back.
        disabled_filters[filter_key] = curr_filters[filter_key].slice(0);
        delete curr_filters[filter_key];
        var filter_row = document.getElementById('activeFiltersArea');
        var filter_area_list = filter_row.getElementsByTagName('td');
        for(var i = 0; i < filter_area_list.length; ++i) {
            if(filter_area_list[i].id == filter_key+'FilterArea') {
                filter_row.deleteCell(i);
                break;
            }
        }

    }
    document.getElementById('doFilterButton').disabled = false;
}

/*
 * We were noticed that a filter selection was changed.  For performance
 * reasons we will not automatically re-filter the figures.  Instead we
 * will update curr_filters and enable the doFilterButton.
 */
function selectionChanged(filter_form) {
    var filter_key = filter_form.name;
    for(var i = 0; i < filter_form.options.length; ++i) {
        var curr_option = filter_form.options[i];
        var index_in_filter = curr_filters[filter_key].indexOf(curr_option.value);
        //alert(filter_form.options[i].selected);
        if(index_in_filter == -1 && curr_option.selected) {
            // The option is not set in curr_filters but is selected.
            // We need to add it to curr_filters.
            curr_filters[filter_key].push(curr_option.value);
        } else if(index_in_filter != -1 && !curr_option.selected) {
            // The option is set in curr_filters but is not selected
            // We need to remove it from curr_filters.
            curr_filters[filter_key].splice(index_in_filter, 1);
        }
        // otherwise either both the value exists and is selected or
        // both the value does not exist and is not selected
    }
    document.getElementById('doFilterButton').disabled = false;
}

/*
 * Toggles all the figure check boxes to allow show all figures.
 */
function toggleSelectAll(select_all_box) {
    var checkboxes = document.getElementsByName("showFigureBox");
    for(var i = 0; i < checkboxes.length; ++i) {
        checkboxes[i].checked = select_all_box.checked;
        countNumChecked(checkboxes[i]);
    }
}

/*
 * Check if any figures are now selected so that we can enable the Show
 * Selected button.
 */
function countNumChecked(figure_checkbox) {
    if(figure_checkbox.checked) {
        ++num_figures_selected;
    } else {
        --num_figures_selected;
    }
    document.getElementById('showAllButton').disabled = num_figures_selected == 0;
}

/*
 * Go through all the figures and only show the ones what match all filters.
 */
function doFilter() {
    document.getElementById('doFilterButton').disabled = true;
    var show_selected_row_html = '<tr><td><input type="checkbox" id="selectAllBox" onClick="toggleSelectAll(this)">Select All</input></td>' +
        '<td style="width: 600px;"><input type="submit" id="showAllButton" onClick="showSelection();" value="Show Selected"/></td><td></td></tr>';
    var num_figures_shown = 0;
    num_figures_selected = 0;
    var all_figures_html = '';
    for(var figure_index = 0; figure_index < figures_data.length; ++figure_index) {
        var matches_filters = true;
        var curr_figure = figures_data[figure_index];
        for(var filter in curr_filters) {
            if(typeof curr_figure[filter] == undefined ||
               curr_filters[filter].indexOf(curr_figure[filter]) == -1 )
            {
                // The current figure does not have the filter or the value
                // for that filter is not selected.
                matches_filters = false;
                break;
            }
        }
        if(matches_filters) {
            // Add the figure to the list.
            var figure_html = '<tr><td><input type="checkbox" onClick="countNumChecked(this);" name="showFigureBox" value="'+figure_index+'"></input></td>';
            figure_html += '<td><a target="_blank" href="'+curr_figure['graphfn']+'">'+curr_figure['graphfn']+
                '</a></td>';
            if(typeof curr_figure['datafn'] != undefined) {
                // only add a link to the data if it was generated
                figure_html += '<td><a href="'+curr_figure['datafn']+'">CSV</a></td>';
            }
            figure_html += '</tr>';
            all_figures_html += figure_html;
            ++num_figures_shown
        }
    }
    document.getElementById('figuresTable').innerHTML = show_selected_row_html + all_figures_html;
    document.getElementById('selectAllBox').disabled = num_figures_shown == 0;
    document.getElementById('showAllButton').disabled = num_figures_selected == 0;
}

/*
 * Show all selected figures in a new window.
 */
function showSelection() {
    // Generate boiler plate for new page.
    var page_html = '<html><head><title>GCAM Seleced Figures</title>' +
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' +
        '<style>body{ color: #FFFFFF; background-color: #FFFFFF; }</style></head><body>';

    // Find the selected showFigureBox check boxes and generate img tags
    // for them one after the other.
    var show_check_boxes = document.getElementsByName('showFigureBox');
    for(var i = 0; i < show_check_boxes.length; ++i) {
        if(show_check_boxes[i].checked) {
            var curr_filefn = figures_data[show_check_boxes[i].value]['graphfn'];
            page_html += '<p>'+curr_filefn+'</p><img src="'+curr_filefn+'" />';
        }
    }
    page_html += '</body></html>';
    // Create a new page and set html
    var new_window = window.open("");
    new_window.document.write(page_html);
}

