#ifndef _GCAM_DATA_CONTAINERS_H_
#define _GCAM_DATA_CONTAINERS_H_
#if defined(_MSC_VER)
#pragma once
#endif

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

/*!
 * \file gcam_data_containers.h
 * \ingroup util
 * \brief Includes for all GCAM headers that are data containers which is
 *        necessary when using GCAM fusion since in principal it may travrse
 *        any GCAM data and therefore must know the data definitions of all
 *        GCAM data containers.
 * \author Pralit Patel
 */

#include "ccarbon_model/include/asimple_carbon_calc.h"
#include "ccarbon_model/include/icarbon_calc.h"
#include "ccarbon_model/include/land_carbon_densities.h"
#include "ccarbon_model/include/no_emiss_carbon_calc.h"
#include "ccarbon_model/include/node_carbon_calc.h"
#include "climate/include/iclimate_model.h"
#include "climate/include/magicc_model.h"
#include "climate/include/hector_model.hpp"
#include "climate/include/no_climate_model.h"
#include "consumers/include/consumer.h"
#include "consumers/include/gcam_consumer.h"
#include "containers/include/gdp.h"
#include "containers/include/region.h"
#include "containers/include/region_minicam.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "demographics/include/demographic.h"
#include "demographics/include/population.h"
#include "demographics/include/population_mini_cam.h"
#include "emissions/include/iemissions_driver.h"
#include "emissions/include/input_driver.h"
#include "emissions/include/output_driver.h"
#include "emissions/include/input_output_driver.h"
#include "emissions/include/aemissions_control.h"
#include "emissions/include/aghg.h"
#include "emissions/include/co2_emissions.h"
#include "emissions/include/gdp_control.h"
#include "emissions/include/mac_control.h"
#include "emissions/include/linear_control.h"
#include "emissions/include/readin_control.h"
#include "emissions/include/nonco2_emissions.h"
#include "functions/include/idiscrete_choice.hpp"
#include "functions/include/relative_cost_logit.hpp"
#include "functions/include/absolute_cost_logit.hpp"
#include "functions/include/building_node_input.h"
#include "functions/include/building_service_input.h"
#include "functions/include/food_demand_input.h"
#include "functions/include/efficiency.h"
#include "functions/include/energy_input.h"
#include "functions/include/icoefficient.h"
#include "functions/include/iinput.h"
#include "functions/include/inested_input.h"
#include "functions/include/input_capital.h"
#include "functions/include/input_OM_fixed.h"
#include "functions/include/input_OM_var.h"
#include "functions/include/input_subsidy.h"
#include "functions/include/input_tax.h"
#include "functions/include/intensity.h"
#include "functions/include/minicam_input.h"
#include "functions/include/node_input.h"
#include "functions/include/non_energy_input.h"
#include "functions/include/renewable_input.h"
#include "functions/include/satiation_demand_function.h"
#include "functions/include/thermal_building_service_input.h"
#include "functions/include/ctax_input.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "land_allocator/include/carbon_land_leaf.h"
#include "land_allocator/include/land_allocator.h"
#include "land_allocator/include/land_leaf.h"
#include "land_allocator/include/land_node.h"
#include "land_allocator/include/land_use_history.h"
#include "land_allocator/include/unmanaged_land_leaf.h"
#include "marketplace/include/calibration_market.h"
#include "marketplace/include/demand_market.h"
#include "marketplace/include/inverse_calibration_market.h"
#include "marketplace/include/linked_market.h"
#include "marketplace/include/market.h"
#include "marketplace/include/market_container.h"
#include "marketplace/include/market_RES.h"
#include "marketplace/include/market_subsidy.h"
#include "marketplace/include/market_tax.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/normal_market.h"
#include "marketplace/include/price_market.h"
#include "marketplace/include/trial_value_market.h"
#include "policy/include/linked_ghg_policy.h"
#include "policy/include/policy_ghg.h"
#include "policy/include/policy_portfolio_standard.h"
#include "resources/include/accumulated_grade.h"
#include "resources/include/accumulated_post_grade.h"
#include "resources/include/aresource.h"
#include "resources/include/grade.h"
#include "resources/include/renewable_subresource.h"
#include "resources/include/resource.h"
#include "resources/include/smooth_renewable_subresource.h"
#include "resources/include/subresource.h"
#include "resources/include/reserve_subresource.h"
#include "resources/include/unlimited_resource.h"
#include "sectors/include/afinal_demand.h"
#include "sectors/include/ag_supply_sector.h"
#include "sectors/include/ag_supply_subsector.h"
#include "sectors/include/capacity_limit_backup_calculator.h"
#include "sectors/include/CSP_backup_calculator.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/ibackup_calculator.h"
#include "sectors/include/pass_through_sector.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/nesting_subsector.h"
#include "sectors/include/subsector_add_techcosts.h"
#include "sectors/include/supply_sector.h"
#include "sectors/include/tran_subsector.h"
#include "solution/util/include/solution_info_param_parser.h"
#include "technologies/include/global_technology_database.h"
#include "technologies/include/ag_production_technology.h"
#include "technologies/include/base_technology.h"
#include "technologies/include/cal_data_output.h"
#include "technologies/include/cal_data_output_percap.h"
#include "technologies/include/default_technology.h"
#include "technologies/include/empty_technology.h"
#include "technologies/include/fixed_production_state.h"
#include "technologies/include/fractional_secondary_output.h"
#include "technologies/include/generic_output.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/icapture_component.h"
#include "technologies/include/intermittent_technology.h"
#include "technologies/include/internal_gains.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ishutdown_decider.h"
#include "technologies/include/itechnical_change_calc.h"
#include "technologies/include/itechnology.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/non_energy_use_capture_component.h"
#include "technologies/include/nuke_fuel_technology.h"
#include "technologies/include/pass_through_technology.h"
#include "technologies/include/phased_shutdown_decider.h"
#include "technologies/include/power_plant_capture_component.h"
#include "technologies/include/primary_output.h"
#include "technologies/include/profit_shutdown_decider.h"
#include "technologies/include/res_secondary_output.h"
#include "technologies/include/residue_biomass_output.h"
#include "technologies/include/retired_production_state.h"
#include "technologies/include/s_curve_shutdown_decider.h"
#include "technologies/include/secondary_output.h"
#include "technologies/include/solar_technology.h"
#include "technologies/include/standard_capture_component.h"
#include "technologies/include/standard_technical_change_calc.h"
#include "technologies/include/technology.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/stub_technology_container.h"
#include "technologies/include/tran_technology.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "technologies/include/variable_production_state.h"
#include "technologies/include/vintage_production_state.h"
#include "technologies/include/wind_technology.h"
#include "technologies/include/resource_reserve_technology.h"
#include "solution/solvers/include/solver.h"
#include "solution/solvers/include/user_configurable_solver.h"
#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_all.h"
#include "solution/solvers/include/logbroyden.hpp"
#include "solution/solvers/include/preconditioner.hpp"
#include "util/base/include/data_definition_util.h"
#include "util/base/include/fixed_interpolation_function.h"
#include "util/base/include/iinterpolation_function.h"
#include "util/base/include/interpolation_rule.h"
#include "util/base/include/linear_interpolation_function.h"
#include "util/base/include/s_curve_interpolation_function.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/data_point.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/object_meta_info.h"

#endif // _GCAM_DATA_CONTAINERS_H_
