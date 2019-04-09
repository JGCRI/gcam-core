This code is used to couple GCAM to E3SM.

GCAM changes for GCAM-E3SM


1) Make a dummy main that runs the whole system
   - Start with just running, then add in the couplings
   - Use scalers of 1 for checking (then test easy to interpret alternatives)
   - based on `gcam_comp_mod.f90` but written in C++
   - build on PIC/Compy from the start 

2) Rewrite `CCSM_GCAM_interface.cpp`
   - Cleaner code
   - Remove all > 2096 if blocks
   - Update loops to be over 384 regions (instead of 14 x 18) — read in mapping as a file which determines num regions
   - Swap visitor calls for fusion calls

3) Recreate restarting capabilities with restart method
   - See `ccsm_scenario_runner.cpp`

4) Rewrite `get_clm_data.cpp` in fusion
   - We don’t want multiple mapping files
   - File is currently under reporting
   - We don’t carbon density (that is commented out in all files referring to this variable)
   (File gets land cover aggregated by GLM categories and gets wood harvest)
   - Make GCAM to GLM mapping flexible and with weights so we can divide one GCAM category into many GLM categories (and vice versa)
   
5) Update GitHub GCAM branch to separate yield without impacts and yield adjusted for impacts
   - We’ll back port this into GCAM master later
   - New variable should be marked as a “state” variable so it shows up in restart file

6) Rewrite `set_carbon_density.cpp` as a fusion thing
   - Above ground adj needs to scale both carbon density and yield

7) Make sure xml database is off

8) Rename “debug_db.xml”

9) New config file with “no_climate_model.xml”

10) Rewrite `rcp_emissions_visitor.cpp`
    - For now, don’t worry about sectors; we just want total CO2

If needed) Rewrite `CCSM_GCAM_interface_wrapper.cpp`
   - We think we need to update only if signature of methods it calls change (or if they change what they are passing to us)

