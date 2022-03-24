How to use: 

User drop folder: For users who are implementing their own controls
1. Place csv file(s) with new input data in "emissions/user_emission_controls" data system folder. Files can have any name and are read in in alphabetical order. Later controls overwrite earlier ones, so order does matter. 
2. Run the data system. 
3. Add "emissions_factor_controls_user.xml" or "emissions_factor_controls_USA.xml" which should be in the xml folder, to the configuration file. Make sure it is after the "emissons_factor_controls.xml" or "emissions_factor_controls_USA.xml" file.

Emissions Control Folder: For data intended to be included in the core model in the future
1. Place csv files in "emissions/emission_controls" data system folder. These files must be named starting with "A53" in order for them to be read in and processed by the data system. 
2. Declare new file(s) as inputs in driver.DECLARE_INPUTS in zchunk_L253.emissions_controls.R. No other data system changes are necessary. 
3. Run the data system. 

Example input file structure: If you wanted to specify an emission factor of 0.026 Mt/EJ for all new conventional pulverized coal plants in the USA built in 2020 or after, you would add the following line to your CSV: 

USA, electricity, coal, coal (conv_pul), SO2, NA, 2020, 0.026, NA, NA, NA, NA, NA, US NSPS

Note you can also specify a GDP level, expected in U.S. 2015 dollars at MER, at which to begin the control. For more examples, see the current core data in the emission_controls folder. 

If users put controls on electricity technologies, they must run GCAM without water turned on, as our current structure is incompatible with water technologies. To do so, comment out "elec_water.xml" in the configuration file and replace with "electricity.xml". Additionally, we have separated out GCAM and GCAM-USA controls, but the USA controls have to be added after non-CO2 emissions are added to GCAM-USA. Emission factors should be set with input drivers, or emissions per unit input. If GCAM uses output drivers, we convert in the code. 

Region options include any GCAM region or MAC regions (in emissions/A_regions.csv). If region is a MAC region, control will be applied to all corresponding GCAM regions. For example, if a user specifies "Africa" as a region, control will be applied to Africa Eastern, Northern, Southern, Western, and South Africa. Users can also specify an "All" region, which will be applied to all GCAM regions. For GCAM-USA, a user can use any state name or state abbreviation. There is also an "All states" option. 
