/* modeltime.h				*
 * global object			*/
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
using namespace std; // enables elimination of std::


// struct for model time period information
class Modeltime
{
private:
	int StartYr; // model start year (read-in) 
	int InterYr1; // first intermediate year
	int InterYr2; // second intermediate yer
	int EndYr; // model end year (read-in)
	int PopStartYr; // start year for population data (read-in) 
	int DataEndYr; // Last year for general data (read-in) 
	int MaxPer; // max number of model periods (calculated)
	int MaxDataPer; // max number of data points (read-in)
	int MaxPopData; // max number of data points for population (read-in)
	int DataTimestep; // timestep for data points
	int Timestep1; // timestep from start to first intermediate year
	int Timestep2; // timestep from first to second intermediate year
	int Timestep3; // timestep from second intermediate to end year
	int NoPer1;  // number of periods in first time interval
	int NoPer1a; // one more in first time interval for remainder year
	int NoPer2;  // number of periods in second time interval
	int NoPer2a; // one more in second time interval for remainder year
	int NoPer3;  // number of periods in third time interval
	int NoPer3a; // one more in third time interval for remainder year
	vector<int> per_to_tstep; // index of timesteps
	vector<int> data_to_mod; // index of data to model per
	vector<int> mod_to_pop; // index of model per to population per
	vector<int> popdata_popvar; // index of population data to variable
	vector<int> dataoffset; // index of timesteps
	vector<int> per_to_yr; // model period to year
	map<int,int> yr_to_per; // year to model period map object

public:
	Modeltime(void) {StartYr=EndYr=Timestep1=MaxPer=MaxDataPer=0;} // default construtor
	//~Modeltime(void); // destructor Why is it not needed?
	void set(void) {
		// function protocol
		void dbmodelread(double* temp,string region,string var1name,string var2name);

		double tmp[1]={9999}; // one dimentional array containing value of 9999
		dbmodelread(tmp,"model","year","begin");	// reads in model start year
		StartYr=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","popbegin");	// reads in model start year
		PopStartYr=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","end");	// reads in model end year
		EndYr=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","dataend");	// reads in model end year
		DataEndYr=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","intermediate1");	// reads in model end year
		InterYr1=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","intermediate2");	// reads in model end year
		InterYr2=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","timestep1");	// reads in model timestep		
		Timestep1=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","timestep2");	// reads in model timestep		
		Timestep2=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","timestep3");	// reads in model timestep		
		Timestep3=int(tmp[0]); // cast from double to int
		dbmodelread(tmp,"model","year","datatimestep");	// reads in data timestep		
		DataTimestep=int(tmp[0]); // cast from double to int

		NoPer1 = (InterYr1 - StartYr)/Timestep1 + 1; // +1 for first year
		NoPer2 = (InterYr2 - InterYr1)/Timestep2; 
		NoPer3 = (EndYr - InterYr2)/Timestep3;
	
		NoPer1a = NoPer1; // initialize
		NoPer2a = NoPer2;
		NoPer3a = NoPer3;
		// write message if time intervals are not divisible by their
		// relative time steps, model will still run okay
		int rem1 = (InterYr1 - StartYr)%Timestep1;
		int rem2 = (InterYr2 - InterYr1)%Timestep2;
		int rem3 = (EndYr - InterYr2)%Timestep3;

		if(rem1 != 0) {
			NoPer1a++; // one more for remainder year
			cout<<"first time interval not divisible timestep1\n"; 
		}
		if(rem2 != 0) {
			NoPer2a++; // one more for remainder year
			cout<<"second time interval not divisible timestep2\n"; 
		}
		if(rem3 != 0) {
			NoPer3a++; // one more for remainder year
			cout<<"third time interval not divisible timestep3\n"; 
		}
		MaxPer = NoPer1a + NoPer2a + NoPer3a; // calculate total number of periods
		// number of periods for general data
		MaxDataPer = (DataEndYr - StartYr)/DataTimestep+1; // +1 for first year
		// number of periods for population data (one more than general data)
		MaxPopData = (DataEndYr - PopStartYr)/DataTimestep+1; // +1 for first year
		// initialize per_timestep vector
		// retrieve timestep for each modeling period
		per_to_tstep.resize(MaxPer);
		for(int i=0;i<NoPer1;i++) per_to_tstep[i]=Timestep1;
		for(i=NoPer1;i<NoPer1a;i++) per_to_tstep[i]=rem1;
		for(i=NoPer1a;i<(NoPer1a+NoPer2);i++) per_to_tstep[i]=Timestep2;
		for(i=(NoPer1a+NoPer2);i<(NoPer1a+NoPer2a);i++) per_to_tstep[i]=rem2;
		for(i=(NoPer1a+NoPer2a);i<(NoPer1a+NoPer2a+NoPer3);i++) per_to_tstep[i]=Timestep3;
		for(i=(NoPer1a+NoPer2a+NoPer3);i<(NoPer1a+NoPer2a+NoPer3a);i++) per_to_tstep[i]=rem3;


		// initialize map object
		// retrieve model period from year
		int baseyr = StartYr;
		yr_to_per[baseyr] = 0; // map object, no resize required
		per_to_yr.resize(MaxPer);
		per_to_yr[0] = baseyr;
		for (i=1;i<MaxPer;i++) {
			yr_to_per[baseyr + per_to_tstep[i]] = i;
			per_to_yr[i] = baseyr + per_to_tstep[i];
			// set years between two time periods to correspond to the
			// second time period
			if(per_to_tstep[i]>1) {
				for(int y=1;y<per_to_tstep[i];y++)
					yr_to_per[baseyr + y] = i;
			} 
			baseyr += per_to_tstep[i];
		}
		// number of model periods to reach each data period
		dataoffset.resize(MaxDataPer);
		// retrieve model period from data period
		data_to_mod.resize(MaxDataPer);
		for (i=0;i<MaxDataPer;i++) {
			int m = yr_to_per[StartYr + i*DataTimestep];
			dataoffset[i] = DataTimestep/per_to_tstep[m];
			data_to_mod[i] = m;
		}
		// retrieve population period from model period
		mod_to_pop.resize(MaxPer);
		for (i=0;i<MaxPer;i++) {
			int m = i+dataoffset[0]; //offset by first timestep only
			mod_to_pop[i] = m;
		}
		// retrieve pop variable index from pop data period
		popdata_popvar.resize(MaxPopData);
		popdata_popvar[0] = 0; 
		for (i=0;i<MaxDataPer;i++) {
			popdata_popvar[i+1] = data_to_mod[i]+dataoffset[0];
		}
	}

	int getstartyr(void) {return StartYr; }
	int getendyr(void) {return EndYr; }
	int getdataendyr(void) {return DataEndYr; }
	int gettimestep(int per) {return per_to_tstep[per]; } // years from last to current per
	int getmaxper(void) {return MaxPer; }  // max modeling periods
	int getmaxdataper(void) {return MaxDataPer; } // data points for reading in
	int getdtimestep(void) {return DataTimestep; } // timestep for data points
	int getmaxpopdata(void) {return MaxPopData; } // number population data points
	int getdataoffset(int dper) {return dataoffset[dper]; }  // data to model timestep
	int getyr_to_per(int year) {return yr_to_per[year]; }  // year to model period
	int getper_to_yr(int per) {return per_to_yr[per]; }  // year to model period
	int getdata_to_mod(int dper) {return data_to_mod[dper];} // data per to model per 
	int getmod_to_pop(int per) {return mod_to_pop[per];} // model per to pop per 
	int getpopdata_popvar(int pdper) {return popdata_popvar[pdper];} // model per to pop per 
};

