#include "../include/GCAM_E3SM_interface.h"

GCAM_E3SM_interface *p_obj;

extern "C" {

  // Initialize a GCAM_E3SM_interface object
  void inite3sminterface_() {
    p_obj = new GCAM_E3SM_interface();
  }
  
  // Delete the GCAM_E3SM_interface object
  void deletee3sminterface_() {
    delete p_obj;
  }
    
  // Call the GCAM initialization
  void initcgcam_() {
    p_obj->initGCAM();
  }

  // Set Carbon Densities in GCAM using scalers from E3SM
  void setdensitycgcam_(int *ymd, int *tod, double *gcami, int *gcami_fdim_1, int *gcami_fdim_2) {
//    p_obj->setDensityGCAM(ymd, tod, gcami,gcami_fdim_1,gcami_fdim_2);
  }
    
  // Run GCAM
  void runcgcam_(int *ymd, int *tod, double *gcami, int *gcami_fdim_1, int *gcami_fdim_2, double *gcamo, int *gcamo_fdim_1, int *gcamo_fdim_2, double *gcamoemis, int *gcamoemis_fdim_1, int *gcamoemis_fdim_2,int *gcamoyr1, int* gcamoyr2,int* sneakermode,int* write_rest) {
    p_obj->runGCAM(ymd, tod, gcami,gcami_fdim_1,gcami_fdim_2,gcamo, gcamo_fdim_1, gcamo_fdim_2,gcamoemis, gcamoemis_fdim_1, gcamoemis_fdim_2,gcamoyr1, gcamoyr2,sneakermode,write_rest);
  }
    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
