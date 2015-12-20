#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "find_file.h"

#define PI 3.1415926
#define RADIUS 6370997.00

#define BUFSIZE 1024

float AreaFromLonLat(float,float);
int main()
{

FILE *fileinAEZ,*fileinProtected,*fileinSoil,*fileinSage, *fileout;
FILE *fileinname;
FILE *fileinVegCarbon;
FILE *FileLandArea;
FILE *fileout_Summary;
FILE *fileinAEZ_2_GCAM,*fileoutSoil_GCAM;
int *sage_class,sage_ID;
float *soil,*LandArea;
short *AEZ;
unsigned char *protect,protectID;
float TotalProtectedArea;
char header[100];
char filename[100];
char buf[BUFSIZE];
char *fn;

double **GridID_LU,**GridID_LU_Veg,**GridID_LU_Area;
long int i,j;
int GCAMclass[16];
float vegCarbon[16];
int AEZ_ID,LU_Combination;
double totalSurfaceArea,level2;

double *Summary_LU_combination,*Summary_AEZ,*Summary_Protected_Soil_GCAM,*Summary_unProtected_Soil_GCAM;
double *Summary_LU_combination_Veg,*Summary_AEZ_Veg,*Summary_Protected_Veg_GCAM,*Summary_unProtected_Veg_GCAM;
double *Summary_LU_combination_Area,*Summary_AEZ_Area,*Summary_Protected_Area_GCAM,*Summary_unProtected_Area_GCAM;

int summaryID;
int GCAM,AEZ2GCAM[16012],tempID,tempAEZ;
float totalArea;
float tempCarbon;

        sage_class = (int *) malloc(4320*2160 * sizeof( int));
        soil = (float *) malloc(4320*2160 * sizeof(float));

        AEZ = (short *) malloc(4320*2160 * sizeof(short));
        protect = (unsigned char *) malloc(4320*2160 * sizeof(unsigned char));
        LandArea = (float *) malloc(4320*2160 * sizeof(float));

		GridID_LU = (double **) malloc(16012 * sizeof(double *)); //region
		for ( i = 0 ; i < 16012 ; i++ )
		{
			GridID_LU[i] = (double *) malloc( 153* sizeof(double));
		}

		GridID_LU_Veg = (double **) malloc(16012 * sizeof(double *)); //region
		for ( i = 0 ; i < 16012 ; i++ )
		{
			GridID_LU_Veg[i] = (double *) malloc( 153* sizeof(double));
		}

		GridID_LU_Area = (double **) malloc(16012 * sizeof(double *)); //region
		for ( i = 0 ; i < 16012 ; i++ )
		{
			GridID_LU_Area[i] = (double *) malloc( 153* sizeof(double));
		}

		Summary_LU_combination = (double *) malloc(153 * sizeof(double)); //Lu_combinations
		Summary_AEZ = (double *) malloc(16012 * sizeof(double)); //AEZ
        Summary_Protected_Soil_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM
        Summary_unProtected_Soil_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM

		Summary_LU_combination_Veg = (double *) malloc(153 * sizeof(double)); //Lu_combinations
		Summary_AEZ_Veg = (double *) malloc(16012 * sizeof(double)); //AEZ
        Summary_Protected_Veg_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM
        Summary_unProtected_Veg_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM

		Summary_LU_combination_Area = (double *) malloc(153 * sizeof(double)); //Lu_combinations
		Summary_AEZ_Area = (double *) malloc(16012 * sizeof(double)); //AEZ
        Summary_Protected_Area_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM
        Summary_unProtected_Area_GCAM = (double *) malloc(15 * sizeof(double)); //GCAM



                fn = find_file(buf, "GISCODEDIR", "input_Soil_Carbon.txt", BUFSIZE);
		fileinname =fopen(fn,"rb"); //float
		if(fileinname==NULL)
		{
                  printf("cannot open file %s for input\n",fn);
                  exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		FileLandArea=fopen(fn,"rb"); //float
		if(FileLandArea==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
                fileinAEZ_2_GCAM=fopen(fn,"r");
		if(fileinAEZ_2_GCAM==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
                fileinVegCarbon=fopen(fn,"r");
		if(fileinVegCarbon==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinAEZ=fopen(fn,"rb"); //int
		if(fileinAEZ==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinProtected=fopen(fn,"rb"); //byte
		if(fileinProtected==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinSage=fopen(fn,"rb"); //float
		if(fileinSage==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinSoil=fopen(fn,"rb"); //float
		if(fileinSoil==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileout=fopen(fn,"w"); //float
		if(fileout==NULL)
		{
		   printf("cannot open file %s for output\n",fn);
		   exit(2);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileout_Summary=fopen(fn,"w"); //float
		if(fileout_Summary==NULL)
		{
		   printf("cannot open file %s for output\n",fn);
		   exit(2);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileoutSoil_GCAM=fopen(fn,"w"); //float
		if(fileoutSoil_GCAM==NULL)
		{
		   printf("cannot open file %s for output\n",fn);
		   exit(2);
		}



   GCAMclass[0]=0;
   GCAMclass[1]=1;
   GCAMclass[2]=1;
   GCAMclass[3]=1;
   GCAMclass[4]=1;
   GCAMclass[5]=1;
   GCAMclass[6]=1;
   GCAMclass[7]=1;
   GCAMclass[8]=1;
   GCAMclass[9]=2;
   GCAMclass[10]=2;
   GCAMclass[11]=3;
   GCAMclass[12]=3;
   GCAMclass[13]=4;
   GCAMclass[14]=5;
   GCAMclass[15]=5;


		fread(LandArea,sizeof(float),4320*2160,FileLandArea);
		fread(AEZ,sizeof(short),4320*2160,fileinAEZ);
		//fread(sage_class,sizeof(float),4320*2160,fileinSage); //changed by yuyu on 6/20/2011
		fread(sage_class,sizeof(int),4320*2160,fileinSage);

		fread(protect,sizeof(unsigned char),4320*2160,fileinProtected);
		fread(soil,sizeof(float),4320*2160,fileinSoil);




		fscanf(fileinAEZ_2_GCAM,"%s %s",header,header);
		while(fscanf(fileinAEZ_2_GCAM,"%d %d",&tempAEZ,&tempID)!=EOF)
		{
			AEZ2GCAM[tempAEZ] = tempID;
		}


		fscanf(fileinVegCarbon,"%s %s",header,header);
		vegCarbon[0] =0.0;
		while(fscanf(fileinVegCarbon,"%d %f",&sage_ID,&tempCarbon)!=EOF)
		{
			vegCarbon[sage_ID] = tempCarbon;
		}

        fprintf(fileout,"AEZ_ID,Category,SoilCarbon(pg),VegCarbon(pg),Area(Km2)\n");
        summaryID=0;


					TotalProtectedArea=0.0f;
					totalSurfaceArea=totalArea=0;
					   printf("LandArea=%f,AEZ=%d,sage_class=%d,protect=%d\n, soil = %f\n",LandArea[2785564],AEZ[2785564],sage_class[2785564],protect[2785564],soil[2785564]);

					for(i=0;i<16012;i++){
                        Summary_AEZ[i]=0;
						Summary_AEZ_Veg[i]=0;
						Summary_AEZ_Area[i]=0;
						for(j=0;j<153;j++)
						{
							GridID_LU[i][j]=0;
							GridID_LU_Veg[i][j]=0;
							GridID_LU_Area[i][j]=0;
							Summary_LU_combination[j]=0;
							Summary_LU_combination_Veg[j]=0;
							Summary_LU_combination_Area[j]=0;
						}
					}

					for(i=0;i<15;i++){
						Summary_Protected_Soil_GCAM[i]=0;
						Summary_unProtected_Soil_GCAM[i]=0;
						Summary_Protected_Veg_GCAM[i]=0;
						Summary_unProtected_Veg_GCAM[i]=0;
						Summary_Protected_Area_GCAM[i]=0;
						Summary_unProtected_Area_GCAM[i]=0;
					}


					for(i=0;i<4320*2160;i++){

						AEZ_ID = AEZ[i];
						if(AEZ_ID<0)
							AEZ_ID=0;

						sage_ID = sage_class[i];
						if(sage_ID<0)
							sage_ID=0;

						if(protect[i]==255)
						   protectID = 2; //unprotected
						else if(protect[i]==1)
						   protectID = 1; //protected
						else
						   protectID = 0; //others

						level2 = 0;


						  if(LandArea[i]>0)
							totalArea = totalArea+ LandArea[i];

						if(LandArea[i]>0 &&  soil[i]>=0)
						{


  							LU_Combination = sage_ID *10 + protectID; //nonCrop nonGrass nonUrban
							//soil unit: kg/m2, area unit: km2
                            GridID_LU[AEZ_ID][LU_Combination] = GridID_LU[AEZ_ID][LU_Combination] + LandArea[i]*soil[i]*1000*1000/1000000000000;//carbon in Pg
                            GridID_LU_Veg[AEZ_ID][LU_Combination] = GridID_LU_Veg[AEZ_ID][LU_Combination] + LandArea[i]*vegCarbon[sage_ID]*1000*1000/1000000000000;//carbon in Pg
                            GridID_LU_Area[AEZ_ID][LU_Combination] = GridID_LU_Area[AEZ_ID][LU_Combination] + LandArea[i];//carbon in Km2



								if(protectID==1 && LandArea[i]>0)
								{
									TotalProtectedArea = TotalProtectedArea+ LandArea[i];
								}
									GCAM = AEZ2GCAM[AEZ_ID];
								if(protectID==1 && GCAM>0)
								{
									Summary_Protected_Soil_GCAM[GCAM] = Summary_Protected_Soil_GCAM[GCAM] +LandArea[i]*soil[i]*1000*1000/1000000000000;
							        Summary_Protected_Veg_GCAM[GCAM] = Summary_Protected_Veg_GCAM[GCAM] +LandArea[i]*vegCarbon[sage_ID]*1000*1000/1000000000000;
							        Summary_Protected_Area_GCAM[GCAM] = Summary_Protected_Area_GCAM[GCAM] +LandArea[i];
								}
								if(protectID==2 && GCAM>0)
								{
									Summary_unProtected_Soil_GCAM[GCAM] = Summary_unProtected_Soil_GCAM[GCAM] +LandArea[i]*soil[i]*1000*1000/1000000000000;
									Summary_unProtected_Veg_GCAM[GCAM] = Summary_unProtected_Veg_GCAM[GCAM] +LandArea[i]*vegCarbon[sage_ID]*1000*1000/1000000000000;
									Summary_unProtected_Area_GCAM[GCAM] = Summary_unProtected_Area_GCAM[GCAM] +LandArea[i];
								}

						}

					}//end i





					for(i=1;i<16012;i++){
						for(j=0;j<153;j++)
							if(GridID_LU[i][j]>0 || GridID_LU_Veg[i][j]>0 || GridID_LU_Area[i][j]>0){
								 Summary_LU_combination[j] = Summary_LU_combination[j] + GridID_LU[i][j];
					             Summary_LU_combination_Veg[j] = Summary_LU_combination_Veg[j] + GridID_LU_Veg[i][j];
					             Summary_LU_combination_Area[j] = Summary_LU_combination_Area[j] + GridID_LU_Area[i][j];
								 fprintf(fileout,"%d,%d,%lf,%lf,%lf\n",i,j,GridID_LU[i][j],GridID_LU_Veg[i][j],GridID_LU_Area[i][j]);
							}
					}


					for(j=0;j<153;j++)
					  for(i=1;i<16012;i++)
						 if(GridID_LU[i][j]>0 || GridID_LU_Veg[i][j]>0){
                                 Summary_AEZ[i] = Summary_AEZ[i] + GridID_LU[i][j];
                                 Summary_AEZ_Veg[i] = Summary_AEZ_Veg[i] + GridID_LU_Veg[i][j];
                                 Summary_AEZ_Area[i] = Summary_AEZ_Area[i] + GridID_LU_Area[i][j];
							}

					printf(" P=%8.3lf, All=%8.3lf, unit: 1000 km^2\n",TotalProtectedArea/1000,totalArea/1000);


      		fprintf(fileoutSoil_GCAM,"GCAM_Region,Protected_Soil(pgC),unProtected_Soil(pgC),Protected_Veg(pgC),unProtected_Veg(pgC),Protected_Area(Km2),unProtected_Area(Km2)\n");

		for(i=1;i<15;i++){
      	   	fprintf(fileoutSoil_GCAM,"%d,%f,%f,%f,%f,%f,%f\n",i,Summary_Protected_Soil_GCAM[i],Summary_unProtected_Soil_GCAM[i],Summary_Protected_Veg_GCAM[i],Summary_unProtected_Veg_GCAM[i],Summary_Protected_Area_GCAM[i],Summary_unProtected_Area_GCAM[i]);
		}


				//	printf("Prot= %f,crop = %f, grass = %f, Urban = %f \n",TotalProtectedArea,totalCropArea,totalGrassArea,totalUrbanArea);
      	fprintf(fileout_Summary,"Summary for Type\n");
      	fprintf(fileout_Summary,"Type,SoilCarbon(pgC),VegCarbon(pgC),Area(Km2)\n");


		for(i=0;i<153;i++)
		{
			if(Summary_LU_combination[i]>0 || Summary_LU_combination_Veg[i]>0 || Summary_LU_combination_Area[i]>0)
			{
      			fprintf(fileout_Summary,"%d",i);
      			fprintf(fileout_Summary,",%f,%f,%f\n",Summary_LU_combination[i],Summary_LU_combination_Veg[i],Summary_LU_combination_Area[i]);
			}
		}
      		fprintf(fileout_Summary,"\n");       		fprintf(fileout_Summary,"\n");
      	fprintf(fileout_Summary,"Summary for AEZ\n");
      	fprintf(fileout_Summary,"AEZ_ID,SoilCarbon(pgC),VegCarbon(pgC),Area(Km2)\n");
		for(i=0;i<16012;i++)
		{
			if(Summary_AEZ[i]>0 || Summary_AEZ_Veg[i]>0 || Summary_AEZ_Area[i] )
			{
      			fprintf(fileout_Summary,"%d",i);
     			fprintf(fileout_Summary,",%f,%f,%f\n",Summary_AEZ[i],Summary_AEZ_Veg[i],Summary_AEZ_Area[i]);
			}
		}



	//fcloseall();

    free(sage_class);free(soil);free(protect);free(LandArea);free(GridID_LU);

    return 0;
}



