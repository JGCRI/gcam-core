#include <stdlib.h>
#include <stdio.h>
#include <math.h>
//#include <unistd.h>
#include "find_file.h"

#define PI 3.1415926
#define RADIUS 6370997.00

#define BUFSIZE 1024

float AreaFromLonLat(float,float);
int main() 
{

FILE *fileinAEZ,*fileinProtected,*fileinHydeUrban,*fileinHydeCrop,*fileinHydeGrass,*fileinSage, *fileout;
FILE *fileinname;
FILE *FileLandArea;
FILE *fileout_Summary;
FILE *fileinAEZ_2_GCAM,*fileoutHYDE_GCAM;
int *sage_class,sage_ID;
float *crop,*grass,*urban,*LandArea;
short *AEZ;
unsigned char *protect,protectID;
unsigned char *protect_test;
//unsigned int *protect,protectID;//updated
float TotalProtectedArea,totalCropArea, totalGrassArea,totalUrbanArea;
char header[100];

char filename[100];
char buf[BUFSIZE];
char *fn;

double **GridID_LU;
long int i,j;
int GCAMclass[16];
int AEZ_ID,LU_Combination;
int Year;
double totalSurfaceArea,level2;
//float gridPercentage;

double **Summary_LU_combination,**Summary_AEZ;
double **Summary_Protected_Crop_GCAM,**Summary_unProtected_Crop_GCAM,**Summary_Protected_Grass_GCAM,**Summary_unProtected_Grass_GCAM;
int summaryID;
int GCAM,AEZ2GCAM[16012],tempID,tempAEZ;

        sage_class = (int *) malloc(4320*2160 * sizeof(int));
        crop = (float *) malloc(4320*2160 * sizeof(float));
        grass = (float *) malloc(4320*2160 * sizeof(float));
        urban = (float *) malloc(4320*2160 * sizeof(float));

        AEZ = (short *) malloc(4320*2160 * sizeof(short));
        protect = (unsigned char *) malloc(4320*2160 * sizeof(unsigned char));
        protect_test = (unsigned char *) malloc(4320*2160 * sizeof(unsigned char));

        LandArea = (float *) malloc(4320*2160 * sizeof(float));

		GridID_LU = (double **) malloc(16012 * sizeof(double *)); //region
		for ( i = 0 ; i < 16012 ; i++ )
		{
			GridID_LU[i] = (double *) malloc( 236* sizeof(double));
		}

		Summary_LU_combination = (double **) malloc(236 * sizeof(double *)); //Lu_combinations
		for ( i = 0 ; i < 236 ; i++ )
		{
			Summary_LU_combination[i] = (double *) malloc( 32* sizeof(double));  //time period
		}

		Summary_AEZ = (double **) malloc(16012 * sizeof(double *)); //AEZ
		for ( i = 0 ; i < 16012 ; i++ )
		{
			Summary_AEZ[i] = (double *) malloc( 32* sizeof(double)); //time period
		}

		Summary_Protected_Crop_GCAM = (double **) malloc(15 * sizeof(double *)); //GCAM region
		for ( i = 0 ; i < 15 ; i++ )
		{
			Summary_Protected_Crop_GCAM[i] = (double *) malloc( 32* sizeof(double)); //time period
		}

		Summary_unProtected_Crop_GCAM = (double **) malloc(15 * sizeof(double *)); //GCAM region
		for ( i = 0 ; i < 15 ; i++ )
		{
			Summary_unProtected_Crop_GCAM[i] = (double *) malloc( 32* sizeof(double)); //time period
		}

		Summary_Protected_Grass_GCAM = (double **) malloc(15 * sizeof(double *)); //GCAM region
		for ( i = 0 ; i < 15 ; i++ )
		{
			Summary_Protected_Grass_GCAM[i] = (double *) malloc( 32* sizeof(double)); //time period
		}

		Summary_unProtected_Grass_GCAM = (double **) malloc(15 * sizeof(double *)); //GCAM region
		for ( i = 0 ; i < 15 ; i++ )
		{
			Summary_unProtected_Grass_GCAM[i] = (double *) malloc( 32* sizeof(double)); //time period
		}


                fn = find_file(buf,"GISCODEDIR", "input_SAGE5_HYDE_Area.txt", BUFSIZE);
		fileinname =fopen(fn,"r"); //float
		if(fileinname==NULL)
		{
		   printf("cannot open file input_SAGE5_HYDE_Area.txt for input\n");
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
                FileLandArea =fopen(fn,"rb"); //float
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
		fileinHydeUrban=fopen(fn,"rb"); //float
		if(fileinHydeUrban==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}

                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinHydeCrop=fopen(fn,"rb"); //float
		if(fileinHydeCrop==NULL)
		{
		   printf("cannot open file %s for input\n",fn);
		   exit(1);
		}


                fscanf(fileinname,"%s %s\n",header,filename);
                fn = find_file(buf,"AGLUDAT",filename,BUFSIZE);
		fileinHydeGrass=fopen(fn,"rb"); //float
		if(fileinHydeGrass==NULL)
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
		fileoutHYDE_GCAM=fopen(fn,"w"); //float
		if(fileoutHYDE_GCAM==NULL)
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
		fread(sage_class,sizeof(int),4320*2160,fileinSage);
		fread(protect,sizeof(unsigned char),4320*2160,fileinProtected);



		for(i=0;i<15;i++){
			for(j=0;j<32;j++){
				Summary_Protected_Crop_GCAM[i][j]=0;
				Summary_unProtected_Crop_GCAM[i][j]=0;
				Summary_Protected_Grass_GCAM[i][j]=0;
				Summary_unProtected_Grass_GCAM[i][j]=0;
			}
		}

		fscanf(fileinAEZ_2_GCAM,"%s %s",header,header);
		while(fscanf(fileinAEZ_2_GCAM,"%d %d",&tempAEZ,&tempID)!=EOF)
		{
			AEZ2GCAM[tempAEZ] = tempID;
		}


        fprintf(fileout,"AEZ_ID,Category,Year,Area(km2)\n");
        summaryID=0;

		for(Year=1700;Year<2006;)
		{
					TotalProtectedArea=totalCropArea=totalGrassArea=totalUrbanArea=0.0f;
					totalSurfaceArea=0;
					fread(grass,sizeof(float),4320*2160,fileinHydeGrass);
					fread(urban,sizeof(float),4320*2160,fileinHydeUrban);
					fread(crop,sizeof(float),4320*2160,fileinHydeCrop);


				    if(Year==1700)
					{
					   printf("LandArea=%f,AEZ=%d,sage_class=%d,protect=%d\n%f,%f,%f\n",LandArea[2785564],AEZ[2785564],sage_class[2785564],protect[2785564],crop[2785564],grass[2785564],urban[2785564]);

					}


					for(i=0;i<16012;i++){
                        Summary_AEZ[i][summaryID]=0;
						for(j=0;j<236;j++)
						{
							GridID_LU[i][j]=0;
							Summary_LU_combination[j][summaryID]=0;
						}
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



						if(LandArea[i]>0 &&  crop[i]>=0 &&  grass[i]>=0 &&  urban[i]>=0)
						{



  							LU_Combination = protectID*100 + 0 + GCAMclass[sage_ID]; //nonCrop nonGrass nonUrban
							GridID_LU[AEZ_ID][LU_Combination] = GridID_LU[AEZ_ID][LU_Combination] + (LandArea[i]-crop[i]-grass[i]-urban[i]);



  							LU_Combination = protectID*100 + 10 + GCAMclass[sage_ID]; //crop
							GridID_LU[AEZ_ID][LU_Combination] = GridID_LU[AEZ_ID][LU_Combination] + crop[i];



							LU_Combination = protectID*100 + 20 + GCAMclass[sage_ID]; //grass
							GridID_LU[AEZ_ID][LU_Combination] = GridID_LU[AEZ_ID][LU_Combination] + grass[i];



							LU_Combination = protectID*100 + 30 + GCAMclass[sage_ID]; //urban
							GridID_LU[AEZ_ID][LU_Combination] = GridID_LU[AEZ_ID][LU_Combination] + urban[i];


								totalCropArea = totalCropArea+ crop[i];
								totalGrassArea = totalGrassArea+ grass[i];
								totalUrbanArea = totalUrbanArea+ urban[i];

								if(protectID==1 && LandArea[i]>0)
								{
									TotalProtectedArea = TotalProtectedArea+ LandArea[i];
								}

									GCAM = AEZ2GCAM[AEZ_ID];
								if(protectID==1 && GCAM>0)
								{
									Summary_Protected_Crop_GCAM[GCAM][summaryID] = Summary_Protected_Crop_GCAM[GCAM][summaryID] +crop[i];
									Summary_Protected_Grass_GCAM[GCAM][summaryID]  = Summary_Protected_Grass_GCAM[GCAM][summaryID]+ grass[i];

								}
								if(protectID==2 && GCAM>0)
								{
									Summary_unProtected_Crop_GCAM[GCAM][summaryID] = Summary_unProtected_Crop_GCAM[GCAM][summaryID] +crop[i];
									Summary_unProtected_Grass_GCAM[GCAM][summaryID]  = Summary_unProtected_Grass_GCAM[GCAM][summaryID]+ grass[i];
								}

						}




					}//end i




					for(i=0;i<16012;i++){
						for(j=0;j<236;j++)
							if(GridID_LU[i][j]>0){
					             Summary_LU_combination[j][summaryID] = Summary_LU_combination[j][summaryID] + GridID_LU[i][j];
								 fprintf(fileout,"%d,%d,%d,%lf\n",i,j,Year,GridID_LU[i][j]);
							}

					}
					for(j=0;j<236;j++)
					  for(i=0;i<16012;i++)
						 if(GridID_LU[i][j]>0)
                            Summary_AEZ[i][summaryID] = Summary_AEZ[i][summaryID] + GridID_LU[i][j];

					printf("in %d, P=%8.3lf,crp=%8.3lf, grs=%8.3lf, U=%5.3lf, unit: 1000 km^2\n",Year,TotalProtectedArea/1000,totalCropArea/1000,totalGrassArea/1000,totalUrbanArea/1000);

				//	printf("total surface area is %f km2 in year %d\n",totalSurfaceArea,Year);
                    if(Year == 2000)
						Year = Year +5;
					else
						Year =Year + 10;
					summaryID++;

		}


      		fprintf(fileoutHYDE_GCAM,"GCAM_Region,Type");
			for(Year=1700;Year<2006;){
      	    	fprintf(fileoutHYDE_GCAM,",%d",Year);
				    if(Year == 2000)
						Year = Year +5;
					else
						Year =Year + 10;
			}
      		fprintf(fileoutHYDE_GCAM,"\n");
		for(i=1;i<15;i++){
      		fprintf(fileoutHYDE_GCAM,"%d",i);
			fprintf(fileoutHYDE_GCAM,",unProtected_crop");
			for(j=0;j<32;j++){
      	    	fprintf(fileoutHYDE_GCAM,",%f",Summary_unProtected_Crop_GCAM[i][j]);
			}
      		fprintf(fileoutHYDE_GCAM,"\n");
		}
		      		fprintf(fileoutHYDE_GCAM,"\n");
		for(i=1;i<15;i++){
      		fprintf(fileoutHYDE_GCAM,"%d",i);
			fprintf(fileoutHYDE_GCAM,",Protected_crop");
			for(j=0;j<32;j++){
      	    	fprintf(fileoutHYDE_GCAM,",%f",Summary_Protected_Crop_GCAM[i][j]);
			}
      		fprintf(fileoutHYDE_GCAM,"\n");
		}
		      		fprintf(fileoutHYDE_GCAM,"\n");       		fprintf(fileoutHYDE_GCAM,"\n");


		for(i=1;i<15;i++){
      		fprintf(fileoutHYDE_GCAM,"%d",i);
			fprintf(fileoutHYDE_GCAM,",unProtected_grass");
			for(j=0;j<32;j++){
      	    	fprintf(fileoutHYDE_GCAM,",%f",Summary_unProtected_Grass_GCAM[i][j]);
			}
      		fprintf(fileoutHYDE_GCAM,"\n");
		}
		      		fprintf(fileoutHYDE_GCAM,"\n");
		for(i=1;i<15;i++){
      		fprintf(fileoutHYDE_GCAM,"%d",i);
			fprintf(fileoutHYDE_GCAM,",Protected_grass");
			for(j=0;j<32;j++){
      	    	fprintf(fileoutHYDE_GCAM,",%f",Summary_Protected_Grass_GCAM[i][j]);
			}
			      		fprintf(fileoutHYDE_GCAM,"\n");
		}

				//	printf("Prot= %f,crop = %f, grass = %f, Urban = %f \n",TotalProtectedArea,totalCropArea,totalGrassArea,totalUrbanArea);
      	fprintf(fileout_Summary,"Summary for Type\n");
      	fprintf(fileout_Summary,"Type");
		for(Year=1700;Year<2006;)
		{
      		fprintf(fileout_Summary,",%d",Year);
			        if(Year == 2000)
						Year = Year +5;
					else
						Year =Year + 10;
		}
      		fprintf(fileout_Summary,"\n");

		for(i=0;i<236;i++)
		{
			if(Summary_LU_combination[i][31]>0 || Summary_LU_combination[i][0]>0 || Summary_LU_combination[i][15]>0)
			{
      			fprintf(fileout_Summary,"%d",i);
				for(j=0;j<32;j++)
				{
      				fprintf(fileout_Summary,",%f",Summary_LU_combination[i][j]);
				}
      		    fprintf(fileout_Summary,"\n");
			}
		}
      		fprintf(fileout_Summary,"\n");       		fprintf(fileout_Summary,"\n");
      	fprintf(fileout_Summary,"Summary for AEZ\n");
      	fprintf(fileout_Summary,"AEZ_ID");
		for(Year=1700;Year<2006;)
		{
      		fprintf(fileout_Summary,",%d",Year);
			        if(Year == 2000)
						Year = Year +5;
					else
						Year =Year + 10;
		}
      		fprintf(fileout_Summary,"\n");

		for(i=0;i<16012;i++)
		{
			if(Summary_AEZ[i][31]>0 || Summary_AEZ[i][0]>0 || Summary_AEZ[i][15]>0)
			{
      			fprintf(fileout_Summary,"%d",i);
				for(j=0;j<32;j++)
				{
      				fprintf(fileout_Summary,",%f",Summary_AEZ[i][j]);
				}
      		    fprintf(fileout_Summary,"\n");
			}
		}



	//fcloseall();

    free(sage_class);free(crop);free(grass);free(urban);free(AEZ);free(protect);free(LandArea);free(GridID_LU);

    return 0;
}



