Page Kyle
April 29, 2011

Inventory Data Folders:

FAO	Food and Agriculture Organization of the United Nations. Data on production and consumption of all agricultural, animal, and forestry commodities
	http://faostat.fao.org/
	Also contains detailed projections (CROSIT) provided by Jelle Bruinsma, FAO. Data documented in Bruinsma 2009, The Resource Outlook to 2050.
	ftp://ftp.fao.org/docrep/fao/012/ak971e/ak971e00.pdf
GIS	Contains output of GIS data processing of SAGE (natural vegetation), HYDE (cropland, pasture, urban land), WDPA (protected areas), and IGBP (soil carbon densities)
	SAGE: http://www.sage.wisc.edu/download/potveg/global_potveg.html
	HYDE: http://eos-webster.sr.unh.edu/data_guides/glm_dg.jsp;jsessionid=DC80F15089214C105282985032B703DE 
	WDPA: http://www.wdpa.org/
	IGBP: http://daac.ornl.gov/SOILS/guides/igbp-surfaces.html
GTAP	Global Trade Analysis Project, GTAP Land Use Data Base, Release 2.1, July 9, 2009.
 	https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=1900
IMAGE	Integrated assessment model of the Netherlands Environmental Assessment Agency. Detailed data on animal production systems.
	http://themasites.pbl.nl/en/themasites/image/index.html
USDA	United States Department of Agriculture; data on costs of crop production by USA subregion
	http://www.ers.usda.gov/Data/CostsAndReturns/testpick.htm

Each folder contains at least one Excel workbook that is used to build CSV files that are saved in the Rdata_in subfolder, from which they are accessed by R. Raw data files are also preserved, where available.