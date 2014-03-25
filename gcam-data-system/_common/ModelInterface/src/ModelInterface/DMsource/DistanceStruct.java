/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2012 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
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
*/
package ModelInterface.DMsource;

/**
 * A simple data structure useful when calculating minimum distance from one map to another.
 * This data structure represents a single grid cell and internalizes the coordinates of
 * this cell in latitude and longitude (in radians), a pointer to the underlying matrices
 * which keep track of the minimum distance and region ID mask, x and y indicies to index
 * into the underlying matrices, and the weight of the region from the closest grid cell.
 * This allows us to keep all of the relavant information together when calculating minimum
 * distance to grid.  A couple of accessor are given to the current minimum distance to grid,
 * the region weight of the current closest grid cell, the ability to set a new minimum distance,
 * and a util to calculate the distance to this grid cell.  Note that if null is given for the
 * parentRegionMask it will be assumed that the user just did not want to maintain that information.
 * The data wrapper is always required however.
 * @author Pralit Patel
 */
public class DistanceStruct {
	/**
	 * Latitude of this representative grid cell in radians.
	 */
	private final double latRadians;
	/**
	 * Longitude of this representative grid cell in radians.
	 */
	private final double lonRadians;
	/**
	 * Pointer to the data matrix which will keep track of the current minimum distance.
	 */
	private final Wrapper parentData;
	/**
	 * Pointer to the data matrix which will keep track of the region ID of the closest
	 * region to this grid cell.  Note that a null pointer for this value will just
	 * imply this information will not be maintained.
	 */
	private final Wrapper parentRegionMask;
	/**
	 * The row index into the parentData and parentRegionMask matrix which maps to
	 * latRadians.
	 */
	private final int yIndex;
	/**
	 * The column index into the parentData and parentRegionMask matrix which maps to
	 * lonRadians.
	 */
	private final int xIndex;
	/**
	 * The region weight of the current closest grid cell to this one.
	 */
	private double currRegionWeight;
	/**
	 * Constructor to initialize values.  Note no consistency checks are done here.
	 * @param latRadians Latitude in radians to represent.
	 * @param lonRadians Longitude in radians to represent.
	 * @param parentData Pointer to the data matrix to update min distances to.
	 * @param parentRegionMask Pointer to the matrix to update closest region IDs. Note null
	 * 			   is allowed and implies this matrix will not be updated.
	 * @param yIndex The index into the matrices for the given latRadians.
	 * @param xIndex The index into the matrices for the given lonRadians.
	 */
	public DistanceStruct(double latRadians, double lonRadians, Wrapper parentData, Wrapper parentRegionMask, int yIndex, int xIndex) {
		this.latRadians = latRadians;
		this.lonRadians = lonRadians;
		this.parentData = parentData;
		this.parentRegionMask = parentRegionMask;
		this.yIndex = yIndex;
		this.xIndex = xIndex;
		currRegionWeight = 0;
	}
	/**
	 * Returns the current value set for the minimum distance.
	 * @return Current value from the data matrix
	 */
	public double getCurrMinDistance() {
		return parentData.getData()[yIndex][xIndex];
	}
	/**
	 * Returns the current region weight from the closest cell to
	 * this one.
	 * @return Current region weight set.
	 */
	public double getCurrRegionWeight() {
		return currRegionWeight;
	}
	/**
	 * Sets a new minmum distance to this grid cell.  Note that no checking of the
	 * given values are performed for performance and flexability reasons.
	 * @param newDistance New distance value to be set into the data matrix.
	 * @param regionID New region ID to be set into the region mask matrix.
	 * @param regionWeight New region weight to set for the new distance.
	 */
	public void setDistance(double newDistance, double regionID, double regionWeight) {
		parentData.getData()[yIndex][xIndex] = newDistance;
		if(parentRegionMask != null) {
			parentRegionMask.getData()[yIndex][xIndex] = regionID;
		}
		currRegionWeight = regionWeight;
	}
	/**
	 * Utility method to compute the distance in kilometers to this grid cell from the
	 * given coordinates.
	 * @param latRadiansTo Latitude in radians of the point to calculate distance to.
	 * @param lonRadiansTo Longitude in radians of the point to calculate distance to.
	 * @return The distance to the given point.
	 */
	public double getDistanceTo(double latRadiansTo, double lonRadiansTo) {
		// constant of the radius of the earth in kilometers
		final double earthRadius = 6371;
		// spherical law of cosines
		return Math.abs(Math.acos(Math.sin(latRadiansTo)*Math.sin(latRadians)+Math.cos(latRadiansTo)*Math.cos(latRadians)*Math.cos(lonRadians - lonRadiansTo)) * earthRadius);
	}
}
