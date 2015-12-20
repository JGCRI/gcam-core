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
package ModelInterface.common;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * A map implementation which can be used as a simple least recently used cache.
 * As per the api documentation for LinkedHashMap this class simply overrides the
 * removeEldestEntry method to enforce a user given maximum entry size for the cache.
 * The rest of the functionality is handled by LinkedHashMap.
 * @see java.util.LinkedHashMap
 * @author Pralit Patel
 */ 
public class LRUCacheMap<K,V> extends LinkedHashMap<K,V> {
	/**
	 * The maximum number of entries that will be allowed in this map.
	 */ 
	private final int maxEntries; 

	/**
	 * Constructor which takes the maximum entries this cache will hold.
	 */
	public LRUCacheMap(int maxEntries) {
		// TODO: Should I go ahead and set the initial size of the map to the
		// maxEntries.
		// Using the same default value for load factor from LinkedHashMap, and
		// setting the accessOrder to true implies least recently used behavoir
		super(maxEntries, (float).75, true);
		this.maxEntries = maxEntries;
	}

	protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
		return size() > maxEntries;
	}
}
