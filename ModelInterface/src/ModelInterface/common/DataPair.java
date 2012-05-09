/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
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

import java.util.Map;

/**
 * Simple implementation of Map.Entry intended to
 * just put to related values together so that they
 * can be stuffed into some datastructure.  The need for 
 * something like this happens more often then expected.
 * @author Pralit Patel
 */ 
public class DataPair<K,V> implements Map.Entry<K,V>, java.io.Serializable {
	/**
	 * Key.
	 */
	private K key;

	/**
	 * Value.
	 */ 
	private V value; 

	/**
	 * Default Constructor.
	 */
	public DataPair() {
	}

	/**
	 * Constructor that takes initial values for the pair.
	 * @param k Initial key.
	 * @param v Initial value. 
	 */
	public DataPair(K k, V v) {
		key = k;
		value = v;
	}

	/**
	 * Get the key.
	 * @return The key.
	 */
	public K getKey() {
		return key;
	}

	/**
	 * Get the value.
	 * @return The value.
	 */
	public V getValue() {
		return value;
	}

	/**
	 * Set the key.
	 * @param k New key.
	 * @return Old key.
	 */
	public K setKey(K k) {
		K oldK = key;
		key = k;
		return oldK;
	}

	/**
	 * Set the value.
	 * @param v New value.
	 * @return Old value.
	 */
	public V setValue(V v) {
		V oldV = value;       
		value = v;
		return oldV;
	}

	public boolean equals(Object o) {
	       if(o == null || !(o instanceof DataPair)) {
		       return false;
	       }
	       DataPair d = (DataPair)o;
	       return key.equals(d.key) && value.equals(d.value);
	}

	public int hashCode() {
		return (key != null? key.hashCode() : 0) ^ (value != null? value.hashCode() : 0);
	}

	public String toString() {
		return "DataPair["+key+"; "+value+"]";
	}
}
