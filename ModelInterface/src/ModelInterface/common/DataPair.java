package ModelInterface.common;

import java.util.Map;

/**
 * Simple implementation of Map.Entry intended to
 * just put to related values together so that they
 * can be stuffed into some datastructure.  The need for 
 * something like this happens more often then expected.
 * TODO: Figure out if I should implement equals/hashCode 
 * @author Pralit Patel
 */ 
public class DataPair<K,V> implements Map.Entry<K,V> {
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

	// do I need to implement equals/hasCode?
}
