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
