import java.util.*;
import java.util.StringTokenizer;

public class Headers {
	private int numCols;
	private ArrayList parentArr;
	private ArrayList childArr;
	private HashMap dataMap;
	//private HashMap attrMap;
	private HashMap tagAttrMap; // new: tag -> attrname
	private HashMap parentAttrMap; // new: tag\attrname -> int/str
	private Stack childStack;


	public Headers() {
		numCols = 0;
		parentArr = new ArrayList();
		childArr = new ArrayList();
		dataMap = new HashMap();
		//attrMap = new HashMap();
		tagAttrMap = new HashMap();
		parentAttrMap = new HashMap();
		childStack = new Stack();

	}
	public Headers ( String[] splitHeaders ) throws Exception{
		numCols = splitHeaders.length;
		StringTokenizer st;
		StringTokenizer stA; // for attribute splitting
		String parent;
		String child;
		String firstArg;
		String secArg;
		String parentPlaceHolder = null;
		parentArr = new ArrayList();
	        childArr = new ArrayList();
        	//attrMap = new HashMap();
        	tagAttrMap = new HashMap();
        	parentAttrMap = new HashMap();
        	dataMap = new HashMap();
		childStack = new Stack();
		ArrayList tempArr;
		ArrayList parentTemp = new ArrayList();
		for ( int i = 0; i < numCols; i++ ) {
			st = new StringTokenizer( splitHeaders[i] , "/", false);
			if (st.countTokens() == 1) {
				parent = null;
				if( (child = st.nextToken()) != null){
					child = child.trim();
				}

			}
			else if (st.countTokens() == 4) {
				parent = st.nextToken() + "/" + st.nextToken() + "/" + st.nextToken();
				child = st.nextToken().trim();
				if (parent.startsWith("{")) {
					parentTemp.add(parent.substring(parent.indexOf("}")+1,parent.indexOf('/')));
				} else {
					//parentTemp.add(parent.substring(0,parent.indexOf('/')));
				}
			}
			else if (st.countTokens() == 2) {
				if( (parent = st.nextToken()) != null ){
					parent = parent.trim(); // right now, don't parse parent
				}
				if( (child = st.nextToken()) != null){
					child = child.trim(); // one of three types
				}
				if ( parent.startsWith("{") ) {
					parentTemp.add(parent.substring(parent.indexOf("}")+1));
				}
			}
			else {
				child = null;
				parent = null;
				throw new Exception("Invalid Formated Header: "+splitHeaders[i]);
				//System.out.println("Invalid Formated Header: "+splitHeaders[i]);
				//System.exit(0);
			}
			if ( child.startsWith( "+{" ) && (child.indexOf("=") == -1)){ // just attr
				System.out.println("In headers, read attr");
				child = child.substring( 2, child.length() );
				stA = new StringTokenizer( child, "}", false);
				firstArg = stA.nextToken(); // holds attr_name
				child = stA.nextToken(); // holds the tag
				tagAttrMap.put(child, firstArg);
				parentAttrMap.put(parent + "\\" + child + "\\" + firstArg, new Integer( i ));
				//attrMap.put( child, new Integer( i ) );
			}

			else if (child.startsWith( "+{" )) { // attr and data, should start with '+{'
				stA = new StringTokenizer( child, "=", false);
				firstArg = stA.nextToken();
				firstArg = firstArg.substring( 2, firstArg.length());
				//firstArg now holds attr_name
				secArg = stA.nextToken();
				stA = new StringTokenizer( secArg, "}", false);
				secArg = stA.nextToken(); // holds attr_value
				child = stA.nextToken(); // holds the tag name
				tagAttrMap.put(child, firstArg );
				if (!parentAttrMap.containsKey(parent+ "\\" +child + "\\" + firstArg)) {
					tempArr = new ArrayList();
				}
				else {
					tempArr = (ArrayList)parentAttrMap.get(parent + "\\" + child+ "\\" + firstArg);
				}
				tempArr.add(secArg);
				parentAttrMap.put(parent + "\\" + child+ "\\" + firstArg, tempArr);
				dataMap.put(parent+ "\\" + child + "\\" + firstArg + "\\" +secArg, new Integer(i));
			}
			else if ( child.startsWith( "+" ) ) { // just data
				child = child.substring( 1, child.length() );
               			dataMap.put( parent + "\\" + child, new Integer( i ) );
			}
			else if (child.startsWith( "{")) { //specify atrr no data
				stA = new StringTokenizer( child, "=", false);
				firstArg = stA.nextToken();
				firstArg = firstArg.substring( 1, firstArg.length());
				secArg = stA.nextToken();
				stA = new StringTokenizer( secArg, "}", false);
				secArg = stA.nextToken(); // holds attr_value
				child = stA.nextToken(); // holds the tag name
				tagAttrMap.put(child, firstArg );
				//System.out.println(parent);
				/*
				if (parent.matches(".*\\..*")) {
					System.out.println("HERE in GP");
					parentPlaceHolder = parent;
					parent = parent.substring(parent.indexOf('.')+4);
					System.out.println("HERE "+parent);
				}
				*/
				if (parent.startsWith("{")) {
					parentAttrMap.put(parent + "\\" + child + "\\" + firstArg, secArg);
				} else {
				if (!parentAttrMap.containsKey(parent + "\\" + child + "\\" + firstArg)) {
					tempArr = new ArrayList();
				}
				else {
					tempArr = (ArrayList)parentAttrMap.get(parent + "\\" +child+ "\\" + firstArg);
				}
				tempArr.add(secArg);
				parentAttrMap.put(parent + "\\" + child+ "\\" + firstArg, tempArr);
				//parentAttrMap.put(parent + "\\" + child + "\\" + firstArg, secArg );
				//parentAttrMap.put(child + "\\" + firstArg, secArg );
				}
				if (parentPlaceHolder != null) {
					System.out.println("Switching back from "+parent+" to "+parentPlaceHolder);
					parent = parentPlaceHolder;
					parentPlaceHolder = null;
				}
			}
			parentArr.add( i, parent );
			childArr.add( i, child );
		}
		int temp;
		for (int i = 0; i < parentTemp.size(); i++) {
			//System.out.println("Parent temp: "+parentTemp.get(i));
			temp = childArr.indexOf((String)parentTemp.get(i));
			if (temp != -1 && !((String)childArr.get(temp)).startsWith("*")) {
				//System.out.println("Success for: "+(String)childArr.get(temp)+" at "+temp);
				child = "*" + (String)childArr.get(temp);
				childArr.set(temp, child);
			}
		}
	}

	public int getNumCols() {
		return numCols;
	}

	public String getParentHeader( int i ) {
		return (String)parentArr.get( i );
	}

	public String getChildHeader( int i ) {
		return (String)childArr.get( i );
	}

	public String isAttribute( String val ) {
		if ( !tagAttrMap.containsKey( val ) ) {
			return "";
		}
		return ((String)tagAttrMap.get( val ));
	}

	public Object getAttribute( String val ) {
		return parentAttrMap.get( val );

	}

	public int isData( String val ) {
                if ( !dataMap.containsKey( val ) ) {
                        return -1;
                }
                return ((Integer)dataMap.get( val )).intValue();
        }

	// pre: assumes headers data is already read in and only one of the headers is the root.
	// If no root is found for some reason, -1 is returned
	public int findRoot(){
		for(int i=0; i<numCols; i++){
			if( parentArr.get( i ) == null ) {
				return i;
			}
		}
		return -1;
	}

	public void addToChildStack( int i ) {
		childStack.push( new Integer( i ) );
	}

	public int checkChildStackFront() {
		if (!childStack.empty()) {
			return ((Integer)childStack.peek()).intValue();
		}
		return -1;
	}

	public void popChildStack() {
		if (!childStack.empty()) {
			childStack.pop();
		}
	}	

	// Given "World", returns an array of all the indices where "world" is the first part
	// Will match +World,-World,World
	public ArrayList getIndecesWhereParentHeaderIs( String word ){
		ArrayList returnvalue = new ArrayList();
		boolean dbgChk = false;
		if (word.equals("demandsector")) {
			//System.out.println("Start Debug");
			dbgChk = false;
		}
		for(int i=0; i<numCols; i++){
			if( parentArr.get( i ) != null ) {
				String strToTest = ((String)parentArr.get(i));
				if (strToTest.matches(".*\\.\\..*") && dbgChk) {
					System.out.println("Child stack: "+checkChildStackFront());
					System.out.println("the parentStr: "+strToTest);
					System.out.println("substred: "+strToTest.substring(strToTest.lastIndexOf('/')+1));
				}
				if (i == checkChildStackFront() && strToTest.substring(strToTest.lastIndexOf('/')+1).equals(word)) {
					if(dbgChk) {
					System.out.println("In some deal about child front: "+i);
					}
					returnvalue.add( new Integer ( i ) );
					continue;
				}
				if( strToTest.split("/").length > 1 && checkChildStackFront() == -1) {
				//if( strToTest.matches("[/]")) {
					if(dbgChk) {
					System.out.println("Some deal about gp: "+i);
					}
					strToTest = strToTest.substring(0,strToTest.indexOf("/"));
				}
				if (i == checkChildStackFront()) {
					strToTest = strToTest.substring(strToTest.lastIndexOf('/')+1);
				}
				if( strToTest.startsWith("{")){
					if(dbgChk) {
					System.out.println("Test start with atrr: "+i);
					}
					StringTokenizer st = new StringTokenizer( strToTest, "}", false);
					st.nextToken();
					String newStrToTest = st.nextToken();
					if(newStrToTest.equals(word)){
						if(dbgChk) {
						System.out.println("Test with attr succ: "+i);
						}
						returnvalue.add( new Integer( i ));
					}
				}
				if( strToTest.equals(word) ){
					if(dbgChk) {
					System.out.println("Words are just eq: "+i);
					}
					returnvalue.add( new Integer( i ) );
				}
			}
		}
		return returnvalue;
	}
}

