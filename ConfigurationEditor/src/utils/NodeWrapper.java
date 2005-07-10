/*
 * Created on Jan 9, 2005
 */
package utils;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * A class which wraps a DOM node and replaces the toString
 * function with one which returns the value of the name attribute. This
 * class uses a proxy method so that all function calls other than
 * toString automatically are forwarded to the underlying node.
 * @author Josh Lurz 
 * 
 */
public class NodeWrapper implements InvocationHandler {
    
    /**
     * Create an instance of this wrapper class with a node to wrap.
     * @param aNode The node to wrap in this proxy class.
     * @return An instance of the invocation handler.
     */
    public static Object createProxy(Node aNode) {
        return Proxy.newProxyInstance(aNode.getClass().getClassLoader(), aNode
                .getClass().getInterfaces(), new NodeWrapper(aNode));
    }

    /**
     * The internal node which this class wraps.
     */
    private Node mInternalNode;

    /**
     * Private constructor which sets the internal node to the one passed in.
     * 
     * @param aNodeToWrap The node to forward all function calls to except
     *  toString.
     */
    private NodeWrapper(Node aNodeToWrap) {
        mInternalNode = aNodeToWrap;
    }

    /**
     * Method called when a function is invoked on the object. Checks if the
     * function is the toString function and replaces it with a function that
     * returns the value of the name attribute. Otherwise the call is forwarded
     * to the internal node.
     */
    public Object invoke(Object proxy, Method method, Object[] args)
            throws Throwable {
        // Special case the toString method to return the value of the name
        // child.
        if (method.getName().equals("toString")) { //$NON-NLS-1$
            // Get the name attr.
            NamedNodeMap attrs = mInternalNode.getAttributes();
            if (attrs == null) {
                return ""; // we've got trouble. //$NON-NLS-1$
            }
            Node nameAttr = attrs.getNamedItem("name"); //$NON-NLS-1$
            if (nameAttr == null) {
                return ""; // also trouble; //$NON-NLS-1$
            }
            return nameAttr.getNodeValue();
        }
        // Dispatch all methods other than toString to the internal node object.
        Object result = null;
        try {
            result = method.invoke(mInternalNode, args);
        } catch (InvocationTargetException e) {
            throw e.getCause();
        }
        return result;
    }
}
