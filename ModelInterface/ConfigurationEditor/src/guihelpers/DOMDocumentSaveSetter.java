package guihelpers;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.w3c.dom.Document;

/**
 * This property change listener watches for a document modified event and add
 * the needs-save attribute to the document.
 * 
 * @author Josh Lurz
 */
final public class DOMDocumentSaveSetter implements PropertyChangeListener {
    /**
     * The source document.
     */
    final transient Document mDocument;

    /**
     * Constructor
     * 
     * @param aDocument
     *            The document to which this document is attached.
     */
    public DOMDocumentSaveSetter(final Document aDocument) {
        super();
        mDocument = aDocument;
    }

    /**
     * Method called when the document-modified property change is fired. This
     * property change listener should be hooked to that property change only.
     * 
     * @param aEvent
     *            The mutation event received.
     */
    public void propertyChange(final PropertyChangeEvent aEvent) {
        if (aEvent.getPropertyName().equals("document-modified")) {
            // Avoid recursively setting events.
            if (!mDocument.getDocumentElement().getAttribute("needs-save")
                    .equals("true")) {
                mDocument.getDocumentElement().setAttribute(
                        "needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
            }
        } else {
            Logger.global
                    .log(Level.WARNING,
                            "Document save setter listeners was hooked up to the wrong property change.");

        }
    }
}