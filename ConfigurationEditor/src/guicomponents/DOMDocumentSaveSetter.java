package guicomponents;

import org.w3c.dom.Document;
import org.w3c.dom.events.Event;
import org.w3c.dom.events.EventListener;

/**
 * This mutation listener watches for events which change the underlying
 * document. When a mutation event is received the dirty attribute is set on
 * the document and the save menu and button are activated. TODO: Use this
 * elsewhere.
 * 
 * @author Josh Lurz
 */
final public class DOMDocumentSaveSetter implements
		EventListener {
	/**
	 * The source document.
	 */
	final transient Document mDocument;
	
	/**
	 * Constructor
	 * @param aDocument The document to which this document is attached.
	 */
	public DOMDocumentSaveSetter(final Document aDocument) {
		super();
		mDocument = aDocument;
	}

	/**
	 * Method called when mutation events from the configuration document
	 * are received.
	 * 
	 * @param aEvent
	 *            The mutation event received.
	 */
	public void handleEvent(final Event aEvent) {
		// This doesn't recursively send another event,
		// not sure why but it works.
		mDocument.getDocumentElement().setAttribute(
				"needs-save", "true"); //$NON-NLS-1$ //$NON-NLS-2$
	}
}