package raytracing;

import java.awt.* ;
import java.awt.event.* ;
import java.awt.image.* ;

public class DrawWindow extends Frame {
	private BufferedImage mImage;
	private DrawArea mDrawArea;

	protected static class DrawArea extends Panel { 

		public BufferedImage mImage;

		public DrawArea(BufferedImage img) {
			super();
			mImage = img;
		}

		public void paint(Graphics g) {
			g.drawImage(mImage, 0, 0, this);
		}

		public Dimension getPreferredSize() {
      		return new Dimension(mImage.getWidth(), mImage.getHeight());
		}
	}

	public void repaintImage() {
		mDrawArea.repaint();
	}

	public DrawWindow(BufferedImage img) {
		super("Ray-tracing");
		mImage = img;

		DrawArea drawing = new DrawArea(img) ;
		this.setLayout(new BorderLayout()) ;
		this.add("Center",drawing) ;
		this.pack() ;
		mDrawArea = drawing;		
	}
}