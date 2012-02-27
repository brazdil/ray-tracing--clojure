(ns raytracing.output
	(:require [raytracing.material :as material])
	(:require [raytracing.drawing :as drawing])
	(:require [raytracing.object :as object])
	(:require [raytracing.object-common :as object-common]))

(defn- png-pixel
	[ image pixel total ]
	(.setRGB image		(first (:coords pixel))
						(first (rest (:coords pixel)))
						(.getRGB (material/colour-to-java  (:colour pixel)))))
(defn png
	"Saves pixels as a PNG"
	[ filename projection pixels ]
	(let [ 	total  	(* (:width projection) (:height projection))
			image 	(new java.awt.image.BufferedImage
						(:width projection)
						(:height projection)
						java.awt.image.BufferedImage/TYPE_INT_RGB)
			file 	(new java.io.File filename)					]
		(dorun (map #(png-pixel image % total) pixels))
		(javax.imageio.ImageIO/write image "png" file)
		nil ))

(defn- realtime-pixel
	[ image pixel counter total window ]
	(do
		(send-off counter #(do	(if (not= 	(quot (* 1000 %) total)
											(quot (* 1000 (inc %)) total))
									(.repaintImage window))
								(inc %)))
		(.setRGB image 		(first (:coords pixel))
							(first (rest (:coords pixel)))
							(.getRGB (material/colour-to-java (:colour pixel))))))

(defn realtime
	[ projection pixels ]
	(let [ 	total  	(* (:width projection) (:height projection))
			counter (agent 0)
			image 	(new java.awt.image.BufferedImage
						(:width projection)
						(:height projection)
						java.awt.image.BufferedImage/TYPE_INT_RGB)
			window 	(new raytracing.DrawWindow image) ]
		(.setVisible window true)
		(let [ graphics   (.getGraphics image) ]
			(.setColor graphics java.awt.Color/WHITE)
			(.fillRect graphics 0 0 (:width projection) (:height projection))
			(.dispose graphics))
		(dorun (map #(realtime-pixel image % counter total window) pixels))
		nil ))
