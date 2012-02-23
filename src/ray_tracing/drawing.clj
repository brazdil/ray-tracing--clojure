(ns ray-tracing.drawing
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material])
	(:require [ray-tracing.object :as object]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
; (defmacro dbg [x] `(let [x# ~x] (println "dbg:" x#) x#))

(defn- cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
	step
	(fn step [v-seqs]
	  (let [increment
		(fn [v-seqs]
		  (loop [ i (dec (count v-seqs)) 
		          v-seqs v-seqs]
		    (if (= i -1) nil
			(if-let [rst (next (v-seqs i))]
			  (assoc v-seqs i rst)
			  (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
	    (when v-seqs
	       (cons (map first v-seqs)
		     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? first seqs)
      (lazy-seq (step v-original-seqs)))))

(defn pixels-seq
  "Returns coordiantes of all pixels in the screen"
  [width height]
  (cartesian-product (range 0 width) (range 0 height)))

(defrecord Camera [position look-at up])

(defn camera-create
	[ position look-at up ]
	(Camera. position look-at up))

(defrecord Projection [ viewing-angle screen-distance width height background-colour ])

(defn projection-create
	[ viewing-angle screen-distance width height background-colour ]
	(Projection. viewing-angle screen-distance width height background-colour))

(defrecord Pixel [ coords colour ])

(defn pixel-create
	[ coords colour ]
	(Pixel. coords colour))

(defrecord Rectangle [ top-left top-right bottom-left bottom-right ] )

(defn rectangle-create
	[ top-left top-right bottom-left bottom-right ]
	(Rectangle. top-left top-right bottom-left bottom-right))

(defn screen-rect
	"Returns the coordinates of screen's rectangle's vertices. 
	 These are in 3D, relative to the position of the camera."
	[ camera projection ]
	(let [	angle-hori-half			(/ (:viewing-angle projection) 2)

			vec-direction			(geometry/vec-normalize
										(geometry/vec-subtract (:look-at camera) (:position camera)))			

			vec-center				(geometry/vec-mult 
										vec-direction
			 							(:screen-distance projection))
			vec-edge-left			(geometry/vec-mult
										(geometry/vec-rotate vec-direction (:up camera) (- angle-hori-half))
										(/ (:screen-distance projection) (java.lang.Math/cos angle-hori-half)))
			
			vec-sideways			(geometry/vec-subtract
										vec-edge-left
										vec-center)
			vec-twice-sideways		(geometry/vec-mult
											vec-sideways
											-2)
			vec-upwards				(geometry/vec-mult
										(geometry/vec-rotate
											vec-sideways
											vec-direction
											(/ java.lang.Math/PI -2))
										(/ (:height projection) (:width projection)))

			vec-top-left			(geometry/vec-add 
										vec-edge-left 
										vec-upwards)
			vec-top-right			(geometry/vec-add
										vec-top-left
										vec-twice-sideways)
			vec-bottom-left			(geometry/vec-subtract
										vec-edge-left
										vec-upwards)											
			vec-bottom-right		(geometry/vec-add
										vec-bottom-left
										vec-twice-sideways) ]
		(rectangle-create 
			vec-top-left
			vec-top-right
			vec-bottom-left
			vec-bottom-right)))

(defn draw-single-coord
	[ objects lights camera projection screen-rect pixel counter ]
	(let [ 	x					(+ (first pixel) 0.5)
			y					(+ (first (rest pixel)) 0.5)
			total  				(* (:width projection) (:height projection))
			screen-coord		(geometry/vec-add
									(geometry/vec-add
										(geometry/vec-mult
											(geometry/vec-subtract
												(:top-right screen-rect)
												(:top-left screen-rect))
											(/ x (:width projection)))
										(geometry/vec-mult
											(geometry/vec-subtract
												(:bottom-left screen-rect)
												(:top-left screen-rect))
											(/ y (:height projection))))
									(:top-left screen-rect))
			ray 				(geometry/ray-create
									(:position camera)
									screen-coord)
			first-object		(object/first-intersecting-object objects ray)	]
		; increase the counter and print if increased by 1 percent
		(send-off counter #(do	(if (not= 	(quot (* 100 %) total)
											(quot (* 100 (inc %)) total))
									(println (str "computing: " (quot (* 100 (inc %)) total) "%")))
								(inc %)))
		(if (nil? first-object)
			(pixel-create pixel (:background-colour projection))
			(pixel-create pixel (.colour-at first-object objects lights ray)))))

(defn draw-simple
	"Draws the scene"
	[ objects lights camera projection ]
	(let [ rect (screen-rect camera projection)
		   counter (agent 0) ]
		(pmap 
			#(draw-single-coord
				objects 
				lights
				camera
				projection
				rect
				%
				counter) 
			(pixels-seq
				(:width projection) 
				(:height projection)))))

(defn- save-as-png-pixel
	[ image pixel counter total ]
	(do
		(send-off counter #(do	(if (not= 	(quot (* 100 %) total)
											(quot (* 100 (inc %)) total))
									(println (str "drawing: " (quot (* 100 (inc %)) total) "%")))
								(inc %)))
		(.setRGB image		(first (:coords pixel))
							(first (rest (:coords pixel)))
							(.getRGB (material/colour-to-java  (:colour pixel))))))
(defn save-as-png
	"Saves pixels as a PNG"
	[ filename projection pixels ]
	(let [ 	total  	(* (:width projection) (:height projection))
			counter (agent 0)
			image 	(new java.awt.image.BufferedImage
						(:width projection)
						(:height projection)
						java.awt.image.BufferedImage/TYPE_INT_RGB)
			file 	(new java.io.File filename)					]
		(dorun (map #(save-as-png-pixel image % counter total) pixels))
		(javax.imageio.ImageIO/write image "png" file)
		nil ))

(defn- show-realtime-pixel
	[ image pixel counter total window ]
	(do
		(send-off counter #(do	(if (not= 	(quot (* 100 %) total)
											(quot (* 100 (inc %)) total))
									(do
										(println (str "drawing: " (quot (* 100 (inc %)) total) "%"))
										(.repaintImage window)))
								(inc %)))
		(.setRGB image 		(first (:coords pixel))
							(first (rest (:coords pixel)))
							(.getRGB (material/colour-to-java (:colour pixel))))))

(defn show-realtime
	[ projection pixels ]
	(let [ 	total  	(* (:width projection) (:height projection))
			counter (agent 0)
			image 	(new java.awt.image.BufferedImage
						(:width projection)
						(:height projection)
						java.awt.image.BufferedImage/TYPE_INT_RGB)
			window 	(new ray_tracing.DrawWindow image) ]
		(.setVisible window true)
		(let [ graphics   (.getGraphics image) ]
			(.setColor graphics java.awt.Color/WHITE)
			(.fillRect graphics 0 0 (:width projection) (:height projection))
			(.dispose graphics))
		(dorun (map #(show-realtime-pixel image % counter total window) pixels))
		nil ))
