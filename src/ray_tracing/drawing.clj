(ns ray-tracing.drawing
	(:require [ray-tracing.math :as math])
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material])
	(:require [ray-tracing.object :as object])
	(:require [ray-tracing.object-common :as object-common]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
; (defmacro dbg [x] `(let [x# ~x] (println "dbg:" x#) x#))

(defn pixels-seq
	"Returns coordiantes of all pixels in the screen"
	[width height]
	(math/cartesian-product (range 0 width) (range 0 height)))

(defn subpixels-seq
	[ sampling ]
	(math/cartesian-product (range 0 sampling) (range 0 sampling)))

(defrecord Camera [position look-at up])

(defn camera-create
	[ position look-at up ]
	(Camera. position look-at up))

(defrecord ScreenRectangle [ top-left top-right bottom-left bottom-right ] )

(defn screen-rect-create
	[ top-left top-right bottom-left bottom-right ]
	(ScreenRectangle. top-left top-right bottom-left bottom-right))

(defn screen-rect
	"Returns the coordinates of screen's rectangle's vertices. 
	 These are in 3D, relative to the position of the camera."
	[ camera screen-distance width height viewing-angle ]
	(let [	angle-hori-half			(/ viewing-angle 2)

			vec-direction			(geometry/vec-normalize
										(geometry/vec-subtract (:look-at camera) (:position camera)))			

			vec-center				(geometry/vec-mult 
										vec-direction
			 							screen-distance)
			vec-edge-left			(geometry/vec-mult
										(geometry/vec-rotate vec-direction (:up camera) (- angle-hori-half))
										(/ screen-distance (java.lang.Math/cos angle-hori-half)))
			
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
											(- math/PIover2))
										(/ height width))

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
		(screen-rect-create 
			vec-top-left
			vec-top-right
			vec-bottom-left
			vec-bottom-right)))

(defrecord Projection [ viewing-angle screen-distance width height camera screen-rect background-colour ])

(defn projection-create
	[ viewing-angle screen-distance width height camera background-colour ]
	(Projection. 	viewing-angle 
					screen-distance 
					width 
					height 
					camera 
					(screen-rect
						camera
						screen-distance
						width
						height 
						viewing-angle) 
					background-colour))

(defrecord Pixel [ coords colour ])

(defn pixel-create
	[ coords colour ]
	(Pixel. coords colour))

(defn get-pixel-classic
	[ root-object lights projection coords ]
	(let [ 	camera 				(:camera projection)
			screen-rect 		(:screen-rect projection)

			x					(+ (first coords) 0.5)
			y					(+ (first (rest coords)) 0.5)

			; total  				(* (:width projection) (:height projection))
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
			intersections		(.intersect root-object ray) ]
		; (println coords)
		; increase the counter and print if increased by 1 percent
		; (send-off counter #(do	(if (not= 	(quot (* 100 %) total)
		; 									(quot (* 100 (inc %)) total))
		; 							(println (str "computing: " (quot (* 100 (inc %)) total) "%")))
		; 						(inc %)))
		(if (empty? intersections)
			(pixel-create coords (:background-colour projection))
			(pixel-create coords (.colour-at root-object root-object lights ray)))))

(defn- subpixel-randomization 
	"Given a subpixel, it randomizes its coordinate"
	[ sampling coords subpixel ] 
	[ (+ (/ (+ (first subpixel) (rand)) sampling) -0.5 (first coords))
	  (+ (/ (+ (first (rest subpixel)) (rand)) sampling) -0.5 (first (rest coords))) ])

(defn get-pixel-antialiased
	[ map-fn sampling root-object lights projection coords ]
	(let [ subpixels-corners 		[ [0, 0] 
		                              [0, (- sampling 1)] 
		                              [(- sampling 1), 0] 
		                              [(- sampling 1) (- sampling 1)] ] ]

		(pixel-create
			coords
			(material/colour-average
				(map-fn	#(:colour (get-pixel-classic root-object lights projection %))
						(map #(subpixel-randomization sampling coords %) (subpixels-seq sampling)))))))

(defn get-fn-classic
	[ ]
	get-pixel-classic)

(defn get-fn-antialiased
	[ sampling ]
	(partial get-pixel-antialiased map sampling))

(defn get-fn-antialiased-parallel
	[ sampling ]
	(partial get-pixel-antialiased pmap sampling))
	
(defn generate-pixels
	"Draws the scene"
	[ root-object lights projection func ]
	(pmap 
		#(func
			root-object
			lights
			projection
			% ) 
		(pixels-seq
			(:width projection) 
			(:height projection))))
