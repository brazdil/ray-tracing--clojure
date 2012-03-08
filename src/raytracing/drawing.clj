(ns raytracing.drawing
	(:require [raytracing.math :as math])
	(:require [raytracing.geometry :as geometry])
	(:require [raytracing.material :as material])
	(:require [raytracing.object :as object])
	(:require [raytracing.object-common :as object-common]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))
; (defmacro dbg [x] `(let [x# ~x] (println "dbg:" x#) x#))

(defn pixels-seq
	"Returns coordiantes of all pixels in the screen"
	[width height]
	(math/cartesian-product (range 0 width) (range 0 height)))

(defn subpixels-seq
	[ sampling ]
	(math/cartesian-product (range 0 sampling) (range 0 sampling)))

(defn dof-decenter-seq
	[ sampling ]
	(let [ center 	(/ sampling 2)
	       radius 	(- center 0.5) ]
		(filter #(let [ px (- center (+ (first %) 0.5))
			            py (- center (+ (first (rest %)) 0.5)) ]
			    	(<= (+ (* px px) (* py py)) (* radius radius)))
				(subpixels-seq sampling))))

(defrecord Camera [ position look-at up sideways direction ])

(defn camera-create
	[ position look-at ]
	(let [  d 			(geometry/vec-subtract look-at position)
			angle-alpha (if (math/is-zero (:z d))
							(if (math/is-zero (:x d))
								(throw (new IllegalArgumentException "Can't deal with camera looking straight up/down"))
								(if (> (:x d) 0)
									math/PIover2
									(- math/PIover2)))
							(java.lang.Math/atan (/ (:x d) (:z d))))
			d-yz 		(geometry/vec-rotate-y d (- angle-alpha))
			angle-beta 	(if (math/is-zero (:z d-yz))
							(throw (new IllegalArgumentException "Can't deal with camera looking straight up/down"))
							(- (java.lang.Math/atan (/ (:y d-yz) (:z d-yz)))))
			up 			(geometry/vec-rotate-y
							(geometry/vec-rotate-x
								geometry/vec-y-pos
								angle-beta)
							angle-alpha)			]
	(Camera. 	position 
				look-at 
				(geometry/vec-normalize up)
				(geometry/vec-normalize
					(geometry/vec-vector-product
						d
						up))
				(geometry/vec-normalize d))))

(defn camera-move
	[ camera delta ]
	(Camera. 	(geometry/vec-add delta (:position camera))
				(geometry/vec-add delta (:look-at camera))
				(:up camera)
				(:sideways camera)
				(:direction camera)))

(defrecord ScreenRectangle [ top-left downwards sideways ] )

(defn screen-rect-create
	[ top-left downwards sideways ]
	(ScreenRectangle. 	top-left 
						downwards
						sideways))

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
										vec-upwards) 	]
			; vec-bottom-right		(geometry/vec-add
			; 							vec-bottom-left
			; 							vec-twice-sideways) ]
		(screen-rect-create 
			vec-top-left
			(geometry/vec-subtract vec-bottom-left vec-top-left)
			(geometry/vec-subtract vec-top-right vec-top-left))))

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

(defn projection-move-camera
	[ projection delta ]
	(Projection. 	(:viewing-angle projection)
					(:screen-distance projection)
					(:width projection)
					(:height projection)
					(camera-move (:camera projection) delta)
					(:screen-rect projection)
					(:background-colour projection)))

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

			screen-coord		(geometry/vec-add
									(geometry/vec-add
										(geometry/vec-mult
											(:sideways screen-rect)
											(/ x (:width projection)))
										(geometry/vec-mult
											(:downwards screen-rect)
											(/ y (:height projection))))
									(:top-left screen-rect))
			ray 				(geometry/ray-create
									(:position camera)
									screen-coord)
			closest-node		(.closest-node root-object ray) ]
		(if (nil? closest-node)
			(pixel-create coords (:background-colour projection))
			(pixel-create coords (.colour-at (:object closest-node) root-object lights ray)))))

(defn- subpixel-randomization 
	"Given a subpixel, it randomizes its coordinate"
	[ sampling coords subpixel ] 
	[ (+ (/ (+ (first subpixel) (rand)) sampling) -0.5 (first coords))
	  (+ (/ (+ (first (rest subpixel)) (rand)) sampling) -0.5 (first (rest coords))) ])

(defn- is-coord-in 
	[ x coords ]
	(let [ eq 	(fn [a b] (and (= (first a) (first b)) (= (first (rest a)) (first (rest b))))) ]
		(reduce #(or %1 (eq x %2))
				false
				coords)))

(defn get-pixel-antialiased
	[ map-fn sampling root-object lights projection coords ]
	(let [ samplingM1				(- sampling 1)
		   corner-subpixels			[ [0, 0] 
			                          [0, samplingM1] 
			                          [samplingM1, 0] 
			                          [samplingM1 samplingM1] ]
		   colours-corners 			(map-fn #(:colour (get-pixel-classic root-object lights projection %))
		   									(map #(subpixel-randomization sampling coords %) corner-subpixels ))
		   colours-corners-average	(material/colour-average colours-corners)	]
		(if (material/colours-same colours-corners colours-corners-average)
			(pixel-create coords colours-corners-average)
			(pixel-create
				coords
				(material/colour-average
					(concat 
						colours-corners
						(map-fn	#(:colour (get-pixel-classic root-object lights projection %))
								(map 	#(subpixel-randomization sampling coords %) 
										(filter #(not (is-coord-in % corner-subpixels))
												(subpixels-seq sampling))))))))))

(defn get-pixel-dof
	[ map-fn get-pixel-fn samples root-object lights projection coords ]
	(let [ 	camera-up 			(:up (:camera projection))
			camera-sideways		(:sideways (:camera projection)) ]
		(pixel-create
			coords
			(material/colour-average
				(map-fn 	(fn [ delta ]
								(:colour (get-pixel-fn 	
												root-object
												lights
												(projection-move-camera 
													projection
													(geometry/vec-add
														(geometry/vec-mult camera-sideways (first delta))
														(geometry/vec-mult camera-up (first (rest delta)))))
												coords)))
							samples)))))

(defn get-fn-classic
	[ ]
	get-pixel-classic)

(defn get-fn-antialiased
	[ sampling ]
	(partial get-pixel-antialiased map sampling))

(defn get-fn-antialiased-parallel
	[ sampling ]
	(partial get-pixel-antialiased pmap sampling))
	
(defn get-fn-dof
	[ sampling diameter subfn ]
	(partial get-pixel-dof 	map 
							subfn
							(map #(let [ subpixel (subpixel-randomization sampling [ 0 0 ] %) ]
									[ (* (first subpixel) diameter)
									  (* (first (rest subpixel)) diameter) ])
								(dof-decenter-seq sampling))))

(defn- focus-on--screen-point
	[ projection screen-rect point ]
	(let [ camera-to-point		(geometry/vec-subtract point (:position (:camera projection))) ]
		(geometry/vec-add 
			(:position (:camera projection))
			(geometry/vec-mult
				camera-to-point
				(/	(geometry/vec-dot-product
						(:direction (:camera projection))
						(:top-left screen-rect))
					(geometry/vec-dot-product
						(:direction (:camera projection))
						camera-to-point))))))

(defn- focus-on--camera-point
	[ projection screen-rect-pos-x screen-rect-pos-y original-point screen-point ]
	(let [ decentered-points 		[ 	(geometry/vec-add screen-point screen-rect-pos-x)
										(geometry/vec-add screen-point screen-rect-pos-y)
										(geometry/vec-subtract screen-point screen-rect-pos-x)
										(geometry/vec-subtract screen-point screen-rect-pos-y)	] ]
		(map #(geometry/vec-subtract
					(:position (:camera projection))
					(geometry/vec-add
						screen-point
						(let [ decenter-to-orig (geometry/vec-subtract % original-point) ]
							(geometry/vec-mult
								decenter-to-orig
								(/ 	(geometry/vec-dot-product 
										(:direction (:camera projection))
										(geometry/vec-subtract
											(:position (:camera projection))
											original-point))
									(geometry/vec-dot-product 
										(:direction (:camera projection))
										decenter-to-orig))))))
			decentered-points)))

(defn focus-on
	"Returns screen distance and DOF diameter that will keep given object in focus"
	( [ projection object epsilon ] 
		(let 	[ 	bbox 				(.bounding-box object)
					center 				(geometry/vec-create
											(/ (+ (:xmin bbox) (:xmax bbox)) 2)
											(/ (+ (:ymin bbox) (:ymax bbox)) 2)
											(/ (+ (:zmin bbox) (:zmax bbox)) 2))
					screen-distance 	(geometry/vec-dot-product
											(geometry/vec-subtract
												center
												(:position (:camera projection)))
											(:direction (:camera projection)))

					screen-rect 		(screen-rect
											(:camera projection)
											screen-distance
											(:width projection)
											(:height projection)
											(:viewing-angle projection)) 
					screen-rect-pos-x	(geometry/vec-mult
											(:sideways screen-rect)
											(/ epsilon (:width projection)))
					screen-rect-pos-y	(geometry/vec-mult
											(:downwards screen-rect)
											(/ epsilon (:height projection)))

					bbox-vertices 		[ (geometry/vec-create (:xmin bbox) (:ymin bbox) (:zmin bbox))
					                      (geometry/vec-create (:xmax bbox) (:ymin bbox) (:zmin bbox))
					                      (geometry/vec-create (:xmin bbox) (:ymax bbox) (:zmin bbox))
					                      (geometry/vec-create (:xmax bbox) (:ymax bbox) (:zmin bbox))
					                      (geometry/vec-create (:xmin bbox) (:ymin bbox) (:zmax bbox))
					                      (geometry/vec-create (:xmax bbox) (:ymin bbox) (:zmax bbox))
					                      (geometry/vec-create (:xmin bbox) (:ymax bbox) (:zmax bbox))
					                      (geometry/vec-create (:xmax bbox) (:ymax bbox) (:zmax bbox)) ]
					screen-points		(map 	#(focus-on--screen-point projection screen-rect %) 
												bbox-vertices)
					camera-points 		(map 	#(focus-on--camera-point
													projection
													(dbg screen-rect-pos-x)
													(dbg screen-rect-pos-y)
													%1
													%2)
												bbox-vertices
												screen-points)
					decentered-dists	(map geometry/vec-length (dbg (reduce concat camera-points)))
				]		
			{ :screen-distance 	screen-distance
 			  :diameter 		(reduce min math/INFINITY decentered-dists) }))
	( [ projection object ]
		(focus-on projection object 0.5)))

(defn generate-pixels
	"Draws the scene"
	[ root-object lights projection func ]
	(pmap 
		#(func
			root-object
			lights
			projection
			% ) 
		(reverse (pixels-seq
			(:width projection) 
			(:height projection)))))
