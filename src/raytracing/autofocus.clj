(ns raytracing.autofocus
	(:require [ raytracing.geometry :as geometry ])
	(:require [ raytracing.math :as math ])
	(:require [ raytracing.drawing :as drawing ]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn- focus-on--screen-point
	[ camera screen-rect point ]
	(let [ camera-to-point		(geometry/pvec-subtract point (:position camera)) ]
		(geometry/pvec-add 
			(:position camera)
			(geometry/pvec-mult
				camera-to-point
				(.divide	(geometry/pvec-dot-product
								(:direction camera)
								(:top-left screen-rect))
							(geometry/pvec-dot-product
								(:direction camera)
								camera-to-point))))))

(defn- focus-on--camera-point
	[ camera screen-rect-pos-x screen-rect-pos-y original-point screen-point ]
	(let [ decentered-points 		[ 	(geometry/pvec-add screen-point screen-rect-pos-x)
										(geometry/pvec-add screen-point screen-rect-pos-y)
										(geometry/pvec-subtract screen-point screen-rect-pos-x)
										(geometry/pvec-subtract screen-point screen-rect-pos-y)	] ]
		(map #(geometry/pvec-subtract
					(:position camera)
					(geometry/pvec-add
						original-point
						(let [ decenter-to-orig (geometry/pvec-subtract % original-point) ]
							(geometry/pvec-mult
								decenter-to-orig
								(.divide 	(geometry/pvec-dot-product 
												(:direction camera)
												(geometry/pvec-subtract
													(:position camera)
													original-point))
											(geometry/pvec-dot-product 
												(:direction camera)
												decenter-to-orig))))))
			decentered-points)))

(defn focus-on
	"Returns screen distance and DOF diameter that will keep given object in focus"
	( [ projection object epsilon ] 
		(let 	[ 	p-epsilon			(geometry/pdouble epsilon)
					bbox 				(.bounding-box object)
					center 				(geometry/vec-create
											(* (+ (:xmin bbox) (:xmax bbox)) 0.5)
											(* (+ (:ymin bbox) (:ymax bbox)) 0.5)
											(* (+ (:zmin bbox) (:zmax bbox)) 0.5))
					screen-distance 	(geometry/vec-dot-product
											(geometry/vec-subtract
												center
												(:position (:camera projection)))
											(:direction (:camera projection)))

					screen-rect 		(drawing/screen-rect
											(:camera projection)
											screen-distance
											(:width projection)
											(:height projection)
											(:viewing-angle projection)) 

					p-screen-rect 		{	:top-left 	(geometry/pvec-create (:top-left screen-rect))
											:sideways 	(geometry/pvec-create (:sideways screen-rect))
											:downwards 	(geometry/pvec-create (:downwards screen-rect)) 	}
					p-camera	 		{   :position 	(geometry/pvec-create (:position (:camera projection)))
											:direction 	(geometry/pvec-create (:direction (:camera projection)))			}

					screen-rect-pos-x	(geometry/pvec-mult
											(:sideways p-screen-rect)
											(.divide p-epsilon (geometry/pdouble (:width projection))))
					screen-rect-pos-y	(geometry/pvec-mult
											(:downwards p-screen-rect)
											(.divide p-epsilon (geometry/pdouble (:height projection))))

					bbox-vertices 		[ (geometry/pvec-create (:xmin bbox) (:ymin bbox) (:zmin bbox))
					                      (geometry/pvec-create (:xmax bbox) (:ymin bbox) (:zmin bbox))
					                      (geometry/pvec-create (:xmin bbox) (:ymax bbox) (:zmin bbox))
					                      (geometry/pvec-create (:xmax bbox) (:ymax bbox) (:zmin bbox))
					                      (geometry/pvec-create (:xmin bbox) (:ymin bbox) (:zmax bbox))
					                      (geometry/pvec-create (:xmax bbox) (:ymin bbox) (:zmax bbox))
					                      (geometry/pvec-create (:xmin bbox) (:ymax bbox) (:zmax bbox))
					                      (geometry/pvec-create (:xmax bbox) (:ymax bbox) (:zmax bbox)) ]
					screen-points		(map 	#(focus-on--screen-point p-camera p-screen-rect %) 
												bbox-vertices)
					camera-points 		(map 	#(focus-on--camera-point
													p-camera
													screen-rect-pos-x
													screen-rect-pos-y
													%1
													%2)
												bbox-vertices
												screen-points)
					decentered-dists	(map geometry/pvec-length (reduce concat camera-points))
				]		
			; (dbg (geometry/pvec-normalize (geometry/pvec-subtract (first bbox-vertices) (first screen-points) ) ))
			; (dbg (geometry/pvec-normalize (geometry/pvec-subtract (first bbox-vertices) (:position p-camera) ) ))
			{ :screen-distance 	screen-distance
 			  :diameter 		(.doubleValue 
 			  						(reduce 	#(if (.lt %1 %2) %1 %2) 
 			  									(geometry/pdouble math/INFINITY) 
 			  									decentered-dists)) 					}))
	( [ projection object ]
		(focus-on projection object 0.5)))
