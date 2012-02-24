(ns ray-tracing.object-common
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material]))

(defprotocol PObject
	"Interface that each object in the scene has to implement"
	(debug [ this ])
	(translate [ this v ]
		"Translates the object by vector v")
	(rotateX [ this angle ])
	(rotateY [ this angle ])
	(rotateZ [ this angle ])
	(scale [ this amount ])
	(intersect [ this ray ] 
		"Returns list of points of intersection of the object with given ray.
		 For convenience these are returned as a single scalar, which is
		 the parameter \"t\" in the line equation: X(t) = P + t * D ")
	(colour-at [ this objects lights ray ]
		"Takes the first intersection with the object and computes
		 its color in that point"))

(def epsilon 0.0001)
(def minus-epsilon (- epsilon))

(defn first-intersection
	[ object ray ]
	(reduce 	#(if (<= %2 epsilon)
					%1
					(if (nil? %1)
						%2
						(min %1 %2)))
				nil
				(.intersect object ray)))

(defn- first-intersecting-struct
	"Returns a structure with the first object intesected by the ray,
	 or nil if there wasn't any intersection."
	[ objects ray ]
	(reduce 	#(if (nil? (:first-intersect %2))
					%1
					(if (nil? %1)
						%2
						(if (< (:first-intersect %1) (:first-intersect %2))
							%1
							%2)))
				nil
				(map 	#(if true 
							{:object %, 
							 :first-intersect 
							 	(first-intersection % ray)}) 
						objects)))

(defn first-intersecting-object
	"Returns the object that is first hit by the ray, or nil if there isn't any."
	[ objects ray ]
	(let [ first-struct		(first-intersecting-struct objects ray)	]
		(if (or 	(nil? first-struct)
					(< (:first-intersect first-struct) epsilon))
			nil
			(:object first-struct))))

(defn first-intersecting-distance
	"Returns the distance to first object hit by the ray (in units), 
	 or +INFINITY if there isn't any."
	[ objects ray ]
	(let [ first-struct		(first-intersecting-struct objects ray)	]
		(if (or 	(nil? first-struct)
					(< (:first-intersect first-struct) epsilon))
			java.lang.Double/POSITIVE_INFINITY
			(:first-intersect first-struct))))

(defn is-intersected
	"Returns whether there is an intersection with any object between two points"
	[ objects v1 v2 ]
	(let [ direction	(geometry/vec-subtract v2 v1) ]
		(< 	(first-intersecting-distance
					objects
					(geometry/ray-create 	v1 
											direction))
			(geometry/vec-length direction))))
