(ns ray-tracing.object
	(:require [ray-tracing.geometry :as geometry]))

(defprotocol PObject
	"Interface that each object in the scene has to implement"
	(intersect [ this ray ] 
		"Returns list of points of intersection of the object with given ray.
		 For convenience these are returned as a single scalar, which is
		 the parameter \"t\" in the line equation: X(t) = P + t * D ")
	(pixel-color [ this ray ]
		"Takes the first intersection with the object and computes
		 its color in that point"))

(defn first-intersect
	[ object ray ]
	(reduce 	#(if (<= %2 0.0001)
					%1
					(if (nil? %1)
						%2
						(min %1 %2)))
				nil
				(.intersect object ray)))

(deftype Sphere
	[ center radius color ]
	PObject
		(intersect [ this ray ]
			(let [ O_C  (geometry/vec-subtract (:point ray) center)
				   a    (geometry/vec-dot-product 
							(:direction ray)
							(:direction ray))
			       b    (geometry/vec-dot-product
			       			(geometry/vec-mult (:direction ray) 2)
			       			O_C)
			       c    (- (geometry/vec-dot-product O_C O_C)
			               (* radius radius))
			       d    (- (* b b) (* 4 a c))			 ]
				(if (< d 0)
					; no intersections
					[]
					(if (= d 0)
						; one intersection
						[ (/ (- b) (* 2 a)) ]
						; two intersections
						[ (/ (+ (- b) (java.lang.Math/sqrt d)) (* 2 a))
						  (/ (- (- b) (java.lang.Math/sqrt d)) (* 2 a)) ]))))
		(pixel-color [ this ray ]
			(let [ intersection 	(geometry/ray-point 	ray
															(first-intersect this ray))			 ]
				color)))

(deftype Parallelogram
	[ origin v1 v2 color ]
	PObject
		(intersect [ this ray ]
			(let [ N 		(geometry/vec-normalize
								(geometry/vec-vector-product v1 v2))
			       d 		(geometry/vec-dot-product N origin)
			       t        (/	(- 	d 
				       				(geometry/vec-dot-product N (:point ray)))
				       			(geometry/vec-dot-product N (:direction ray)))
				   P 		(geometry/ray-point ray t)
				   OP		(geometry/vec-subtract
				   				P
				   				origin)
				   e1       (geometry/vec-dot-product v1 OP)
				   e2       (geometry/vec-dot-product v2 OP)			]
				(if (and 	(>= e1 0) (<= e1 1)
							(>= e2 0) (<= e2 1))
					[ t ]
					[ ])))
		(pixel-color [ this ray ]
			(let [ intersection 	(geometry/ray-point 	ray
															(first-intersect this ray))			 ]
				color)))

