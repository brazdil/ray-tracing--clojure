(ns ray-tracing.object
	(:require [ray-tracing.geometry :as geometry]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defprotocol PObject
	"Interface that each object in the scene has to implement"
	(intersect [ this ray ] 
		"Returns list of points of intersection of the object with given ray.
		 For convenience these are returned as a single scalar, which is
		 the parameter \"t\" in the line equation: X(t) = P + t * D ")
	(color-at [ this ray ]
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
		(color-at [ this ray ]
			(let [ intersection 	(geometry/ray-point 	ray
															(first-intersect this ray))			 ]
				color)))

(defn create-sphere
	[ origin radius color ]
	(Sphere. origin radius color))

(deftype Parallelogram
	[ origin v1 v2 color N d len-v1-sq len-v2-sq ]
	PObject
		(intersect [ this ray ]
			(let [ t        (/	(-  d
				       				(geometry/vec-dot-product N (:point ray)))
				       			(geometry/vec-dot-product N (:direction ray)))
				   P 		(geometry/ray-point ray t)
				   OP		(geometry/vec-subtract
				   				P
				   				origin)
				   e1       (geometry/vec-dot-product v1 OP)
				   e2       (geometry/vec-dot-product v2 OP) ]
				(if (and 	(>= e1 0) (<= e1 len-v1-sq)
							(>= e2 0) (<= e2 len-v2-sq))
					[ t ]
					[ ])))
		(color-at [ this ray ]
			(let [ intersection 	(geometry/ray-point 	ray
															(first-intersect this ray))			 ]
				color)))

(defn create-parallelogram
	[ origin v1 v2 color ]
	(let [ 	len-v1 		(geometry/vec-length v1)
	       	len-v2 		(geometry/vec-length v2)
	       	N        	(geometry/vec-normalize
							(geometry/vec-vector-product v1 v2))
			d 			(geometry/vec-dot-product N origin) ]
	(Parallelogram. 	origin 
						v1 
						v2 
						color
						N
						d
						(* len-v1 len-v1)
						(* len-v2 len-v2) )))

(deftype Box
	[ rect1 rect2 rect3 rect4 rect5 rect6 color ])

(defn create-box 
	[ origin v1 v2 v3 color ]
	[])