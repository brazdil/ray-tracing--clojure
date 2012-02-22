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

(def epsilon 0.0001)
(def minus-epsilon (- epsilon))

(defn first-intersect
	[ object ray ]
	(reduce 	#(if (<= %2 epsilon)
					%1
					(if (nil? %1)
						%2
						(min %1 %2)))
				nil
				(.intersect object ray)))

(defn first-intersecting-object
	[ objects ray ]
	(let [ first-struct		(reduce 	#(if (nil? (:first-intersect %2))
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
													 	(first-intersect % ray)}) 
												objects))		]
		(if (nil? first-struct)
			nil
			(:object first-struct))))

(deftype Sphere
	[ center radius material ]
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
				material)))

(defn sphere-create
	[ origin radius material ]
	(Sphere. origin radius material))

(deftype Parallelogram
	[ origin v1 v2 material N d len-v1-sq len-v2-sq ]
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
				material)))

(defn parallelogram-create
	[ origin v1 v2 material ]
	(let [ 	len-v1 		(geometry/vec-length v1)
	       	len-v2 		(geometry/vec-length v2)
	       	N        	(geometry/vec-normalize
							(geometry/vec-vector-product v1 v2))
			d 			(geometry/vec-dot-product N origin) ]
	(Parallelogram. 	origin 
						v1 
						v2 
						material
						N
						d
						(* len-v1 len-v1)
						(* len-v2 len-v2) )))

(defn rectangle-create
	[ origin v1 v2 material ]
	(let 	[ 	dot12			(geometry/vec-dot-product v1 v2)	]
		(if (and	(< dot12 epsilon)	(> dot12 minus-epsilon))
			(parallelogram-create origin v1 v2 material)
			(throw (new IllegalArgumentException "Vectors are not perpendicular")))))

(deftype Composite
	[ objects ]
	PObject
		(intersect [ this ray ]
			(reduce 	#(reduce conj %1 %2)
						(map	#(.intersect % ray)
								objects)))
		(color-at [ this ray ]
			(.color-at (first-intersecting-object objects ray) ray)))
		

(defn parallelepiped-create
	[ origin v1 v2 v3 material ]
	(let [	origin-opposite		(geometry/vec-add
									origin
									(geometry/vec-add
										v1
										(geometry/vec-add
											v2
											v3)))
			mv1					(geometry/vec-mult v1 -1)
			mv2					(geometry/vec-mult v2 -1)
			mv3					(geometry/vec-mult v3 -1) 		]
	(Composite. 		[	(parallelogram-create
								origin
								v1
								v2
								material)
							(parallelogram-create
								origin
								v1
								v3
								material)
							(parallelogram-create
								origin
								v2
								v3
								material)
							(parallelogram-create
								origin-opposite
								mv1
								mv2
								material)
							(parallelogram-create
								origin-opposite
								mv1
								mv3
								material)
							(parallelogram-create
								origin-opposite
								mv2
								mv3
								material)				])))



(defn box-create
	[ origin v1 v2 v3 material ]
	(let 	[ 	dot12			(geometry/vec-dot-product v1 v2)
				dot13			(geometry/vec-dot-product v1 v3)
				dot23			(geometry/vec-dot-product v2 v3)	]
		(if (and	(< dot12 epsilon)	(> dot12 minus-epsilon)
					(< dot13 epsilon)	(> dot13 minus-epsilon)
					(< dot23 epsilon)	(> dot23 minus-epsilon))
			(parallelepiped-create origin v1 v2 v3 material)
			(throw (new IllegalArgumentException "Vectors are not perpendicular")))))