(ns ray-tracing.object
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material])
	(:require [ray-tracing.lighting :as lighting]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defprotocol PObject
	"Interface that each object in the scene has to implement"
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

(defrecord Light [ position colour ])

(defn light-create
	[ position colour ]
	(Light. position colour))

(defn- light-compute-diffuse-single
	[ objects light point normal ]
	(if (is-intersected objects point (:position light))
		material/colour-black
		(let [ angle-dot-prod 	(geometry/vec-dot-product
									normal
									(geometry/vec-normalize
										(geometry/vec-subtract
											(:position light)
											point))) 			]
			(if (< angle-dot-prod 0)
				material/colour-black
				(material/colour-mult-scalar 	(:colour light)
			 									angle-dot-prod)))))

(defn light-compute-diffuse
	[ objects lights point normal ]
	(material/colour-average
		(map	#(light-compute-diffuse-single objects % point normal)
				lights)))

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
		(colour-at [ this objects lights ray ]
			(let [ intersection 	(geometry/ray-point 	
										ray
										(first-intersection this ray)) ]
			(material/material-mix 	material
									(light-compute-diffuse
										objects
										lights
										intersection
										(geometry/vec-normalize
											(geometry/vec-subtract
												intersection
												center)))))))

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
		(colour-at [ this objects lights ray ]
			(material/material-mix 	material
									(light-compute-diffuse
										objects
										lights
										(geometry/ray-point 	
											ray
											(first-intersection this ray))
										N))))
												
(defn parallelogram-create-normal
	[ origin v1 v2 N material]
	(let [ 	N 			(geometry/vec-normalize N)
			len-v1 		(geometry/vec-length v1)
	       	len-v2 		(geometry/vec-length v2)
			d 			(geometry/vec-dot-product N origin) ]
	(Parallelogram. 	origin 
						v1 
						v2 
						material
						N
						d
						(* len-v1 len-v1)
						(* len-v2 len-v2) )))

(defn parallelogram-create
	[ origin v1 v2 material ]
	(parallelogram-create-normal
	 	origin
		v1 
		v2 
       	(geometry/vec-normalize
			(geometry/vec-vector-product v1 v2))
		material))


(defn rectangle-create
	[ origin v1 v2 material ]
	(let 	[ 	dot12			(geometry/vec-dot-product v1 v2)	]
		(if (and	(< dot12 epsilon)	(> dot12 minus-epsilon))
			(parallelogram-create origin v1 v2 material)
			(throw (new IllegalArgumentException "Vectors are not perpendicular")))))

(defn rectangle-create-normal
	[ origin v1 v2 N material ]
	(let 	[ 	dot12			(geometry/vec-dot-product v1 v2)	]
		(if (and	(< dot12 epsilon)	(> dot12 minus-epsilon))
			(parallelogram-create-normal origin v1 v2 N material)
			(throw (new IllegalArgumentException "Vectors are not perpendicular")))))

(deftype Composite
	[ sub-objects ]
	PObject
		(intersect [ this ray ]
			(reduce 	#(reduce conj %1 %2)
						(map	#(.intersect % ray)
								sub-objects)))
		(colour-at [ this objects lights ray ]
			(.colour-at (first-intersecting-object sub-objects ray) objects lights ray)))
		

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
