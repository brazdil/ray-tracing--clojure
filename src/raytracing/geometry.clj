(ns raytracing.geometry
	(:require [raytracing.math :as math]))

; (set! *warn-on-reflection* true)

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defrecord Vector [ ^Double x
					^Double y
					^Double z ])

(defrecord Ray [ ^Vector point 
			     ^Vector direction ])

(defrecord BoundingBox [ 	^Double xmin 
							^Double xmax 
							^Double ymin 
							^Double ymax 
							^Double zmin 
							^Double zmax ])

(defn- fn-comp	
	[ coord cmp a b ]
	(if (cmp a (coord b))
		a
		(coord b)))

(defn vec-create 
	[ x y z ]
	(Vector. x y z))

(def vec-zero 	(vec-create 0 0 0))
(def vec-x-pos 	(vec-create 1 0 0))
(def vec-x-neg 	(vec-create -1 0 0))
(def vec-y-pos 	(vec-create 0 1 0))
(def vec-y-neg 	(vec-create 0 -1 0))
(def vec-z-pos 	(vec-create 0 0 1))
(def vec-z-neg 	(vec-create 0 0 -1))

(defn vec-add
	"Adds two vectors"
	[ v1 v2 ]
	(vec-create	   (+ (:x v1) (:x v2))
	               (+ (:y v1) (:y v2))
	               (+ (:z v1) (:z v2))))

(defn vec-subtract
	"Subtracts two vectors"
	[ v1 v2 ]
	(vec-create	   (- (:x v1) (:x v2))
	               (- (:y v1) (:y v2))
	               (- (:z v1) (:z v2))))

(defn vec-mult
	"Multiplies a vector by a scalar"
	[ v k ]
	(vec-create	   (* k (:x v))
	               (* k (:y v))
	               (* k (:z v))))

(defn vec-length
	"Returns the length of a vector"
	[ v ]
	(java.lang.Math/sqrt
		(+  (* (:x v) (:x v)) 
			(* (:y v) (:y v)) 
			(* (:z v) (:z v)))))

(defn vec-normalize
	"Returns unit length vector"
	[ v ]
	(vec-mult v (/ 1 (vec-length v))))

(defn vec-dot-product
	"Returns the dot product of two vectors"
	[ v1 v2 ]
	(+  (* (:x v1) (:x v2)) 
		(* (:y v1) (:y v2)) 
		(* (:z v1) (:z v2))))

(defn vec-vector-product
	"Returns the vector product of two vectors"
	[ v1 v2 ]
	(vec-create	   	(- 	(* (:y v1) (:z v2))
						(* (:y v2) (:z v1)))
					(- 	(* (:z v1) (:x v2))
					  	(* (:z v2) (:x v1)))
					(- 	(* (:x v1) (:y v2))
					  	(* (:x v2) (:y v1)))))

; (defn matrix-transpose
; 	[ s ]
; 	(apply map vector s))
 
; (defn- nested-for
; 	[ f x y ]
; 	(map 	(fn [a]
; 				(map (fn [b] 
;                 	(f a b)) y))
; 			x))
 
; (defn matrix-mult
; 	[ a b ]
; 	(nested-for 
; 		(fn [x y] (reduce + (map * x y))) a (matrix-transpose b)))

; (defn vec-to-matrix
; 	[ v ]
; 	[ [(:x v)] [(:y v)] [(:z v)] ])

(defn vec-rotate-x
	"Rotation around the x-axis"
	[ v angle ]
	(let [ angle-sin	(java.lang.Math/sin angle)
	       angle-cos	(java.lang.Math/cos angle) ]
		(vec-create
			(:x v)
			(- (* (:y v) angle-cos) (* (:z v) angle-sin))
			(+ (* (:y v) angle-sin) (* (:z v) angle-cos)))))

(defn vec-rotate-y
	"Rotation around the y-axis"
	[ v angle ]
	(let [ angle-sin	(java.lang.Math/sin angle)
	       angle-cos	(java.lang.Math/cos angle) ]
		(vec-create
			(+ (* (:x v) angle-cos) (* (:z v) angle-sin))
			(:y v)
			(- (* (:z v) angle-cos) (* (:x v) angle-sin)))))

(defn vec-rotate-z
	"Rotation around the z-axis"
	[ v angle ]
	(let [ angle-sin	(java.lang.Math/sin angle)
	       angle-cos	(java.lang.Math/cos angle) ]
		(vec-create
			(- (* (:x v) angle-cos) (* (:y v) angle-sin))
			(+ (* (:x v) angle-sin) (* (:y v) angle-cos))
			(:z v))))

(defn vec-rotate
	"Rotates the vector around an arbitrary axis (intersecting the origin). The axis has to be a unit vector."
	[ v1 axis angle ]
	(let [ x			(:x v1)
	       y			(:y v1)
	       z			(:z v1)
		   u 			(:x axis)
	       v 			(:y axis)
	       w 			(:z axis)
	       angle-sin	(java.lang.Math/sin angle)
	       angle-cos	(java.lang.Math/cos angle)
	       angle-1Mcos	(- 1 angle-cos)
	       uxPvyPwz		(+ (* u x) (* v y) (* w z)) ]
		(vec-create
			(+ 	(* u uxPvyPwz angle-1Mcos) 
				(* x angle-cos) 
				(* 
					(- (* v z) (* w y) )
					angle-sin))
			(+ 	(* v uxPvyPwz angle-1Mcos) 
				(* y angle-cos) 
				(* 
					(- (* w x) (* u z) )
					angle-sin))
			(+ 	(* w uxPvyPwz angle-1Mcos) 
				(* z angle-cos) 
				(* 
					(- (* u y) (* v x) )
					angle-sin)))))

(defn ray-create
	[ point direction ]
	(Ray. point (vec-normalize direction)))

(defn ray-point
	"Returns point on the ray line given by the multiple of lengths of
	 the direction vector from the starting point (which is normalized,
	 so technically units)."
	 [ ray param ]
	 (vec-add
	 	(:point ray)
	 	(vec-mult
	 		(:direction ray)
	 		param)))

(defn bounding-box-merge
	[ bboxes ]
	(BoundingBox.
		(reduce (partial fn-comp :xmin <) math/INFINITY bboxes)
		(reduce (partial fn-comp :xmax >) math/mINFINITY bboxes)
		(reduce (partial fn-comp :ymin <) math/INFINITY bboxes)
		(reduce (partial fn-comp :ymax >) math/mINFINITY bboxes)
		(reduce (partial fn-comp :zmin <) math/INFINITY bboxes)
		(reduce (partial fn-comp :zmax >) math/mINFINITY bboxes)))

(defn bounding-box-create
	( [ vertices ]
		(BoundingBox.
			(reduce (partial fn-comp :x <) math/INFINITY vertices)
			(reduce (partial fn-comp :x >) math/mINFINITY vertices)
			(reduce (partial fn-comp :y <) math/INFINITY vertices)
			(reduce (partial fn-comp :y >) math/mINFINITY vertices)
			(reduce (partial fn-comp :z <) math/INFINITY vertices)
			(reduce (partial fn-comp :z >) math/mINFINITY vertices)))
	( [ xmin xmax ymin ymax zmin zmax ]
		(BoundingBox. xmin xmax ymin ymax zmin zmax)))

(defn bounding-box-intersects
	[ bbox ray ]
	; implementation of http://pages.cpsc.ucalgary.ca/~blob/ps/jgt04.pdf
	; code at http://jgt.akpeters.com/papers/MahovskyWyvill04/
	(let [ D 	(:direction ray)
	       O 	(:point ray)
	     ]
		(if (< (:x D) 0)
			(if (< (:y D) 0)
				(if (< (:z D) 0)
					; case MMM
					(if (or (< (:x O) (:xmin bbox))
							(< (:y O) (:ymin bbox))
							(< (:z O) (:zmin bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) ya) (* (:y D) xb)) 0)
										(> (- (* (:x D) yb) (* (:y D) xa)) 0)
										(< (- (* (:x D) za) (* (:z D) xb)) 0)
										(> (- (* (:x D) zb) (* (:z D) xa)) 0)
										(< (- (* (:y D) za) (* (:z D) yb)) 0)
										(> (- (* (:y D) zb) (* (:z D) ya)) 0))
								false
								true)))
					; case MMP
					(if (or (< (:x O) (:xmin bbox))
							(< (:y O) (:ymin bbox))
							(> (:z O) (:zmax bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) ya) (* (:y D) xb)) 0)
										(> (- (* (:x D) yb) (* (:y D) xa)) 0)
										(< (- (* (:x D) za) (* (:z D) xa)) 0)
										(> (- (* (:x D) zb) (* (:z D) xb)) 0)
										(< (- (* (:y D) za) (* (:z D) ya)) 0)
										(> (- (* (:y D) zb) (* (:z D) yb)) 0))
								false
								true))))
				(if (< (:z D) 0)
					; case MPM
					(if (or (< (:x O) (:xmin bbox))
							(> (:y O) (:ymax bbox))
							(< (:z O) (:zmin bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) ya) (* (:y D) xa)) 0)
										(> (- (* (:x D) yb) (* (:y D) xb)) 0)
										(< (- (* (:x D) za) (* (:z D) xb)) 0)
										(> (- (* (:x D) zb) (* (:z D) xa)) 0)
										(< (- (* (:y D) zb) (* (:z D) yb)) 0)
										(> (- (* (:y D) za) (* (:z D) ya)) 0))
								false
								true)))
					; case MPP
					(if (or (< (:x O) (:xmin bbox))
							(> (:y O) (:ymax bbox))
							(> (:z O) (:zmax bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) ya) (* (:y D) xa)) 0)
										(> (- (* (:x D) yb) (* (:y D) xb)) 0)
										(< (- (* (:x D) za) (* (:z D) xa)) 0)
										(> (- (* (:x D) zb) (* (:z D) xb)) 0)
										(< (- (* (:y D) zb) (* (:z D) ya)) 0)
										(> (- (* (:y D) za) (* (:z D) yb)) 0))
								false
								true)))))
			(if (< (:y D) 0)
				(if (< (:z D) 0)
					; case PMM
					(if (or (> (:x O) (:xmax bbox))
							(< (:y O) (:ymin bbox))
							(< (:z O) (:zmin bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) yb) (* (:y D) xb)) 0)
										(> (- (* (:x D) ya) (* (:y D) xa)) 0)
										(< (- (* (:x D) zb) (* (:z D) xb)) 0)
										(> (- (* (:x D) za) (* (:z D) xa)) 0)
										(< (- (* (:y D) za) (* (:z D) yb)) 0)
										(> (- (* (:y D) zb) (* (:z D) ya)) 0))
								false
								true)))
					; case PMP
					(if (or (> (:x O) (:xmax bbox))
							(< (:y O) (:ymin bbox))
							(> (:z O) (:zmax bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) yb) (* (:y D) xb)) 0)
										(> (- (* (:x D) ya) (* (:y D) xa)) 0)
										(< (- (* (:x D) zb) (* (:z D) xa)) 0)
										(> (- (* (:x D) za) (* (:z D) xb)) 0)
										(< (- (* (:y D) za) (* (:z D) ya)) 0)
										(> (- (* (:y D) zb) (* (:z D) yb)) 0))
								false
								true))))
				(if (< (:z D) 0)
					; case PPM
					(if (or (> (:x O) (:xmax bbox))
							(> (:y O) (:ymax bbox))
							(< (:z O) (:zmin bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) yb) (* (:y D) xa)) 0)
										(> (- (* (:x D) ya) (* (:y D) xb)) 0)
										(< (- (* (:x D) zb) (* (:z D) xb)) 0)
										(> (- (* (:x D) za) (* (:z D) xa)) 0)
										(< (- (* (:y D) zb) (* (:z D) yb)) 0)
										(> (- (* (:y D) za) (* (:z D) ya)) 0))
								false
								true)))
					; case PPP
					(if (or (> (:x O) (:xmax bbox))
							(> (:y O) (:ymax bbox))
							(> (:z O) (:zmax bbox)))
						false
						(let [	xa 	(- (:xmin bbox) (:x O))
								ya 	(- (:ymin bbox) (:y O))
								za 	(- (:zmin bbox) (:z O))
								xb 	(- (:xmax bbox) (:x O))
								yb 	(- (:ymax bbox) (:y O))
								zb 	(- (:zmax bbox) (:z O)) ]
							(if (or 	(< (- (* (:x D) yb) (* (:y D) xa)) 0)
										(> (- (* (:x D) ya) (* (:y D) xb)) 0)
										(< (- (* (:x D) za) (* (:z D) xb)) 0)
										(> (- (* (:x D) zb) (* (:z D) xa)) 0)
										(< (- (* (:y D) zb) (* (:z D) ya)) 0)
										(> (- (* (:y D) za) (* (:z D) yb)) 0))
								false
								true))))))))
