(ns raytracing.object
	(:require [raytracing.math :as math])
	(:require [raytracing.geometry :as geometry])
	(:require [raytracing.object-common :as object-common])
	(:require [raytracing.material :as material])
	(:require [raytracing.lighting :as lighting]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

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
							 	(object-common/first-intersection % ray)}) 
						objects)))

(defn first-intersecting-object
	"Returns the object that is first hit by the ray, or nil if there isn't any."
	[ objects ray ]
	(let [ first-struct		(first-intersecting-struct objects ray)	]
		(if (or 	(nil? first-struct)
					(< (:first-intersect first-struct) object-common/epsilon))
			nil
			(:object first-struct))))

(defn first-intersecting-distance
	"Returns the distance to first object hit by the ray (in units), 
	 or +INFINITY if there isn't any."
	[ objects ray ]
	(let [ first-struct		(first-intersecting-struct objects ray)	]
		(if (or 	(nil? first-struct)
					(< (:first-intersect first-struct) object-common/epsilon))
			java.lang.Double/POSITIVE_INFINITY
			(:first-intersect first-struct))))

(defrecord Composite
	[ sub-objects ]
	object-common/PObject
		(debug [ this ]
			(dorun (map #(.debug %) sub-objects))
			this)
		(translate [ this v ]
			(Composite.
				(map #(.translate % v) sub-objects)))
		(rotateX [ this angle ]
			(Composite.
				(map #(.rotateX % angle) sub-objects)))
		(rotateY [ this angle ]
			(Composite.
				(map #(.rotateY % angle) sub-objects)))
		(rotateZ [ this angle ]
			(Composite.
				(map #(.rotateZ % angle) sub-objects)))
		(scale [ this amount ]
			(Composite.
				(map #(.scale % amount) sub-objects)))
		(flip-normal [ this ]
			(Composite.
				(map #(.flip-normal %) sub-objects)))
		(intersect [ this ray ]
			(reduce 	#(reduce conj %1 %2)
						(map	#(.intersect % ray)
								sub-objects)))
		(colour-at [ this root-object lights ray ]
			(.colour-at
				(first-intersecting-object 
					sub-objects ray) 
				root-object 
				lights 
				ray)))
		
(defn composite-create
	[ objects ]
	(Composite. objects))

(defn composite-merge
	[ comp1 comp2 ]	
	(composite-create
		(reduce 	conj
					(:sub-objects comp1)
					(:sub-objects comp2))))

(defrecord Sphere
	[ center radius inside-out material ]
	object-common/PObject
		(debug [ this ]
			(do	
				(dbg center)
				(dbg radius)
				this))
		(translate [ this v ]
			(Sphere. (geometry/vec-add center v) radius inside-out material))
		(rotateX [ this angle ]
			this)
		(rotateY [ this angle ]
			this)
		(rotateZ [ this angle ]
			this)
		(scale [ this amount ]
			(Sphere. center (* radius amount) inside-out material))
		(flip-normal [ this ]
			(Sphere. center radius (not inside-out) material))
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
		(colour-at [ this root-object lights ray ]
			(let [ intersection 	(geometry/ray-point 	
										ray
										(object-common/first-intersection this ray)) ]
			(material/material-mix 	material
									(lighting/light-compute-diffuse
										root-object
										lights
										intersection
										(geometry/vec-normalize
											(if inside-out
												(geometry/vec-subtract center intersection)
												(geometry/vec-subtract intersection center))))))))

(defn sphere-create
	(	[ origin radius material ]
		(Sphere. origin radius false material))
	(	[ radius material ]
		(sphere-create (geometry/vec-create 0 0 0) radius material))
	(	[ material ]
		(sphere-create 1 material)))

(defrecord Rectangle
	[ origin v1 v2 N material d len-v1-sq len-v2-sq ]
	object-common/PObject
		(debug [ this ]
			(do	
				(dbg origin)
				(dbg v1)
				(dbg v2)
				(dbg N)
				(dbg d)
				this))
		(translate [ this v ]
			(let [ newOrigin 	(geometry/vec-add origin v) ]
				(Rectangle. 
					newOrigin 
					v1 
					v2 
					N 
					material 
					(geometry/vec-dot-product N newOrigin)
					len-v1-sq 
					len-v2-sq)))
		(rotateX [ this angle ]
			(let [ 	newOrigin	(geometry/vec-rotate-x origin angle)
					newN 		(geometry/vec-subtract
									(geometry/vec-rotate-x
										(geometry/vec-add N origin)
										angle)
								origin)				]
			(Rectangle.
				newOrigin
				(geometry/vec-rotate-x v1 angle)
				(geometry/vec-rotate-x v2 angle)
				newN
				material
				(geometry/vec-dot-product newN newOrigin)
				len-v1-sq
				len-v2-sq)))
		(rotateY [ this angle ]
			(let [ 	newOrigin	(geometry/vec-rotate-y origin angle)
					newN 		(geometry/vec-subtract
									(geometry/vec-rotate-y
										(geometry/vec-add N origin)
										angle)
								origin)				]
			(Rectangle.
				newOrigin
				(geometry/vec-rotate-y v1 angle)
				(geometry/vec-rotate-y v2 angle)
				newN
				material
				(geometry/vec-dot-product newN newOrigin)
				len-v1-sq
				len-v2-sq)))
		(rotateZ [ this angle ]
			(let [ 	newOrigin	(geometry/vec-rotate-z origin angle)
					newN 		(geometry/vec-subtract
									(geometry/vec-rotate-z
										(geometry/vec-add N origin)
										angle)
								origin)				]
			(Rectangle.
				newOrigin
				(geometry/vec-rotate-z v1 angle)
				(geometry/vec-rotate-z v2 angle)
				newN
				material
				(geometry/vec-dot-product newN newOrigin)
				len-v1-sq
				len-v2-sq)))
		(scale [ this amount ]
			this)
		(flip-normal [ this ]
			(Rectangle.
				origin
				v1
				v2
				(geometry/vec-subtract geometry/vec-zero N)
				material
				(- d)
				len-v1-sq
				len-v2-sq))
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
		(colour-at [ this root-object lights ray ]
			(material/material-mix 	material
									(lighting/light-compute-diffuse
										root-object
										lights
										(geometry/ray-point 	
											ray
											(object-common/first-intersection this ray))
										N))))
												
(defn rectangle-create-normal
	(	[ origin sizeX sizeY normal material ]
		(let [ 	v1			(geometry/vec-create sizeX 0 0)
				v2 			(geometry/vec-create 0 sizeY 0)
				N 			(geometry/vec-normalize normal)
				len-v1 		(geometry/vec-length v1)
		       	len-v2 		(geometry/vec-length v2)
				d 			(geometry/vec-dot-product N origin) ]
		(Rectangle. 	origin 
						v1
						v2
						N
						material
						d
						(* len-v1 len-v1)
						(* len-v2 len-v2) )))
	(	[ sizeX sizeY normal material ]
		(rectangle-create-normal geometry/vec-zero sizeX sizeY normal material)))

(defn rectangle-create
	(	[ origin sizeX sizeY material ]
		(rectangle-create-normal origin sizeX sizeY geometry/vec-z-pos material))
	(	[ sizeX sizeY material ]
		(rectangle-create geometry/vec-zero sizeX sizeY material)))

(defn box-create
	(	[ origin sizeX sizeY sizeZ material ]
		(let [ 	rect-xy1 	(rectangle-create-normal
								sizeX
								sizeY
								geometry/vec-z-neg
								material)
				rect-xy2	(.. rect-xy1
								(flip-normal)
								(translate (geometry/vec-create 0 0 sizeZ)))
				rect-xz1 	(.rotateX
								(rectangle-create-normal
									sizeX
									sizeZ
									geometry/vec-z-pos
									material)
								math/PIover2)
				rect-xz2	(.. rect-xz1
								(flip-normal)
								(translate (geometry/vec-create 0 sizeY 0)))
				rect-yz1 	(.rotateY
								(rectangle-create-normal
									sizeZ
									sizeY
									geometry/vec-z-pos
									material)
								(- math/PIover2))
				rect-yz2	(.. rect-yz1
								(flip-normal)
								(translate (geometry/vec-create sizeX 0 0)))	]
		(.translate
			(composite-create	[ rect-xy1 rect-xz1 rect-yz1 rect-xy2 rect-xz2 rect-yz2 ])
			origin)))
	(	[ sizeX sizeY sizeZ material ]
		(box-create geometry/vec-zero sizeX sizeY sizeZ material)))

(defn chessboard-create
	"Expects to be given the origin and size of the corner box and creates
	 a chessboard of given size from it."
	(	[ origin rows cols thickness material-black material-white ]
		(.. (reduce composite-merge
				(composite-create
					(map 	#(let [	xpos 		(first %) 
									ypos 		(first (rest %))

									pos 		(geometry/vec-create xpos 0 ypos)
									material 	(if (odd? (+ xpos ypos))
													material-black
													material-white) ]
								(..	(rectangle-create-normal
										1 1
										geometry/vec-z-neg
										material)
									(rotateX math/PIover2) (translate pos)))
							(math/cartesian-product (range 0 cols) (range 0 rows))))
				[ (composite-create
					(map 	#(let [ material-front 	(if (odd? %)
														material-black
														material-white)
									material-back 	(if (even? %)
														material-black
														material-white)					

									rect-front 		(.. (rectangle-create-normal
															1 thickness
															geometry/vec-z-neg
															material-front)
														(translate (geometry/vec-create % (- thickness) 0)))
									rect-back 		(.. (rectangle-create-normal
															1 thickness
															geometry/vec-z-pos
															material-back)
														(translate (geometry/vec-create % (- thickness) rows))) 	]
								(composite-create [ rect-front rect-back ]))
							(range 0 cols)))
				(composite-create
					(map 	#(let [ material-front 	(if (odd? %)
														material-black
														material-white)
									material-back 	(if (even? %)
														material-black
														material-white)					

									rect-front 		(.. (rectangle-create-normal
															1 thickness
															geometry/vec-z-pos
															material-front)
														(rotateY (- math/PIover2))
														(translate (geometry/vec-create 0 (- thickness) %)))
									rect-back 		(.. (rectangle-create-normal
															1 thickness
															geometry/vec-z-neg
															material-back)
														(rotateY (- math/PIover2))
														(translate (geometry/vec-create cols (- thickness) %))) 	]
								(composite-create [ rect-front rect-back ]))
							(range 0 rows))) ])
			(translate origin) ))
	( 	[ rows cols thickness material-black material-white ]
		(chessboard-create geometry/vec-zero rows cols thickness material-black material-white)))

