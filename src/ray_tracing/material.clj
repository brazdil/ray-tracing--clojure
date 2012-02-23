(ns ray-tracing.material)

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defrecord Colour [ #^Float r
					#^Float g 
					#^Float b ])

(defrecord Material [ colour-diffuse colour-specular ])

(defn colour-create
	[ r g b ]
	(Colour. r g b))

(def colour-black 	(colour-create    0    0    0))
(def colour-white 	(colour-create    1    1    1))
(def colour-red 	(colour-create    1    0    0))
(def colour-green 	(colour-create    0    1    0))
(def colour-blue 	(colour-create    0    0    1))
(def colour-gray 	(colour-create 0.25 0.25 0.25))

(defn colour-to-java
	[ colour ]
	(new java.awt.Color (float (:r colour)) (float (:g colour)) (float (:b colour))))

(defn- colour-add
	[ c1 c2 ]
	(colour-create 	(+ (:r c1) (:r c2))
					(+ (:g c1) (:g c2))
					(+ (:b c1) (:b c2))))

(defn colour-mult-scalar
	"Multiplies each of the color parts by a given factor"
	[ colour intensity ]
	(colour-create 	(* (:r colour) intensity)
					(* (:g colour) intensity)
					(* (:b colour) intensity)))

(defn colour-mult
	"Multiplies each of the color parts by intensity of the same part in the other argument"
	[ colour intensity ]
	(colour-create 	(* (:r colour) (:r intensity))
					(* (:g colour) (:g intensity))
					(* (:b colour) (:b intensity))))

(defn colour-average
	"Mixes all the colours together"
	[ colours ]
	(let [ total-count 		(count colours) ]
		(colour-mult-scalar
			(reduce colour-add colour-black colours)
			(/ 1 total-count))))

(defn material-create
	[ colour-diffuse colour-specular ]
	(Material. colour-diffuse colour-specular))

(defn material-create-simple
	[ colour ]
	(material-create colour (colour-create 0 0 0)))

(defn material-mix
	[ material light-diffuse ]
	(colour-mult (:colour-diffuse material) light-diffuse))