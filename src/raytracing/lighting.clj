(ns raytracing.lighting
	(:require [raytracing.geometry :as geometry])
	(:require [raytracing.object-common :as object-common])
	(:require [raytracing.material :as material]))

(defrecord Light [ position colour ])

(defn light-create
	[ position colour ]
	(Light. position colour))

(defn- light-compute-diffuse-single
	[ root-object light point normal ]
	(if (object-common/is-intersected root-object point (:position light))
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
	[ root-object lights point normal ]
	(material/colour-average
		(map	#(light-compute-diffuse-single root-object % point normal)
				lights)))
