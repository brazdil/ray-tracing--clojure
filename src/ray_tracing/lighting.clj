(ns ray-tracing.lighting
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.object-common :as object-common])
	(:require [ray-tracing.material :as material]))

(defrecord Light [ position colour ])

(defn light-create
	[ position colour ]
	(Light. position colour))

(defn- light-compute-diffuse-single
	[ objects light point normal ]
	(if (object-common/is-intersected objects point (:position light))
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
