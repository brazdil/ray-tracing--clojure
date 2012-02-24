(ns ray-tracing.main
	(:require [ray-tracing.drawing :as drawing])
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material])
	(:require [ray-tracing.lighting :as lighting])
	(:require [ray-tracing.object :as object]))

(def camera 	(drawing/camera-create
					(geometry/vec-create 0 0 0)
					(geometry/vec-create 0 0 1)
					(geometry/vec-create 0 1 0)))

(def projection	(drawing/projection-create
					(java.lang.Math/toRadians 60)
					2
					1024
					768
					(material/colour-create 0.858823529		; light blue background
											0.909803922
											0.831372549 )))

(def sphere1 	(object/sphere-create
					(geometry/vec-create -1 0 7)
					1
					(material/material-create-simple
						material/colour-red )))

(def sphere2 	(object/sphere-create
					(geometry/vec-create -2 0 7)
					0.75
					(material/material-create-simple
						material/colour-blue )))

(def box1		(object/box-create
					(geometry/vec-create  1   -1   4)
					(geometry/vec-create  1    0   0)
					(geometry/vec-create  0 0.75   0)
					(geometry/vec-create  0    0   1)
					(material/material-create-simple
						material/colour-green)))

(def floor    	(object/rectangle-create-normal
					(geometry/vec-create -2 -1 0)
					(geometry/vec-create 4 0 0)
					(geometry/vec-create 0 0 10)
					(geometry/vec-create 0 1 0)
					(material/material-create-simple
						material/colour-gray)))

(def light1		(lighting/light-create
					(geometry/vec-create 10 30 20)
					material/colour-white))

(def objects [ floor sphere1 sphere2 box1 ])

(def lights [ light1 ])


(defn test-draw []
	(drawing/draw-simple objects lights camera projection))

(defn test-save []
	(drawing/save-as-png 	"test.png"
							projection
							(test-draw)))

(defn test-realtime []
	(drawing/show-realtime 	projection
							(test-draw)))
