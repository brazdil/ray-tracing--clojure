(ns ray-tracing.main
	(:require [ray-tracing.drawing :as drawing])
	(:require [ray-tracing.geometry :as geometry])
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
					java.awt.Color/BLACK ))

(def sphere1 	(object/sphere-create
					(geometry/vec-create -1 0 7)
					1
					java.awt.Color/RED ))

(def sphere2 	(object/sphere-create
					(geometry/vec-create -2 0 7)
					0.75
					java.awt.Color/BLUE ))

(def floor    	(object/parallelogram-create
					(geometry/vec-create -2 -1 0)
					(geometry/vec-create 4 0 0)
					(geometry/vec-create 0 0 10)
					java.awt.Color/GRAY))

(defn test-draw []
	(drawing/draw [ floor sphere1 sphere2 ] camera projection))

(defn test-save []
	(drawing/save-as-png 	"test.png"
							projection
							(test-draw)))

(defn test-realtime []
	(drawing/show-realtime 	projection
							(test-draw)))
