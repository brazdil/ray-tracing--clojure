(ns ray-tracing.main
	(:require [ray-tracing.drawing :as drawing])
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.object :as object])
	(:import (ray_tracing.object 	Sphere
									Parallelogram)))

(def camera 	(struct drawing/Camera
					(struct geometry/Vector 0 0 0)
					(struct geometry/Vector 0 0 1)
					(struct geometry/Vector 0 1 0)))

(def projection	(struct drawing/Projection 
					(java.lang.Math/toRadians 60)
					2
					1024
					768
					java.awt.Color/BLACK ))

(def sphere1 	(Sphere. 
					(struct geometry/Vector -1 0 7)
					1
					java.awt.Color/RED ))

(def sphere2 	(Sphere. 
					(struct geometry/Vector -2 0 7)
					0.75
					java.awt.Color/BLUE ))

(def floor    (object/create-parallelogram
					(struct geometry/Vector -2 -1 0)
					(struct geometry/Vector 4 0 0)
					(struct geometry/Vector 0 0 10)
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
