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
					800
					600
					java.awt.Color/BLACK ))

(def sphere1 	(Sphere. 
					(struct geometry/Vector 3 0 15)
					2
					java.awt.Color/RED ))

(def sphere2 	(Sphere. 
					(struct geometry/Vector 5 0 15)
					1.5
					java.awt.Color/BLUE ))

(def square1    (Parallelogram. 
					(struct geometry/Vector -1 0 3)
					(struct geometry/Vector 2 0 2)
					(struct geometry/Vector 0 2 0)
					java.awt.Color/GREEN))

(defn test-draw []
	(drawing/draw [ sphere1 sphere2 square1 ] camera projection))

(defn test-save []
	(drawing/save-as-png 	"test.png"
							projection
							(test-draw)))

