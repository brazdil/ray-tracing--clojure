(ns ray-tracing.main
	(:require [ray-tracing.drawing :as drawing])
	(:require [ray-tracing.math :as math])
	(:require [ray-tracing.geometry :as geometry])
	(:require [ray-tracing.material :as material])
	(:require [ray-tracing.lighting :as lighting])
	(:require [ray-tracing.object :as object]))

(def camera 	(drawing/camera-create
					(geometry/vec-create 1.0 1.6 -3.7)
					(geometry/vec-create -5 1.8 5)
					(geometry/vec-create 0 1 0)))

(def projection	(drawing/projection-create
					(java.lang.Math/toRadians 60)
					2
					1920
					1080
					(material/colour-create 0.858823529		; light blue background
											0.909803922
											0.831372549 )))

(def sphere 	(object/sphere-create
					(geometry/vec-create -0.5 0.6 4)
					0.6
					(material/material-create-simple
						material/colour-pastel-cyan )))

; (def box1		(object/box-create
; 					(geometry/vec-create  1   -1   4)
; 					1
; 					0.75
; 					1
; 					(material/material-create-simple
; 						material/colour-green)))

(def chessboard 	(object/chessboard-create 
						(geometry/vec-create -8 0 0)
						8
						8
						0.2
						(material/material-create-simple material/colour-pastel-light-gray)
						(material/material-create-simple material/colour-pastel-white)))

; (def floor 		(.debug
; 				(.translate 
; 					(.debug
; 					(.rotateX
; 						(.debug
; 						(object/rectangle-create-normal
; 							10
; 							20
; 							geometry/vec-z-neg
; 							(material/material-create-simple
; 								material/colour-gray)))
; 						math/PIover2))
; 					(geometry/vec-create -5 -1 0))))

(def light1		(lighting/light-create
					(geometry/vec-create 10 30 20)
					material/colour-white))

(def objects [ sphere chessboard ])

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
