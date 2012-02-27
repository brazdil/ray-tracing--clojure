(ns ray-tracing.network
	(:require [lamina.core :as lamina])
	(:require [ray-tracing.drawing :as drawing]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; SERVER PART

(def server-name "ray-tracing-server-v1")

(defn- server-generator []
  (proxy [ray_tracing.RayTracingRMI] [] 
    (ping [] :alive)))

(def server-registry (atom nil))
(def server-object (server-generator))

(defn server-start [ port ]
	(do
		(swap! server-registry (fn [ a ] (java.rmi.registry.LocateRegistry/createRegistry port) ))
  		(.bind
 			(java.rmi.registry.LocateRegistry/getRegistry)
			server-name
   			(java.rmi.server.UnicastRemoteObject/exportObject server-object 0))))

; CLIENT PART

(defrecord Computer [ name ip port ] )

(defn computer-create
	"Creates a computer. IP is a string, port is an integer."
	[ name ip port ]
	(Computer. name ip port))



(defn check-computers
	[ computers ]
	(do 
		(dorun 	(map 	#(do 	(print (str "Pinging " (:name %) " (" (:ip %) ") on port " (:port %) "... "))
							(let [ state 		(try 	(.ping 
															(.lookup 
																(java.rmi.registry.LocateRegistry/getRegistry 
																	(:ip %) (:port %)) 
																server-name))
								   						(catch Exception e (println (.getMessage e)))) ]
								(if (= :alive state)
									(println "OK")
									(println "not responding"))
								{:computer % :state state}))
						computers))
		nil))

; (defn- get-pixel
; 	[ root-object lights projection func computer-queue io-agent coords ]
; 	(let
; 		; get a computer 
; 		[ computer 	(lamina/wait-for-message computer-queue) ]
; 		(send-off 	io-agent
; 					#(do (println 
; 							(str "Computing (" (get coords 0) ", " (get coords 1)
; 						  		 ") using " (:name computer)))
; 						 %))
; 		; compute the value
; 		(let [ value 	(deref (first 
; 						(net-eval/net-eval 
; 							[[ (:ip computer) (:port computer) 
; 								#'get-pixel-task 
; 								{ :coords 	coords
; 								  :func   	1 
; 								} ]]))) ]
; 			; put the computer back into the queue
; 			(lamina/enqueue computer-queue computer)
; 			; decide what to do next
; 			(if (nil? (dbg value))
; 				; couldn't compute => try different computer
; 				(do
; 					(send-off 	io-agent
; 								#(do (println 
; 										(str "FAILED computing (" (first coords) ", " (first (rest coords)) 
; 									  		 ") using " (:name computer)))
; 									 %))
; 					; try it again
; 					(recur root-object lights projection func computer-queue io-agent coords))
; 				; computed ! return the value
; 				value))))

; (defn generate-pixels
;  	"Draws the scene distributively"
;  	[ root-object lights projection func computers ]
;  	(let [ 	computer-queue 	(lamina/channel) 
;  			io-agent		(agent 0)			]
;  		(do
;  			; put all the computers in the queue
;  			(dorun (map #(lamina/enqueue computer-queue %) computers))
;  			; create the sequence that will compute everything distributively
; 		(pmap 	#(get-pixel
; 					root-object
; 					lights
; 					projection
; 					func
; 					computer-queue
; 					io-agent
; 					(vec %))
; 				(drawing/pixels-seq
; 					(:width projection) 
; 					(:height projection))))))