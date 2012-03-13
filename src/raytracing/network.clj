(ns raytracing.network
	(:require [lamina.core :as lamina])
	(:require [raytracing.drawing :as drawing])
	(:require [raytracing.math :as math])	
	(:require [raytracing.material :as material]))

(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def server-name "ray-tracing-server-v1")

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
								   						(catch Exception e nil)) ]
								(if (= :alive state)
									(println "OK")
									(println "FAILED"))
								{:computer % :state state}))
						computers))
		nil))

(defn- get-pixel-classic
	[ root-object lights projection computer-queue io-agent part-index part-count ]
	(let
		; get a computer 
		[ computer 	(lamina/wait-for-message computer-queue)
		; compute the range
		  part-size	(quot (:width projection) part-count)
		  col-from 	(* part-size part-index)
		  col-to	(if (= part-index (- part-count 1))
		  				(:width projection)
		  				(* part-size (inc part-index))) 	]

		(send-off 	io-agent
					#(do (println 
							(str "SENDING " col-from "-" col-to
						  		 " to " (:name computer)))
						 %))

		; compute the value
		(let [ value 	(try 	(.. (java.rmi.registry.LocateRegistry/getRegistry (:ip computer) (:port computer))
									(lookup server-name) (getPixelsClassic col-from col-to))
								(catch Exception e nil)) ] ; (println e)))	]
			; put the computer back into the queue
			(lamina/enqueue computer-queue computer)
			; decide what to do next
			(if (and (nil? value) (= (count value) (* (:height projection) (- col-to col-from))))
				; couldn't compute => try different computer
				(do
					(send-off 	io-agent
								#(do (println 
										(str "FAILED " col-from "-" col-to
									  		 " using " (:name computer)))
									 %))
					; try it again
					(recur root-object lights projection computer-queue io-agent part-index part-count))
				; computed ! return the value
				(do 
					(send-off 	io-agent
								#(do (println 
										(str "DONE " col-from "-" col-to
									  		 " using " (:name computer)))
									 %))
					value)))))

(defn- init-computer
	[ root-object lights projection computer-queue computer ]
	(let [ value 	(try 	(.. (java.rmi.registry.LocateRegistry/getRegistry (:ip computer) (:port computer))
								(lookup server-name) (init root-object lights projection))
							(catch Exception e nil)) ] ;(println e)))	]
		(if (= value :initialized)
			(lamina/enqueue computer-queue computer)
			(println (str (:name computer) " wasn't initialized") ))))


(defn generate-pixels
 	"Draws the scene distributively"
 	[ root-object lights projection computers ]
 	(let [ 	computer-queue 	(lamina/channel) 
 			io-agent		(agent 0)			
 			part-count 		(* 3 (count computers))		]
		; put all the available computers in the queue
		(dorun (map #(init-computer root-object lights projection computer-queue %) computers))
		; create the sequence that will compute everything distributively
		(reduce concat 
			(pmap 	#(get-pixel-classic
						root-object
						lights
						projection
						computer-queue
						io-agent
						%
						part-count)
					(range 0 part-count)))))

; SERVER PART

(def server-registry (atom nil))
(def server-data (atom nil))

(defn- server-generator []
  (proxy [raytracing.RayTracingRMI] [] 
    (ping [] :alive)
    (init [ root_object lights projection ]
    	(do 
	    	(swap! server-data
	    		   (fn [ a ] {:root-object root_object
	    		    :lights lights
	    		    :projection projection }))
    		:initialized))
    (getPixelsClassic [ col_from col_to ] 
    	(do (println "Computing " col_from "~" col_to "... ")
    		(let [ result   (pmap 	#(drawing/get-pixel-classic 	
    									(:root-object @server-data)
					               		(:lights @server-data)
			               				(:projection @server-data)
			               				%)
									(math/cartesian-product 
										(range col_from col_to) 
										(range 0 (:height (:projection @server-data))))) ]
    			(vec result))))
    ))

(def server-object (server-generator))

(defn server-start [ port ]
	(do
		(swap! server-registry (fn [ a ] (java.rmi.registry.LocateRegistry/createRegistry port) ))
  		(.bind
 			(java.rmi.registry.LocateRegistry/getRegistry)
			server-name
   			(java.rmi.server.UnicastRemoteObject/exportObject server-object 0))))

