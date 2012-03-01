(ns raytracing.network
	(:require [lamina.core :as lamina])
	(:require [raytracing.drawing :as drawing])
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
	[ root-object lights projection computer-queue io-agent coords ]
	(let
		; get a computer 
		[ computer 	(lamina/wait-for-message computer-queue) ]
		; (send-off 	io-agent
		; 			#(do (println 
		; 					(str "Computing (" (get coords 0) ", " (get coords 1)
		; 				  		 ") using " (:name computer)))
		; 				 %))
		; compute the value
		(let [ value 	(try 	(.. (java.rmi.registry.LocateRegistry/getRegistry (:ip computer) (:port computer))
									(lookup server-name) (getPixelClassic coords))
								(catch Exception e (println e)))	]
			; put the computer back into the queue
			(lamina/enqueue computer-queue computer)
			; decide what to do next
			(if (nil? value)
				; couldn't compute => try different computer
				(do
					(send-off 	io-agent
								#(do (println 
										(str "FAILED computing (" (first coords) ", " (first (rest coords)) 
									  		 ") using " (:name computer)))
									 %))
					; try it again
					(recur root-object lights projection computer-queue io-agent coords))
				; computed ! return the value
				value))))

(defn- init-computer
	[ root-object lights projection computer-queue computer ]
	(let [ value 	(try 	(.. (java.rmi.registry.LocateRegistry/getRegistry (:ip computer) (:port computer))
								(lookup server-name) (init root-object lights projection))
							(catch Exception e (println e)))	]
		(if (= (dbg value) :initialized)
			(lamina/enqueue computer-queue computer)
			(println (str (:name computer) " wasn't initialized") ))))


(defn generate-pixels
 	"Draws the scene distributively"
 	[ root-object lights projection computers ]
 	(let [ 	computer-queue 	(lamina/channel) 
 			io-agent		(agent 0)			]
		; put all the computers in the queue
		(dorun (map #(init-computer root-object lights projection computer-queue %) computers))
		; create the sequence that will compute everything distributively
		(pmap 	#(get-pixel-classic
					root-object
					lights
					projection
					computer-queue
					io-agent
					(vec %))
				(drawing/pixels-seq
					(:width projection) 
					(:height projection)))))

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
	    	(println "Initializing...")
    		:initialized))
    (getPixelClassic [ coords ] 
    	(do ; (print "Computing " coords "... ")
    		(let [ result   	(drawing/get-pixel-classic (:root-object @server-data)
						    				               (:lights @server-data)
						    				               (:projection @server-data)
						    				               coords) ]
    			; (println "OK")
    			result)))
    ))

(def server-object (server-generator))

(defn server-start [ port ]
	(do
		(swap! server-registry (fn [ a ] (java.rmi.registry.LocateRegistry/createRegistry port) ))
  		(.bind
 			(java.rmi.registry.LocateRegistry/getRegistry)
			server-name
   			(java.rmi.server.UnicastRemoteObject/exportObject server-object 0))))

