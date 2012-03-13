package raytracing;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RayTracingRMI extends Remote {
    clojure.lang.Keyword ping() throws RemoteException;
    clojure.lang.Keyword init(Object root_object, 
    	                      clojure.lang.PersistentVector lights, 
    	                      Object projection) throws RemoteException;
    clojure.lang.PersistentVector getPixelsClassic(Integer col_from, Integer col_to) throws RemoteException;
    // Object getPixelAntialiased(clojure.lang.PersistentVector coords) throws RemoteException;
}