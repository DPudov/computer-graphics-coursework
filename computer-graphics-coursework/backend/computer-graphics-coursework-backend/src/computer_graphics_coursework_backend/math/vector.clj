(ns computer_graphics_coursework_backend.math.vector
  (:require [clojure.core.matrix :as m])
  (:import (clojure.lang Counted Sequential Associative MapEntry Seqable ILookup IFn)
           (java.io Writer)))

(definterface Coords2D
  (^double getX [])
  (^double getY []))

(defn- add-hashcode [hash x]
  (+ hash (* 37 hash) (Float/floatToIntBits x)))

(deftype Vector2D [^double x ^double y]
  Coords2D
  (getX [_] x)
  (getY [_] y)
  Counted (count [_] 2)
  Sequential
  Associative
  (equiv [v o] (.equals v o))
  (containsKey [_ k]
    (case (int k)
      (0 1) true
      false))
  (entryAt [_ k]
    (case (int k)
      0 (MapEntry. 0 x)
      1 (MapEntry. 1 y)))
  (assoc [_ i v]
    (case (int i)
      0 (Vector2D. v y)
      1 (Vector2D. x v)))
  Seqable
  (seq [_] (list x y))
  ILookup
  (valAt [v i]
    (.valAt v i nil))
  (valAt [_ i not-found]
    (case (int i) 0 x 1 y not-found))
  IFn
  (invoke [v i]
    (.valAt v i))
  Object
  (toString [_]
    (str "#vec2d [" x " " y "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x) (add-hashcode y)))
  (equals [self v]
    (or (identical? self v)
        (and (instance? Vector2D v)
             (== x (.getX ^Vector2D v))
             (== y (.getY ^Vector2D v)))
        (and (counted? v)
             (== (count v) 2)
             (== x (v 0))
             (== y (v 1))))))

(definterface Coords3D
  (^double getX [])
  (^double getY [])
  (^double getZ []))

(deftype Vector3D [^double x ^double y ^double z]
  Coords3D
  (getX [_] x)
  (getY [_] y)
  (getZ [_] z)

  Counted
  (count [_] 3)

  Sequential

  Seqable
  (seq [_] (list x y z))

  ILookup
  (valAt [v i]
    (.valAt v i nil))
  (valAt [_ i not-found]
    (case (int i) 0 x 1 y 2 z not-found))

  Associative
  (equiv [v o] (.equals v o))
  (containsKey [_ k]
    (case (int k)
      (0 1 2) true
      false))
  (entryAt [_ k]
    (case (int k)
      0 (MapEntry. 0 x)
      1 (MapEntry. 1 y)
      2 (MapEntry. 2 z)))
  (assoc [_ i v]
    (case (int i)
      0 (Vector3D. v y z)
      1 (Vector3D. x v z)
      2 (Vector3D. x y v)))

  IFn
  (invoke [v i]
    (.valAt v i))

  Object
  (toString [_]
    (str "#vec3d [" x " " y " " z "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x)
        (add-hashcode y)
        (add-hashcode z)))
  (equals [self v]
    (or (identical? self v)
        (and (instance? Vector3D v)
             (== x (.getX ^Vector3D v))
             (== y (.getY ^Vector3D v))
             (== z (.getZ ^Vector3D v)))
        (and (counted? v)
             (== (count v) 3)
             (== x (v 0))
             (== y (v 1))
             (== z (v 2))))))

(definterface Coords4D
  (^double getX [])
  (^double getY [])
  (^double getZ [])
  (^double getW []))

(deftype Vector4D [^double x ^double y ^double z ^double w]
  Coords4D
  (getX [_] x)
  (getY [_] y)
  (getZ [_] z)
  (getW [_] w)

  Counted
  (count [_] 4)

  Sequential

  Seqable
  (seq [_] (list x y z w))

  ILookup
  (valAt [v i]
    (.valAt v i nil))
  (valAt [_ i not-found]
    (case (int i) 0 x 1 y 2 z 3 w not-found))

  Associative
  (equiv [v o] (.equals v o))
  (containsKey [_ k]
    (case (int k)
      (0 1 2 3) true
      false))
  (entryAt [_ k]
    (case (int k)
      0 (MapEntry. 0 x)
      1 (MapEntry. 1 y)
      2 (MapEntry. 2 z)
      3 (MapEntry. 3 w)))
  (assoc [_ i v]
    (case (int i)
      0 (Vector4D. v y z w)
      1 (Vector4D. x v z w)
      2 (Vector4D. x y v w)
      3 (Vector4D. x y z v)))

  IFn
  (invoke [v i]
    (.valAt v i))

  Object
  (toString [_]
    (str "vec4d [" x " " y " " z " " w "]"))
  (hashCode [_]
    (-> 17 (add-hashcode x)
        (add-hashcode y)
        (add-hashcode z)
        (add-hashcode w)))
  (equals [self v]
    (or (identical? self v)
        (and (instance? Vector4D v)
             (== x (.getX ^Vector4D v))
             (== y (.getY ^Vector4D v))
             (== z (.getZ ^Vector4D v))
             (== w (.getW ^Vector4D v)))
        (and (counted? v)
             (== (count v) 3)
             (== x (v 0))
             (== y (v 1))
             (== z (v 2))
             (== w (v 3))))))

(alter-meta! #'->Vector2D assoc :no-doc true)
(alter-meta! #'->Vector3D assoc :no-doc true)
(alter-meta! #'->Vector4D assoc :no-doc true)

(defn- add-2d [^Vector2D v1 ^Vector2D v2]
  (Vector2D. (+ (.getX v1) (.getX v2))
             (+ (.getY v1) (.getY v2))))

(defn- sub-2d [^Vector2D v1 ^Vector2D v2]
  (Vector2D. (- (.getX v1) (.getX v2))
             (- (.getY v1) (.getY v2))))

(defn- mult-2d [^Vector2D v1 ^Vector2D v2]
  (Vector2D. (* (.getX v1) (.getX v2))
             (* (.getY v1) (.getY v2))))

(defn- div-2d [^Vector2D v1 ^Vector2D v2]
  (Vector2D. (/ (.getX v1) (.getX v2))
             (/ (.getY v1) (.getY v2))))

(defn- scale-2d [^Vector2D v ^double f]
  (Vector2D. (* (.getX v) f)
             (* (.getY v) f)))

(defn- dot-2d [^Vector2D v1 ^Vector2D v2]
  (+ (* (.getX v1) (.getX v2))
     (* (.getY v1) (.getY v2))))

(defn- magnitude-2d [^Vector2D v]
  (let [x (.getX v)
        y (.getY v)]
    (Math/sqrt (+ (* x x) (* y y)))))

(defn- add-3d [^Vector3D v1 ^Vector3D v2]
  (Vector3D. (+ (.getX v1) (.getX v2))
             (+ (.getY v1) (.getY v2))
             (+ (.getZ v1) (.getZ v2))))

(defn sub-3d [^Vector3D v1 ^Vector3D v2]
  (Vector3D. (- (.getX v1) (.getX v2))
             (- (.getY v1) (.getY v2))
             (- (.getZ v1) (.getZ v2))))

(defn- mult-3d [^Vector3D v1 ^Vector3D v2]
  (Vector3D. (* (.getX v1) (.getX v2))
             (* (.getY v1) (.getY v2))
             (* (.getZ v1) (.getZ v2))))

(defn- div-3d [^Vector3D v1 ^Vector3D v2]
  (Vector3D. (/ (.getX v1) (.getX v2))
             (/ (.getY v1) (.getY v2))
             (/ (.getZ v1) (.getZ v2))))

(defn scale-3d [^Vector3D v ^double f]
  (Vector3D. (* (.getX v) f)
             (* (.getY v) f)
             (* (.getZ v) f)))

(defn dot-3d [v1 v2]
  (+ (* (.getX v1) (.getX v2))
     (* (.getY v1) (.getY v2))
     (* (.getZ v1) (.getZ v2))))

(defn- magnitude-3d [^Vector3D v]
  (let [x (.getX v)
        y (.getY v)
        z (.getZ v)]
    (Math/sqrt (+ (* x x) (* y y) (* z z)))))

(defn- add-4d [^Vector4D v1 ^Vector4D v2]
  (Vector4D. (+ (.getX v1) (.getX v2))
             (+ (.getY v1) (.getY v2))
             (+ (.getZ v1) (.getZ v2))
             (+ (.getW v1) (.getW v2))))

(defn sub-4d [^Vector4D v1 ^Vector4D v2]
  (Vector4D. (- (.getX v1) (.getX v2))
             (- (.getY v1) (.getY v2))
             (- (.getZ v1) (.getZ v2))
             (- (.getW v1) (.getW v2))))

(defn- mult-4d [^Vector4D v1 ^Vector4D v2]
  (Vector4D. (* (.getX v1) (.getX v2))
             (* (.getY v1) (.getY v2))
             (* (.getZ v1) (.getZ v2))
             (* (.getW v1) (.getW v2))))

(defn- div-4d [^Vector4D v1 ^Vector4D v2]
  (Vector4D. (/ (.getX v1) (.getX v2))
             (/ (.getY v1) (.getY v2))
             (/ (.getZ v1) (.getZ v2))
             (/ (.getW v1) (.getW v2))))

(defn scale-4d [^Vector4D v ^double f]
  (Vector4D. (* (.getX v) f)
             (* (.getY v) f)
             (* (.getZ v) f)
             (* (.getW v) f)))

(defn dot-4d [^Vector4D v1 ^Vector4D v2]
  (+ (* (.getX v1) (.getX v2))
     (* (.getY v1) (.getY v2))
     (* (.getZ v1) (.getZ v2))
     (* (.getW v1) (.getW v2))))

(defn- magnitude-4d [^Vector4D v]
  (let [x (.getX v)
        y (.getY v)
        z (.getZ v)
        w (.getW v)]
    (Math/sqrt (+ (* x x) (* y y) (* z z) (* w w)))))

(defprotocol Vector
  (^:no-doc add* [v1 v2] "Add two vectors together.")
  (^:no-doc sub* [v1 v2] "Subtract the second vector from the first.")
  (^:no-doc mult* [v1 v2] "Multiply one vector by another.")
  (^:no-doc div* [v1 v2] "Divide one vector by another.")
  (scale [v f] "Scale a vector by a factor.")
  (dot [v1 v2] "Find the dot-product of two vectors.")
  (magnitude [v] "The magnitude (length) of the vector."))

(extend-protocol Vector
  Vector2D
  (add* [v1 v2] (add-2d v1 v2))
  (sub* [v1 v2] (sub-2d v1 v2))
  (mult* [v1 v2] (mult-2d v1 v2))
  (div* [v1 v2] (div-2d v1 v2))
  (scale [v f] (scale-2d v f))
  (dot [v1 v2] (dot-2d v1 v2))
  (magnitude [v] (magnitude-2d v))
  Vector3D
  (add* [v1 v2] (add-3d v1 v2))
  (sub* [v1 v2] (sub-3d v1 v2))
  (mult* [v1 v2] (mult-3d v1 v2))
  (div* [v1 v2] (div-3d v1 v2))
  (scale [v f] (scale-3d v f))
  (dot [v1 v2] (dot-3d v1 v2))
  (magnitude [v] (magnitude-3d v))
  Vector4D
  (add* [v1 v2] (add-4d v1 v2))
  (sub* [v1 v2] (sub-4d v1 v2))
  (mult* [v1 v2] (mult-4d v1 v2))
  (div* [v1 v2] (div-4d v1 v2))
  (scale [v f] (scale-4d v f))
  (dot [v1 v2] (dot-4d v1 v2))
  (magnitude [v] (magnitude-4d v)))

(defn add
  "Return the sum of one or more vectors."
  ([v] v)
  ([v1 v2] (add* v1 v2))
  ([v1 v2 & more] (reduce add* v1 (cons v2 more))))

(defn sub
  "If only one vector is supplied, return the negation of the vector. Otherwise
  all subsequent vectors are subtracted from the first."
  ([v] (scale v -1.0))
  ([v1 v2] (sub* v1 v2))
  ([v1 v2 & more] (reduce sub* v1 (cons v2 more))))

(defn mult
  "Return the product of each x, y and z value in one or more vectors."
  ([v] v)
  ([v1 v2] (mult* v1 v2))
  ([v1 v2 & more] (reduce mult* v1 (cons v2 more))))

(defn div
  "If only one vector is supplied, return 1/value for x, y and z. Otherwise
  the first vector is divided by all other vectors."
  ([v] (if (instance? Vector3D v)
         (div* (Vector3D. 1.0 1.0 1.0) v)
         (div* (Vector2D. 1.0 1.0) v)))
  ([v1 v2] (div* v1 v2))
  ([v1 v2 & more] (reduce div* v1 (cons v2 more))))

(defn normalize
  "Normalize a vector by dividing by its magnitude."
  [v]
  (scale v (/ 1.0 (magnitude v))))

(defn cross
  "Find the cross-product of two 3D vectors."
  [v1 v2]
  (Vector3D. (- (* (.getY v1) (.getZ v2))
                (* (.getZ v1) (.getY v2)))
             (- (* (.getZ v1) (.getX v2))
                (* (.getX v1) (.getZ v2)))
             (- (* (.getX v1) (.getY v2))
                (* (.getY v1) (.getX v2)))))

(defn get-x
  "Find the x coordinate of a vector."
  [v]
  (if (instance? Vector3D v)
    (.getX ^Vector3D v)
    (.getX ^Vector2D v)))

(defn get-y
  "Find the y coordinate of a vector."
  [v]
  (if (instance? Vector3D v)
    (.getY ^Vector3D v)
    (.getY ^Vector2D v)))

(defn get-z
  "Find the z coordinate of a vector."
  [v]
  (.getZ ^Vector3D v))

(defn vector
  "Create a new 2D or 3D math vector."
  ([^double x ^double y]
   (Vector2D. x y))
  ([^double x ^double y ^double z]
   (Vector3D. x y z))
  ([^double x ^double y ^double z ^double w]
   (Vector4D. x y z w)))

(defn into-vector [coll]
  "Turn a collection of numbers into a math vector."
  (if (satisfies? Vector coll)
    coll
    (apply vector coll)))

(defmethod print-method Vector2D [^Vector2D v ^Writer w]
  (.write w (.toString v)))

(defmethod print-method Vector3D [^Vector3D v ^Writer w]
  (.write w (.toString v)))

(defmethod print-method Vector4D [^Vector4D v ^Writer w]
  (.write w (.toString v)))

(defmethod print-dup Vector2D [^Vector2D v ^Writer w]
  (.write w (.toString v)))

(defmethod print-dup Vector3D [^Vector3D v ^Writer w]
  (.write w (.toString v)))

(defmethod print-dup Vector4D [^Vector4D v ^Writer w]
  (.write w (.toString v)))

(defn is-outside [v]
  (let [x (v 0)
        y (v 1)
        z (v 2)
        w (v 3)]
    (or (< x (- w)) (> x w) (< y (- w)) (> y w) (< z (- w)) (> z w))))

(defn reflect [d n]
  (m/sub d (m/scale (m/scale n 2) (m/dot d n))))
