(ns computer_graphics_coursework_backend.world.perlin
  (:require [fastmath.random :as r]))

(def perlin-mask 255)

(defn fade
  [^double t]
  (* t t t (+ (* t (- (* t 6) 15)) 10)))

(defn lerp
  [^double t ^double a ^double b] (+ a (* t (- b a))))

(defn grad
  [^long hash ^double x ^double y ^double z]
  (let [h (bit-and hash 15)
        u (if (< h 8) x y)
        v (if (< h 4) y
                      (if (or (= h 12)
                              (= h 14)) x z))]
    (+ (if (= (bit-and h 1) 0) u (- u))
       (if (= (bit-and h 2) 0) v (- v)))))

(def ^:const permutation (vector 151, 160, 137, 91, 90, 15,
                                 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
                                 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
                                 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166,
                                 77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244,
                                 102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196,
                                 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123,
                                 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
                                 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
                                 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
                                 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107,
                                 49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
                                 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180))

(def ^:const p (vec (concat permutation permutation)))

(def p-array (into-array Integer/TYPE p))

(defn standard-noise
  [^double x ^double y ^double z]
  (let [x-floor (Math/floor x)
        y-floor (Math/floor y)
        z-floor (Math/floor z)
        X (bit-and (int x-floor) perlin-mask)
        Y (bit-and (int y-floor) perlin-mask)
        Z (bit-and (int z-floor) perlin-mask)
        x-relative (- x x-floor)
        y-relative (- y y-floor)
        z-relative (- z z-floor)
        u (fade x-relative)
        v (fade y-relative)
        w (fade z-relative)
        A (+ (aget p-array X) Y)
        AA (+ (aget p-array A) Z)
        AB (+ (aget p-array (inc A)) Z)
        B (+ (aget p-array (inc X)) Y)
        BA (+ (aget p-array B) Z)
        BB (+ (aget p-array (inc B)) Z)]
    (lerp w
          (lerp v
                (lerp u
                      (grad (aget p-array AA) x-relative y-relative z-relative)
                      (grad (aget p-array BA) (dec x-relative) y-relative z-relative))
                (lerp u
                      (grad (aget p-array AB) x-relative (dec y-relative) z-relative)
                      (grad (aget p-array BB) (dec x-relative) (dec y-relative) z-relative)))
          (lerp v
                (lerp u
                      (grad (aget p-array (inc AA)) x-relative y-relative (dec z-relative))
                      (grad (aget p-array (inc BA)) (dec x-relative) y-relative (dec z-relative)))
                (lerp u
                      (grad (aget p-array (inc AB)) x-relative (dec y-relative) (dec z-relative))
                      (grad (aget p-array (inc BB)) (dec x-relative) (dec y-relative) (dec z-relative)))))))

(defn noise
  [^double x ^double y ^double z]
  (r/vnoise x y z))