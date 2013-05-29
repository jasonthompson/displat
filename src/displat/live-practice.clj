(ns displat.live-practice
  (:use [overtone.live]
        [overtone.inst.drum]))


(definst x1
  [note 60 dur 1]
  (let [freq (* (midicps note) (lf-noise1 10))
        env  (env-gen (line:kr 1 0 dur FREE))
        src  (sin-osc [freq (* freq 1.01)])
        env2 (env-gen (perc 0.001 dur) :action FREE)]
    (* env (* 0.75 src) env2)))

(x1)
(defsynth fm [note 60 divisor 2.0 depth 1.0 out-bus 0]
  (let [carrier   (midicps note)
        modulator (/ carrier divisor)
        mod-env   (env-gen (lin-env 0.001 0 2))
        amp-env   (env-gen (lin-env 0.001 0 2) :action FREE)]
    (out out-bus (pan2 (* 0.5 amp-env
                          (sin-osc (+ carrier
                                      (* mod-env  (* carrier depth) (sin-osc modulator)))))))))
(fm 50 2 2)

(definst wobble
  [freq 100 rate 2]
  (let [src0    (sin-osc freq)
        src1    (sin-osc (* freq 2))
        src2    (sin-osc (* freq 3))
        src3    (sin-osc (* freq 5))
        amp-mod (sin-osc:kr rate)
        sub     (sin-osc (/ freq 2))]
    (* amp-mod  (/ (+ sub src0 (* src1 0.5) (* src2 0.33) (* src3 0.2)) 2))))

(wobble 40 4)
(ctl wobble :freq 30)
(stop)

(def x1-pattern {
                     :pitches   [1 4 1 4 1 4 1 4]
                     :times     [0 0.5 1 1.5 4 5.25 5.75 6]
                     :durations [1/2 1/4 1/4 1 1/4 1/2 1/4 1/4]
                 })

(defn x1-player
  [m]
  (let [n-data x1-pattern
        notes (degrees->pitches (:pitches n-data) :minor :e5)
        beat  (m)
        length (count (:times n-data))]
    (if (on? :x1-player)
      (dorun
       (map (fn [n offset dur]
              (at (m (+ beat offset))
                  (x1 n (/ (* dur (metro-tick m)) 1000))))
            notes
            (:times n-data)
            (:durations n-data))))
    (apply-at (m (+ beat length)) #'x1-player [m])))
(on? x1-player)
;; FM Section
(def fm-pattern {
              :pitches [0 0 0 0]
              :times [0 4 8 12]
              :durations [1/2 1/2 1/1 1/2]})

(defn fm-player
  [m]

  (let [n-data fm-pattern
        notes (degrees->pitches (:pitches n-data) :minor :e2)
        beat  (m)
        bar   (+ beat 16)]
    (if (on? :fm-player)
      (dorun
       (map (fn [n offset dur]
              (at (m (+ beat offset))
                  (fm n (/ dur 1.3) 2)))
            notes
            (:times n-data)
            (:durations n-data))))
    (apply-at (m bar) #'fm-player [m])))

(stop)
(def metro (metronome 100))

;; Make this into an atom?
(def players {:fm-player :off
              :x1-player :off})

(defn on?
  [player]
  (= (player players) :on))

(stop)
(x1-player metro)
(fm-player metro)

(def x1-verb (inst-fx! x1 fx-freeverb))
(def x1-echo (inst-fx! x1 fx-echo))

(ctl x1-echo :max-echo 0.25 :delay-time 0.25 :decay-time 0.1)
(ctl x1-verb :wet-dry 0.3)
(clear-fx x1)
