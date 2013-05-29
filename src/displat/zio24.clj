(ns displat.zio24
  (:use [overtone.live])
  (:require displat.sequencer))

(definst x1
  [note 60 dur 1]
  (let [freq (midicps note)
        env  (env-gen (line:kr 1 0 dur FREE))
        src  (sin-osc [freq (* freq 1.01)])
        env2 (env-gen (perc 0.01 dur) :action FREE)]
    (* env env2 src)))

(defsynth fm [note 60 divisor 2.0 depth 1.0 out-bus 0]
  (let [carrier   (midicps note)
        modulator (/ carrier divisor)
        mod-env   (env-gen (lin-env 0.001 0 2))
        amp-env   (env-gen (lin-env 0.001 0 2) :action FREE)]
    (out out-bus (pan2 (* 0.5 amp-env
                          (sin-osc (+ carrier
                                      (* mod-env  (* carrier depth) (sin-osc modulator)))))))))
(fm 50 2 2)
(def x1-pattern {
                     :pitches   [8 5 8 0 8 3 3 0]
                     :times     [0 0.5 1 1.5 4 5 6 6]
                     :durations [1/2 1/4 1/4 1/1 1/4 1/4 1/1 1/1]
                 })

(defn player
  [m]
  (let [n-data x1-pattern
        notes (degrees->pitches (:pitches n-data) :minor :d3)
        beat  (m)
        length (count (:times n-data))]
    (dorun
     (map (fn [n offset dur]
            (at (m (+ beat offset))
                (x1 n (/ (* dur (metro-tick m)) 1000))))
          notes
          (:times n-data)
          (:durations n-data)))
    (apply-at (m (+ beat length)) #'player [m])))

;; FM Section
(def fm-pattern {
              :pitches [0 0 0 0]
              :times [0 4 8 12]
              :durations [1/1 1/2 1/1 1/2]})

(defn fm-player
  [m]
  (let [n-data fm-pattern
        notes (degrees->pitches (:pitches n-data) :minor :d3)
        beat  (m)
        bar   (+ beat 16)]
    (dorun
     (map (fn [n offset dur]
            (at (m (+ beat offset))
                (fm n (/ dur 1.3) 2)))
          notes
          (:times n-data)
          (:durations n-data)))
    (apply-at (m bar) #'fm-player [m])))


(def buf-size 16)
;; Sequencer adapted from Internal Sequencer example
(defonce buf-0 (buffer buf-size))
(defonce buf-1 (buffer buf-size))
(defonce buf-2 (buffer buf-size))
(defonce buf-3 (buffer buf-size))

(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

(def BEAT-FRACTION "Number of global pulses per beat" 30)

;; Here we design synths that will drive our pulse buses.
(defsynth root-trg [rate 100]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div))  )

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

;; Now we get a little close to the sounds. Here's four nice sounding
;; samples from Freesound.org
(def kick-s (sample (freesound-path 777)))
(def click-s (sample (freesound-path 406)))
(def boom-s (sample (freesound-path 33637)))
(def subby-s (sample (freesound-path 25649)))

;; Here's a synth for playing back the samples with a bit of modulation
;; to keep things interesting.
(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                      (= beat-num (mod cnt buf-size))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out
     out-bus (* vol
                amp
                (pan2
                 (rlpf
                  (scaled-play-buf 1 buf rate bar-trg)
                  (demand bar-trg 0 (dbrown 200 20000 50 INF))
                  (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

;; Now, let's start up all the synths:
(do
  (def r-cnt (root-cnt))
  (def b-cnt (beat-cnt))
  (def b-trg (beat-trg))
  (def r-trg (root-trg))

  (def kicks (doall
              (for [x (range buf-size)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0 :out-bus 10))))

  (def clicks (doall
               (for [x (range buf-size)]
                 (mono-sequencer :buf click-s :beat-num x :sequencer buf-1 :out-bus 11))))

  (def booms (doall
              (for [x (range buf-size)]
                (mono-sequencer :buf boom-s :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range buf-size)]
                  (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3)))))


;; An empty palatte to play with:
(do
  (buffer-write! buf-0 [1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 0 0 0 0 0 1 1 1 0 0 1 0])  ;; click
  (buffer-write! buf-2 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])  ;; boom
  (buffer-write! buf-3 [1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0])) ;; subby
;; Playback
(def metro (metronome 100))

(defsynth chan1 []
  (let [inb (in:ar 11)]
    (out 0 (pan2 inb 0.75))))

(chan1)
(fm-player metro)
(player metro)

(stop)

(node-tree)
